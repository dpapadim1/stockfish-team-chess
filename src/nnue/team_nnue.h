/*
  Team Chess NNUE — Simplified neural network evaluation
  
  Architecture: SeatPieceSquare features → small network
  
  Feature set: 4 seats × 6 piece types × 128 squares = 3,072 sparse binary features
  Each piece on the board activates exactly ONE feature based on:
    feature_index = seat * 768 + (piece_type - 1) * 128 + square
  
  Network:  3,072 → 64 (ClippedReLU) → 32 (ClippedReLU) → 1
  
  Weights stored as int16 in Q6.10 fixed-point (divide by 64 for centipawns).
  Score is from side-to-move perspective.
  
  Binary file format:
    Magic:   "TCNN" (4 bytes)
    Version: uint32 (1)
    InputDim:  uint32 (3072)
    L1Dim:     uint32 (64)
    L2Dim:     uint32 (32)
    FT weights:  int16[3072 × 64]  (row-major: weight[input][hidden])
    FT biases:   int16[64]
    L1 weights:  int16[64 × 32]
    L1 biases:   int16[32]
    L2 weights:  int16[32]
    L2 bias:     int16[1]
*/

#ifndef TEAM_NNUE_H_INCLUDED
#define TEAM_NNUE_H_INCLUDED

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "../position.h"
#include "../types.h"

namespace Stockfish {
namespace TeamNNUE {

// ============================================================================
// Architecture constants
// ============================================================================
constexpr int INPUT_DIM  = 3072;  // 4 seats × 6 piece types × 128 squares
constexpr int L1_DIM     = 64;    // First hidden layer
constexpr int L2_DIM     = 32;    // Second hidden layer
constexpr int OUTPUT_DIM = 1;

// Weight scale: int16 values are divided by this to get centipawns
constexpr int WEIGHT_SCALE = 64;

// ClippedReLU bounds (in scaled units)
constexpr int16_t CRELU_MIN = 0;
constexpr int16_t CRELU_MAX = 127 * WEIGHT_SCALE;  // Max activation

// Feature index computation
// feature = seat * 768 + (pieceType - 1) * 128 + square
// where pieceType is 1-6 (PAWN..KING), seat is 0-3
constexpr int FEATURES_PER_SEAT = 6 * SQUARE_NB;  // 6 * 128 = 768

inline int feature_index(Seat seat, PieceType pt, Square sq) {
    return int(seat) * FEATURES_PER_SEAT + (int(pt) - 1) * SQUARE_NB + int(sq);
}

// ============================================================================
// Network weights
// ============================================================================
struct alignas(64) NetworkWeights {
    // Feature transform: INPUT_DIM → L1_DIM
    int16_t ft_weights[INPUT_DIM][L1_DIM];
    int16_t ft_biases[L1_DIM];

    // Hidden layer 1: L1_DIM → L2_DIM
    int16_t l1_weights[L1_DIM][L2_DIM];
    int16_t l1_biases[L2_DIM];

    // Output layer: L2_DIM → 1
    int16_t l2_weights[L2_DIM];
    int16_t l2_bias;
};

// ============================================================================
// Global NNUE state
// ============================================================================
class TeamNNUEState {
public:
    TeamNNUEState() : loaded_(false), weights_(nullptr) {}

    ~TeamNNUEState() {
        delete weights_;
    }

    // Load weights from binary file. Returns true on success.
    bool load(const std::string& path) {
        std::ifstream in(path, std::ios::binary);
        if (!in) {
            std::cerr << "[TeamNNUE] Cannot open file: " << path << std::endl;
            return false;
        }

        // Read and verify magic
        char magic[4];
        in.read(magic, 4);
        if (std::memcmp(magic, "TCNN", 4) != 0) {
            std::cerr << "[TeamNNUE] Invalid magic number" << std::endl;
            return false;
        }

        // Read and verify version
        uint32_t version;
        in.read(reinterpret_cast<char*>(&version), 4);
        if (version != 1) {
            std::cerr << "[TeamNNUE] Unsupported version: " << version << std::endl;
            return false;
        }

        // Read and verify dimensions
        uint32_t dims[3];
        in.read(reinterpret_cast<char*>(dims), 12);
        if (dims[0] != INPUT_DIM || dims[1] != L1_DIM || dims[2] != L2_DIM) {
            std::cerr << "[TeamNNUE] Dimension mismatch: expected "
                      << INPUT_DIM << "/" << L1_DIM << "/" << L2_DIM
                      << " got " << dims[0] << "/" << dims[1] << "/" << dims[2]
                      << std::endl;
            return false;
        }

        // Allocate weights
        auto* w = new NetworkWeights();

        // Read feature transform weights: [INPUT_DIM][L1_DIM]
        in.read(reinterpret_cast<char*>(w->ft_weights), sizeof(w->ft_weights));

        // Read feature transform biases: [L1_DIM]
        in.read(reinterpret_cast<char*>(w->ft_biases), sizeof(w->ft_biases));

        // Read L1 weights: [L1_DIM][L2_DIM]
        in.read(reinterpret_cast<char*>(w->l1_weights), sizeof(w->l1_weights));

        // Read L1 biases: [L2_DIM]
        in.read(reinterpret_cast<char*>(w->l1_biases), sizeof(w->l1_biases));

        // Read L2 weights: [L2_DIM]
        in.read(reinterpret_cast<char*>(w->l2_weights), sizeof(w->l2_weights));

        // Read L2 bias: [1]
        in.read(reinterpret_cast<char*>(&w->l2_bias), sizeof(w->l2_bias));

        if (!in) {
            std::cerr << "[TeamNNUE] Failed to read all weights" << std::endl;
            delete w;
            return false;
        }

        // Swap in new weights
        delete weights_;
        weights_ = w;
        loaded_ = true;

        std::cerr << "[TeamNNUE] Loaded network from " << path << std::endl;
        std::cerr << "[TeamNNUE] Architecture: " << INPUT_DIM << " -> "
                  << L1_DIM << " -> " << L2_DIM << " -> 1" << std::endl;

        return true;
    }

    bool is_loaded() const { return loaded_; }

    // Evaluate a position using the NNUE.
    // Returns score in centipawns from side-to-move perspective.
    Value evaluate(const Position& pos) const {
        if (!loaded_ || !weights_)
            return VALUE_ZERO;

        // --- Step 1: Collect active features ---
        int active[64];  // Team chess has up to 64 pieces (16×4 occupied ranks)
        int numActive = 0;

        for (Bitboard b = pos.pieces(); bool(b);) {
            Square sq = pop_lsb(b);
            Piece  pc = pos.piece_on(sq);
            if (pc == NO_PIECE) continue;

            Seat      seat = pos.seat_of(sq);
            PieceType pt   = type_of(pc);

            if (seat < SEAT_NB && pt >= PAWN && pt <= KING) {
                int idx = feature_index(seat, pt, sq);
                if (idx >= 0 && idx < INPUT_DIM && numActive < 64) {
                    active[numActive++] = idx;
                }
            }
        }

        // --- Step 2: Feature transform (accumulator) ---
        // Start with biases, then add weight rows for each active feature
        int32_t l1_input[L1_DIM];
        for (int i = 0; i < L1_DIM; i++)
            l1_input[i] = int32_t(weights_->ft_biases[i]);

        for (int a = 0; a < numActive; a++) {
            const int16_t* row = weights_->ft_weights[active[a]];
            for (int i = 0; i < L1_DIM; i++)
                l1_input[i] += int32_t(row[i]);
        }

        // ClippedReLU on L1 input
        int16_t l1_act[L1_DIM];
        for (int i = 0; i < L1_DIM; i++) {
            int32_t v = l1_input[i];
            l1_act[i] = int16_t(std::clamp(v, int32_t(CRELU_MIN), int32_t(CRELU_MAX)));
        }

        // --- Step 3: Hidden layer 1 (L1_DIM → L2_DIM) ---
        // Use int64_t accumulator: 64 products of int16(8128) × int16(32767)
        // can reach ~17G, exceeding int32 range.
        int32_t l2_input[L2_DIM];
        for (int j = 0; j < L2_DIM; j++) {
            int64_t sum = int64_t(weights_->l1_biases[j]) * WEIGHT_SCALE;
            for (int i = 0; i < L1_DIM; i++)
                sum += int64_t(l1_act[i]) * int64_t(weights_->l1_weights[i][j]);
            l2_input[j] = int32_t(sum / WEIGHT_SCALE);
        }

        // ClippedReLU on L2 input
        int16_t l2_act[L2_DIM];
        for (int j = 0; j < L2_DIM; j++) {
            l2_act[j] = int16_t(std::clamp(l2_input[j], int32_t(CRELU_MIN), int32_t(CRELU_MAX)));
        }

        // --- Step 4: Output layer (L2_DIM → 1) ---
        // Use int64_t: 32 products of int16(8128) × int16(32767) can reach ~8.5G.
        int64_t output = int64_t(weights_->l2_bias) * WEIGHT_SCALE;
        for (int j = 0; j < L2_DIM; j++)
            output += int64_t(l2_act[j]) * int64_t(weights_->l2_weights[j]);

        // Convert from Q6.10 fixed-point to centipawns
        int score = int(output / (WEIGHT_SCALE * WEIGHT_SCALE));

        // Score is from white's perspective; adjust for side to move
        if (pos.side_to_move() == BLACK)
            score = -score;

        // Clamp to reasonable range
        score = std::clamp(score, -20000, 20000);

        return Value(score);
    }

private:
    bool            loaded_;
    NetworkWeights* weights_;
};

// Global instance
inline TeamNNUEState& global_nnue() {
    static TeamNNUEState instance;
    return instance;
}

}  // namespace TeamNNUE
}  // namespace Stockfish

#endif  // TEAM_NNUE_H_INCLUDED
