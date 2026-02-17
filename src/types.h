/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2026 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

// =============================================================================
// TEAM CHESS VARIANT — 16×8 board, 4 armies (w1/b1/w2/b2), 2v2 teams.
//
// Key differences from standard Stockfish:
//   • 128 squares (16 files × 8 ranks) — Square is 0..127, File is 0..15
//   • NORTH = 16 (row stride), not 8
//   • Bitboard is a 128-bit struct (two uint64_t)
//   • Move is 32-bit (7-bit squares instead of 6-bit)
//   • 4 seats (w1, b1, w2, b2) with separate castling rights (8 bits)
//   • NNUE is disabled — classical evaluation only
// =============================================================================

#ifndef TYPES_H_INCLUDED
    #define TYPES_H_INCLUDED

    #include <cassert>
    #include <cstddef>
    #include <cstdint>
    #include <type_traits>
    #include "misc.h"

    #if defined(_MSC_VER)
        #pragma warning(disable: 4127)
        #pragma warning(disable: 4146)
        #pragma warning(disable: 4800)
    #endif

    #if defined(__GNUC__) && !defined(__clang__) \
      && (__GNUC__ < 9 || (__GNUC__ == 9 && __GNUC_MINOR__ < 3))
        #error "Stockfish requires GCC 9.3 or later for correct compilation"
    #endif

    #if defined(__clang__) && (__clang_major__ < 10)
        #error "Stockfish requires Clang 10.0 or later for correct compilation"
    #endif

    #define ASSERT_ALIGNED(ptr, alignment) assert(reinterpret_cast<uintptr_t>(ptr) % alignment == 0)

    #if defined(_WIN64) && defined(_MSC_VER)
        #include <intrin.h>
        #define IS_64BIT
    #endif

    #if defined(USE_POPCNT) && defined(_MSC_VER)
        #include <nmmintrin.h>
    #endif

    #if !defined(NO_PREFETCH) && defined(_MSC_VER)
        #include <xmmintrin.h>
    #endif

    // PEXT disabled for team chess (128-bit bitboards, magic tables recomputed)
    #undef USE_PEXT
    #define pext(b, m) 0

namespace Stockfish {

    #ifdef USE_POPCNT
constexpr bool HasPopCnt = true;
    #else
constexpr bool HasPopCnt = false;
    #endif

constexpr bool HasPext = false;  // Always disabled for team chess

    #ifdef IS_64BIT
constexpr bool Is64Bit = true;
    #else
constexpr bool Is64Bit = false;
    #endif

using Key = uint64_t;

// =============================================================================
// BITBOARD — 128-bit type for the 16×8 board
// =============================================================================
// lo covers squares 0-63  (rank 1-4, all 16 files)
// hi covers squares 64-127 (rank 5-8, all 16 files)

struct Bitboard {
    uint64_t lo;  // squares  0..63
    uint64_t hi;  // squares 64..127

    constexpr Bitboard() : lo(0), hi(0) {}
    constexpr Bitboard(uint64_t l, uint64_t h) : lo(l), hi(h) {}

    // Boolean conversion — true if any bit is set
    constexpr explicit operator bool() const { return lo || hi; }

    // Comparison
    constexpr bool operator==(const Bitboard& b) const { return lo == b.lo && hi == b.hi; }
    constexpr bool operator!=(const Bitboard& b) const { return lo != b.lo || hi != b.hi; }

    // Bitwise operators
    constexpr Bitboard operator&(const Bitboard& b) const { return {lo & b.lo, hi & b.hi}; }
    constexpr Bitboard operator|(const Bitboard& b) const { return {lo | b.lo, hi | b.hi}; }
    constexpr Bitboard operator^(const Bitboard& b) const { return {lo ^ b.lo, hi ^ b.hi}; }
    constexpr Bitboard operator~() const { return {~lo, ~hi}; }
    constexpr Bitboard operator-() const {
        // Two's complement negation for 128-bit
        uint64_t new_lo = ~lo + 1;
        uint64_t carry  = (new_lo == 0) ? 1ULL : 0ULL;
        return {new_lo, ~hi + carry};
    }

    constexpr Bitboard& operator&=(const Bitboard& b) { lo &= b.lo; hi &= b.hi; return *this; }
    constexpr Bitboard& operator|=(const Bitboard& b) { lo |= b.lo; hi |= b.hi; return *this; }
    constexpr Bitboard& operator^=(const Bitboard& b) { lo ^= b.lo; hi ^= b.hi; return *this; }

    // Subtraction (for Carry-Rippler trick: b = (b - mask) & mask)
    constexpr Bitboard operator-(const Bitboard& b) const {
        uint64_t new_lo = lo - b.lo;
        uint64_t borrow = (lo < b.lo) ? 1ULL : 0ULL;
        return {new_lo, hi - b.hi - borrow};
    }

    // Shift left/right
    constexpr Bitboard operator<<(int n) const {
        if (n >= 128) return {0, 0};
        if (n >= 64)  return {0, lo << (n - 64)};
        if (n == 0)   return *this;
        return {lo << n, (hi << n) | (lo >> (64 - n))};
    }
    constexpr Bitboard operator>>(int n) const {
        if (n >= 128) return {0, 0};
        if (n >= 64)  return {hi >> (n - 64), 0};
        if (n == 0)   return *this;
        return {(lo >> n) | (hi << (64 - n)), hi >> n};
    }

    // Test if more than one bit is set
    constexpr bool more_than_one() const {
        if (lo && hi) return true;
        if (lo) return (lo & (lo - 1)) != 0;
        return (hi & (hi - 1)) != 0;
    }

    // Test if a specific bit (square index 0-127) is set
    constexpr bool test(int sq) const {
        if (sq < 64) return (lo >> sq) & 1;
        return (hi >> (sq - 64)) & 1;
    }

    // Set a specific bit
    constexpr Bitboard& set(int sq) {
        if (sq < 64) lo |= (1ULL << sq);
        else         hi |= (1ULL << (sq - 64));
        return *this;
    }

    // Clear a specific bit
    constexpr Bitboard& clear(int sq) {
        if (sq < 64) lo &= ~(1ULL << sq);
        else         hi &= ~(1ULL << (sq - 64));
        return *this;
    }

    // Check if empty
    constexpr bool empty() const { return lo == 0 && hi == 0; }
};

// Helper: a Bitboard with exactly one bit set at square index sq
constexpr Bitboard bb_from_sq(int sq) {
    if (sq < 64)
        return Bitboard{1ULL << sq, 0};
    else
        return Bitboard{0, 1ULL << (sq - 64)};
}

// Empty bitboard constant
constexpr Bitboard BB_ZERO = {0, 0};
// All bits set
constexpr Bitboard BB_ALL  = {~0ULL, ~0ULL};

constexpr int MAX_MOVES = 512;  // More moves possible on wider board with more pieces
constexpr int MAX_PLY   = 246;

// =============================================================================
// COLOR & SEAT — Team chess has 2 colors but 4 seats
// =============================================================================

enum Color : uint8_t {
    WHITE,
    BLACK,
    COLOR_NB = 2
};

// Seat identifies which of the 4 armies a player controls
// Turn order: W1 -> B1 -> W2 -> B2 -> ...
enum Seat : uint8_t {
    SEAT_W1,
    SEAT_B1,
    SEAT_W2,
    SEAT_B2,
    SEAT_NB = 4,
    SEAT_NONE = 4
};

constexpr Color seat_color(Seat s) { return Color(s & 1); }  // W1->WHITE, B1->BLACK, W2->WHITE, B2->BLACK
constexpr Seat  next_seat(Seat s)  { return Seat((s + 1) % SEAT_NB); }
constexpr Seat  prev_seat(Seat s)  { return Seat((s + SEAT_NB - 1) % SEAT_NB); }
constexpr Seat  teammate(Seat s)   { return Seat(s ^ 2); }  // W1<->W2, B1<->B2
constexpr Seat  make_seat(Color c, int army) { return Seat(c + army * 2); } // army 0 or 1

// =============================================================================
// CASTLING RIGHTS — 8 castling rights for 4 seats × 2 sides
// =============================================================================

enum CastlingRights : uint16_t {
    NO_CASTLING = 0,
    W1_OO  = 1 << 0,
    W1_OOO = 1 << 1,
    B1_OO  = 1 << 2,
    B1_OOO = 1 << 3,
    W2_OO  = 1 << 4,
    W2_OOO = 1 << 5,
    B2_OO  = 1 << 6,
    B2_OOO = 1 << 7,

    W1_CASTLING = W1_OO | W1_OOO,
    B1_CASTLING = B1_OO | B1_OOO,
    W2_CASTLING = W2_OO | W2_OOO,
    B2_CASTLING = B2_OO | B2_OOO,

    WHITE_CASTLING = W1_CASTLING | W2_CASTLING,
    BLACK_CASTLING = B1_CASTLING | B2_CASTLING,

    KING_SIDE  = W1_OO  | B1_OO  | W2_OO  | B2_OO,
    QUEEN_SIDE = W1_OOO | B1_OOO | W2_OOO | B2_OOO,

    ANY_CASTLING = WHITE_CASTLING | BLACK_CASTLING,

    CASTLING_RIGHT_NB = 256  // 8 bits → 256 combinations
};

// Get castling rights for a specific seat
constexpr CastlingRights seat_castling(Seat s) {
    constexpr CastlingRights map[SEAT_NB] = { W1_CASTLING, B1_CASTLING, W2_CASTLING, B2_CASTLING };
    return map[s];
}

constexpr CastlingRights seat_oo(Seat s) {
    constexpr CastlingRights map[SEAT_NB] = { W1_OO, B1_OO, W2_OO, B2_OO };
    return map[s];
}

constexpr CastlingRights seat_ooo(Seat s) {
    constexpr CastlingRights map[SEAT_NB] = { W1_OOO, B1_OOO, W2_OOO, B2_OOO };
    return map[s];
}

// Bitwise operators for CastlingRights
constexpr CastlingRights operator|(CastlingRights a, CastlingRights b) { return CastlingRights(uint16_t(a) | uint16_t(b)); }
constexpr CastlingRights operator&(CastlingRights a, CastlingRights b) { return CastlingRights(uint16_t(a) & uint16_t(b)); }
constexpr CastlingRights operator~(CastlingRights cr) { return CastlingRights(~uint16_t(cr)); }
constexpr CastlingRights& operator|=(CastlingRights& a, CastlingRights b) { return a = a | b; }
constexpr CastlingRights& operator&=(CastlingRights& a, CastlingRights b) { return a = a & b; }

enum Bound : uint8_t {
    BOUND_NONE,
    BOUND_UPPER,
    BOUND_LOWER,
    BOUND_EXACT = BOUND_UPPER | BOUND_LOWER
};

using Value = int;

constexpr Value VALUE_ZERO     = 0;
constexpr Value VALUE_DRAW     = 0;
constexpr Value VALUE_NONE     = 32002;
constexpr Value VALUE_INFINITE = 32001;

constexpr Value VALUE_MATE             = 32000;
constexpr Value VALUE_MATE_IN_MAX_PLY  = VALUE_MATE - MAX_PLY;
constexpr Value VALUE_MATED_IN_MAX_PLY = -VALUE_MATE_IN_MAX_PLY;

constexpr Value VALUE_TB                 = VALUE_MATE_IN_MAX_PLY - 1;
constexpr Value VALUE_TB_WIN_IN_MAX_PLY  = VALUE_TB - MAX_PLY;
constexpr Value VALUE_TB_LOSS_IN_MAX_PLY = -VALUE_TB_WIN_IN_MAX_PLY;

constexpr bool is_valid(Value value) { return value != VALUE_NONE; }
constexpr bool is_win(Value value)   { assert(is_valid(value)); return value >= VALUE_TB_WIN_IN_MAX_PLY; }
constexpr bool is_loss(Value value)  { assert(is_valid(value)); return value <= VALUE_TB_LOSS_IN_MAX_PLY; }
constexpr bool is_decisive(Value value) { return is_win(value) || is_loss(value); }

constexpr Value PawnValue   = 208;
constexpr Value KnightValue = 781;
constexpr Value BishopValue = 825;
constexpr Value RookValue   = 1276;
constexpr Value QueenValue  = 2538;

// clang-format off
enum PieceType : std::uint8_t {
    NO_PIECE_TYPE, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING,
    ALL_PIECES = 0,
    PIECE_TYPE_NB = 8
};

enum Piece : std::uint8_t {
    NO_PIECE,
    W_PAWN = PAWN,     W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
    B_PAWN = PAWN + 8, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING,
    PIECE_NB = 16
};
// clang-format on

constexpr Value PieceValue[PIECE_NB] = {
  VALUE_ZERO, PawnValue, KnightValue, BishopValue, RookValue, QueenValue, VALUE_ZERO, VALUE_ZERO,
  VALUE_ZERO, PawnValue, KnightValue, BishopValue, RookValue, QueenValue, VALUE_ZERO, VALUE_ZERO};

using Depth = int;

constexpr Depth DEPTH_QS           = 0;
constexpr Depth DEPTH_UNSEARCHED   = -2;
constexpr Depth DEPTH_ENTRY_OFFSET = -3;

// =============================================================================
// SQUARE — 128 squares for 16×8 board
// =============================================================================
// Layout: Square = rank * 16 + file
// Rank 1 = bottom (squares 0-15), Rank 8 = top (squares 112-127)
// Files a-p = columns 0-15

// clang-format off
enum Square : uint8_t {
    SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
    SQ_I1, SQ_J1, SQ_K1, SQ_L1, SQ_M1, SQ_N1, SQ_O1, SQ_P1,
    SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
    SQ_I2, SQ_J2, SQ_K2, SQ_L2, SQ_M2, SQ_N2, SQ_O2, SQ_P2,
    SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
    SQ_I3, SQ_J3, SQ_K3, SQ_L3, SQ_M3, SQ_N3, SQ_O3, SQ_P3,
    SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
    SQ_I4, SQ_J4, SQ_K4, SQ_L4, SQ_M4, SQ_N4, SQ_O4, SQ_P4,
    SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
    SQ_I5, SQ_J5, SQ_K5, SQ_L5, SQ_M5, SQ_N5, SQ_O5, SQ_P5,
    SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
    SQ_I6, SQ_J6, SQ_K6, SQ_L6, SQ_M6, SQ_N6, SQ_O6, SQ_P6,
    SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
    SQ_I7, SQ_J7, SQ_K7, SQ_L7, SQ_M7, SQ_N7, SQ_O7, SQ_P7,
    SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
    SQ_I8, SQ_J8, SQ_K8, SQ_L8, SQ_M8, SQ_N8, SQ_O8, SQ_P8,
    SQ_NONE = 128,

    SQUARE_ZERO = 0,
    SQUARE_NB   = 128
};
// clang-format on

// DIRECTION — stride is 16 for NORTH (16 files per rank)
enum Direction : int16_t {
    NORTH = 16,
    EAST  = 1,
    SOUTH = -16,
    WEST  = -1,

    NORTH_EAST = NORTH + EAST,   // 17
    SOUTH_EAST = SOUTH + EAST,   // -15
    SOUTH_WEST = SOUTH + WEST,   // -17
    NORTH_WEST = NORTH + WEST    // 15
};

// =============================================================================
// FILE — 16 files (a through p)
// =============================================================================

enum File : uint8_t {
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H,
    FILE_I, FILE_J, FILE_K, FILE_L, FILE_M, FILE_N, FILE_O, FILE_P,
    FILE_NB = 16
};

enum Rank : uint8_t {
    RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8,
    RANK_NB = 8
};

// Keep track of what a move changes on the board (used by NNUE)
// NNUE is disabled for team chess, but we keep the structs for compilation.
struct DirtyPiece {
    Piece  pc;
    Square from, to;
    Square remove_sq, add_sq;
    Piece  remove_pc, add_pc;
};

struct DirtyThreat {
    static constexpr int PcSqOffset         = 0;
    static constexpr int ThreatenedSqOffset = 8;
    static constexpr int ThreatenedPcOffset = 16;
    static constexpr int PcOffset           = 20;

    DirtyThreat() { /* don't initialize data */ }
    DirtyThreat(uint32_t raw) : data(raw) {}
    DirtyThreat(Piece pc, Piece threatened_pc, Square pc_sq, Square threatened_sq, bool add) {
        data = (uint32_t(add) << 31) | (pc << PcOffset) | (threatened_pc << ThreatenedPcOffset)
             | (threatened_sq << ThreatenedSqOffset) | (pc_sq << PcSqOffset);
    }

    Piece    pc()             const { return static_cast<Piece>(data >> PcOffset & 0xf); }
    Piece    threatened_pc()  const { return static_cast<Piece>(data >> ThreatenedPcOffset & 0xf); }
    Square   threatened_sq()  const { return static_cast<Square>(data >> ThreatenedSqOffset & 0xff); }
    Square   pc_sq()          const { return static_cast<Square>(data >> PcSqOffset & 0xff); }
    bool     add()            const { return data >> 31; }
    uint32_t raw()            const { return data; }

   private:
    uint32_t data;
};

using DirtyThreatList = ValueList<DirtyThreat, 128>;  // Increased for wider board

struct DirtyThreats {
    DirtyThreatList list;
    Color           us;
    Square          prevKsq, ksq;

    Bitboard threatenedSqs, threateningSqs;
};

    #define ENABLE_INCR_OPERATORS_ON(T) \
        constexpr T& operator++(T& d) { return d = T(int(d) + 1); } \
        constexpr T& operator--(T& d) { return d = T(int(d) - 1); }

ENABLE_INCR_OPERATORS_ON(PieceType)
ENABLE_INCR_OPERATORS_ON(Square)
ENABLE_INCR_OPERATORS_ON(File)
ENABLE_INCR_OPERATORS_ON(Rank)
ENABLE_INCR_OPERATORS_ON(Seat)

    #undef ENABLE_INCR_OPERATORS_ON

constexpr Direction operator+(Direction d1, Direction d2) { return Direction(int(d1) + int(d2)); }
constexpr Direction operator*(int i, Direction d) { return Direction(i * int(d)); }

constexpr Square  operator+(Square s, Direction d) { return Square(int(s) + int(d)); }
constexpr Square  operator-(Square s, Direction d) { return Square(int(s) - int(d)); }
constexpr Square& operator+=(Square& s, Direction d) { return s = s + d; }
constexpr Square& operator-=(Square& s, Direction d) { return s = s - d; }

// Toggle color
constexpr Color operator~(Color c) { return Color(c ^ BLACK); }

// Swap A1 <-> A8 (flip vertically). SQ_A8 = 112. s ^ 112 flips rank.
constexpr Square flip_rank(Square s) { return Square(s ^ SQ_A8); }

// Swap file: A <-> P (mirror horizontally, 16 files => XOR with 15)
constexpr Square flip_file(Square s) { return Square(s ^ SQ_P1); }

// Swap color of piece
constexpr Piece operator~(Piece pc) { return Piece(pc ^ 8); }

// Castling rights for a color (both armies combined)
constexpr CastlingRights operator&(Color c, CastlingRights cr) {
    return CastlingRights((c == WHITE ? uint16_t(WHITE_CASTLING) : uint16_t(BLACK_CASTLING)) & uint16_t(cr));
}

// Castling rights for a seat
constexpr CastlingRights operator&(Seat s, CastlingRights cr) {
    return CastlingRights(uint16_t(seat_castling(s)) & uint16_t(cr));
}

constexpr Value mate_in(int ply)  { return VALUE_MATE - ply; }
constexpr Value mated_in(int ply) { return -VALUE_MATE + ply; }

// =============================================================================
// COORDINATE HELPERS — adapted for 16-file board
// =============================================================================

// Square = rank * 16 + file
constexpr Square make_square(File f, Rank r) { return Square((r << 4) + f); }

constexpr Piece make_piece(Color c, PieceType pt) { return Piece((c << 3) + pt); }

constexpr PieceType type_of(Piece pc) { return PieceType(pc & 7); }

constexpr Color color_of(Piece pc) {
    assert(pc != NO_PIECE);
    return Color(pc >> 3);
}

constexpr bool is_ok(Square s) { return s >= SQ_A1 && s <= SQ_P8; }

constexpr File file_of(Square s) { return File(s & 15); }  // 16 files: mask with 0xF

constexpr Rank rank_of(Square s) { return Rank(s >> 4); }  // 16 files per rank: shift by 4

constexpr Square relative_square(Color c, Square s) { return Square(s ^ (c * 112)); }  // 7 ranks * 16 = 112

constexpr Rank relative_rank(Color c, Rank r) { return Rank(r ^ (c * 7)); }

constexpr Rank relative_rank(Color c, Square s) { return relative_rank(c, rank_of(s)); }

constexpr Direction pawn_push(Color c) { return c == WHITE ? NORTH : SOUTH; }

// Board half helpers — left half (files a-h) vs right half (files i-p)
constexpr bool is_left_half(Square s)  { return file_of(s) < FILE_I; }
constexpr bool is_right_half(Square s) { return file_of(s) >= FILE_I; }

// Which army's home territory a square is in
constexpr Seat home_seat(Square s) {
    bool right = is_right_half(s);
    bool top   = rank_of(s) >= RANK_5;
    if (!right && !top)  return SEAT_W1;
    if (!right && top)   return SEAT_B1;
    if (right  && !top)  return SEAT_W2;
    return SEAT_B2;
}

// King starting squares for each seat
constexpr Square king_start_sq(Seat s) {
    // w1: e1=4, b1: e8=116, w2: m1=12, b2: m8=124
    constexpr Square starts[SEAT_NB] = {
        Square(4),   // SQ_E1: w1 king
        Square(116), // SQ_E8: b1 king
        Square(12),  // SQ_M1: w2 king
        Square(124)  // SQ_M8: b2 king
    };
    return starts[s];
}

// Rook starting squares for each seat (kingside, queenside)
constexpr Square rook_start_sq(Seat s, bool kingside) {
    // w1: h1(K)=7, a1(Q)=0  |  b1: h8(K)=119, a8(Q)=112
    // w2: p1(K)=15, i1(Q)=8 |  b2: p8(K)=127, i8(Q)=120
    constexpr Square ks[SEAT_NB] = { Square(7), Square(119), Square(15), Square(127) };
    constexpr Square qs[SEAT_NB] = { Square(0), Square(112), Square(8),  Square(120) };
    return kingside ? ks[s] : qs[s];
}

constexpr Key make_key(uint64_t seed) {
    return seed * 6364136223846793005ULL + 1442695040888963407ULL;
}

// =============================================================================
// MOVE — 32-bit encoding for 128-square board
// =============================================================================
//
// bit  0- 6: destination square (0-127)
// bit  7-13: origin square (0-127)
// bit 14-15: promotion piece type - 2 (KNIGHT-2 to QUEEN-2)
// bit 16-17: special move flag: normal(0), promotion(1), en passant(2), castling(3)

enum MoveType : uint32_t {
    NORMAL,
    PROMOTION  = 1 << 16,
    EN_PASSANT = 2 << 16,
    CASTLING   = 3 << 16
};

class Move {
   public:
    Move() = default;
    constexpr explicit Move(std::uint32_t d) : data(d) {}

    constexpr Move(Square from, Square to) :
        data((uint32_t(from) << 7) + to) {}

    template<MoveType T>
    static constexpr Move make(Square from, Square to, PieceType pt = KNIGHT) {
        return Move(uint32_t(T) + ((pt - KNIGHT) << 14) + (uint32_t(from) << 7) + to);
    }

    constexpr Square from_sq() const {
        assert(is_ok());
        return Square((data >> 7) & 0x7F);
    }

    constexpr Square to_sq() const {
        assert(is_ok());
        return Square(data & 0x7F);
    }

    constexpr Square to_sq_unchecked() const { return Square(data & 0x7F); }

    constexpr MoveType type_of() const { return MoveType(data & (3U << 16)); }

    constexpr PieceType promotion_type() const { return PieceType(((data >> 14) & 3) + KNIGHT); }

    constexpr bool is_ok() const { return none().data != data && null().data != data; }

    static constexpr Move null() { return Move(uint32_t(129)); }  // from=1, to=1
    static constexpr Move none() { return Move(uint32_t(0)); }

    constexpr bool operator==(const Move& m) const { return data == m.data; }
    constexpr bool operator!=(const Move& m) const { return data != m.data; }

    constexpr explicit operator bool() const { return data != 0; }

    constexpr std::uint32_t raw() const { return data; }

    // Compress to 14-bit index for butterfly/low-ply history tables
    // Uses from_sq (7 bits) and to_sq (7 bits), ignoring promotion and move type
    constexpr std::uint32_t butterfly_index() const {
        return (data & 0x3FFF);  // bits 0-13: to(7) + from(7)
    }

    struct MoveHash {
        std::size_t operator()(const Move& m) const { return make_key(m.data); }
    };

   protected:
    std::uint32_t data;
};

// =============================================================================
// BITBOARD-SQUARE OPERATORS
// =============================================================================

constexpr Bitboard square_bb(Square s) {
    assert(is_ok(s));
    return bb_from_sq(s);
}

constexpr Bitboard  operator&(Bitboard b, Square s) { return b & square_bb(s); }
constexpr Bitboard  operator|(Bitboard b, Square s) { return b | square_bb(s); }
constexpr Bitboard  operator^(Bitboard b, Square s) { return b ^ square_bb(s); }
constexpr Bitboard& operator|=(Bitboard& b, Square s) { return b |= square_bb(s); }
constexpr Bitboard& operator^=(Bitboard& b, Square s) { return b ^= square_bb(s); }

constexpr Bitboard operator&(Square s, Bitboard b) { return b & s; }
constexpr Bitboard operator|(Square s, Bitboard b) { return b | s; }
constexpr Bitboard operator^(Square s, Bitboard b) { return b ^ s; }

constexpr Bitboard operator|(Square s1, Square s2) { return square_bb(s1) | s2; }

constexpr bool more_than_one(Bitboard b) { return b.more_than_one(); }

template<typename T, typename... Ts>
struct is_all_same {
    static constexpr bool value = (std::is_same_v<T, Ts> && ...);
};

template<typename... Ts>
constexpr auto is_all_same_v = is_all_same<Ts...>::value;

}  // namespace Stockfish

#endif  // #ifndef TYPES_H_INCLUDED

#include "tune.h"  // Global visibility to tuning setup
