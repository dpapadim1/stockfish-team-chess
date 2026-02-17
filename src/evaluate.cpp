/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2026 The Stockfish developers

  Team Chess variant — classical evaluation (no NNUE)
  
  Evaluates the position from the side-to-move perspective.
  WHITE team (w1+w2) scores are positive, BLACK team (b1+b2) negative.
  
  Features:
    1) Material balance
    2) Piece-square tables (centralization, development)
    3) Pawn structure (doubled, isolated, passed)
    4) Mobility
    5) King safety (basic)
    6) Rook on open files
    7) Bishop pair bonus
*/

#include "evaluate.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>

#include "bitboard.h"
#include "position.h"
#include "types.h"
#include "nnue/team_nnue.h"

namespace Stockfish {
namespace Eval {

// =============================================================================
// PIECE-SQUARE TABLES — bonuses for piece placement (from white's perspective)
// Indexed by [PieceType][Square], mirrored for black using relative_square.
//
// For 16×8 board: use file_of(s) clamped to 0-7 range (left half identical to
// right half in terms of centrality bonuses). Multiply file centrality by rank.
// =============================================================================

// Centrality bonus: distance from center (0-7 for each axis)
// For a 16-file board, center files are around 7-8 (0-indexed)
static int centrality_file(int f) {
    // Files 0-15: center at 7.5, max distance 7.5
    int d = std::abs(2 * f - 15);  // 0 at center, 15 at edges
    return 15 - d;  // Higher = more central
}

static int centrality_rank(int r) {
    // Ranks 0-7: center at 3.5
    int d = std::abs(2 * r - 7);  // 0 at center, 7 at edges
    return 7 - d;
}

// PST values for knights/bishops (prefer center)
static int knight_pst(Square s) {
    int f = file_of(s), r = rank_of(s);
    return centrality_file(f) + centrality_rank(r) * 2;
}

static int bishop_pst(Square s) {
    int f = file_of(s), r = rank_of(s);
    return centrality_file(f) + centrality_rank(r);
}

// Rook PST: prefer open files and 7th rank
static int rook_pst(Square s, Color c) {
    int r = relative_rank(c, rank_of(s));
    return (r == RANK_7 ? 20 : 0) + (r == RANK_8 ? 10 : 0);
}

// Pawn PST: advancement bonus
static int pawn_pst(Square s, Color c) {
    int r = relative_rank(c, rank_of(s));
    // Bonus for advancement, especially past middle
    static const int advance_bonus[] = {0, 0, 5, 10, 20, 35, 50, 0};
    return advance_bonus[r];
}


// =============================================================================
// CLASSICAL EVALUATION
// =============================================================================

int simple_eval(const Position& pos) {
    Color c = pos.side_to_move();
    return PawnValue * (pos.count<PAWN>(c) - pos.count<PAWN>(~c))
         + pos.non_pawn_material(c) - pos.non_pawn_material(~c);
}


Value evaluate(const Position& pos, int optimism) {

    assert(!bool(pos.checkers()));

    Color us   = pos.side_to_move();
    Color them = ~us;

    int score = 0;

    // --- 1. Material ---
    score += PawnValue * (pos.count<PAWN>(us) - pos.count<PAWN>(them));
    score += pos.non_pawn_material(us) - pos.non_pawn_material(them);

    // --- 2. Piece-Square Tables ---
    // Knights
    {
        Bitboard bb = pos.pieces(us, KNIGHT);
        while (bool(bb)) score += knight_pst(pop_lsb(bb));
        bb = pos.pieces(them, KNIGHT);
        while (bool(bb)) score -= knight_pst(pop_lsb(bb));
    }

    // Bishops
    {
        Bitboard bb = pos.pieces(us, BISHOP);
        while (bool(bb)) score += bishop_pst(pop_lsb(bb));
        bb = pos.pieces(them, BISHOP);
        while (bool(bb)) score -= bishop_pst(pop_lsb(bb));
    }

    // Rooks
    {
        Bitboard bb = pos.pieces(us, ROOK);
        while (bool(bb)) score += rook_pst(pop_lsb(bb), us);
        bb = pos.pieces(them, ROOK);
        while (bool(bb)) score -= rook_pst(pop_lsb(bb), them);
    }

    // Pawns
    {
        Bitboard bb = pos.pieces(us, PAWN);
        while (bool(bb)) score += pawn_pst(pop_lsb(bb), us);
        bb = pos.pieces(them, PAWN);
        while (bool(bb)) score -= pawn_pst(pop_lsb(bb), them);
    }

    // --- 3. Bishop pair ---
    if (pos.count<BISHOP>(us) >= 2)
        score += 30;
    if (pos.count<BISHOP>(them) >= 2)
        score -= 30;

    // --- 4. Rook on open/semi-open file ---
    {
        Bitboard bbUs = pos.pieces(us, ROOK);
        while (bool(bbUs)) {
            Square s = pop_lsb(bbUs);
            Bitboard fileBB = file_bb(s);
            if (!bool(fileBB & pos.pieces(us, PAWN)))
                score += bool(fileBB & pos.pieces(them, PAWN)) ? 10 : 20;  // Semi-open or open
        }
        Bitboard bbThem = pos.pieces(them, ROOK);
        while (bool(bbThem)) {
            Square s = pop_lsb(bbThem);
            Bitboard fileBB = file_bb(s);
            if (!bool(fileBB & pos.pieces(them, PAWN)))
                score -= bool(fileBB & pos.pieces(us, PAWN)) ? 10 : 20;
        }
    }

    // --- 5. Mobility (simplified — count of pseudo-legal moves for non-pawn pieces) ---
    {
        Bitboard occupied = pos.pieces();
        int mob_us = 0, mob_them = 0;

        for (PieceType pt : {KNIGHT, BISHOP, ROOK, QUEEN}) {
            Bitboard bb = pos.pieces(us, pt);
            while (bool(bb)) {
                Square s = pop_lsb(bb);
                mob_us += popcount(attacks_bb(pt, s, occupied) & ~pos.pieces(us));
            }
            bb = pos.pieces(them, pt);
            while (bool(bb)) {
                Square s = pop_lsb(bb);
                mob_them += popcount(attacks_bb(pt, s, occupied) & ~pos.pieces(them));
            }
        }
        score += (mob_us - mob_them) * 3;  // 3 centipawns per mobility square
    }

    // --- 6. Pawn structure ---
    {
        Bitboard ourPawns   = pos.pieces(us, PAWN);
        Bitboard theirPawns = pos.pieces(them, PAWN);

        // Doubled pawns penalty
        for (File f = FILE_A; f <= FILE_P; ++f) {
            Bitboard fp = file_bb(f);
            int ourOnFile = popcount(ourPawns & fp);
            int theirOnFile = popcount(theirPawns & fp);
            if (ourOnFile > 1) score -= 10 * (ourOnFile - 1);
            if (theirOnFile > 1) score += 10 * (theirOnFile - 1);
        }

        // Isolated pawn penalty (no friendly pawn on adjacent files)
        Bitboard temp = ourPawns;
        while (bool(temp)) {
            Square s = pop_lsb(temp);
            File f = file_of(s);
            Bitboard adjacent = BB_ZERO;
            if (f > FILE_A) adjacent |= file_bb(File(f - 1));
            if (f < FILE_P) adjacent |= file_bb(File(f + 1));
            if (!bool(adjacent & ourPawns))
                score -= 15;
        }
        temp = theirPawns;
        while (bool(temp)) {
            Square s = pop_lsb(temp);
            File f = file_of(s);
            Bitboard adjacent = BB_ZERO;
            if (f > FILE_A) adjacent |= file_bb(File(f - 1));
            if (f < FILE_P) adjacent |= file_bb(File(f + 1));
            if (!bool(adjacent & theirPawns))
                score += 15;
        }
    }

    // --- 7. King safety ---
    // Evaluate all 4 kings. Penalize positions where enemy pieces attack
    // the king zone, especially when escape squares are scarce (mate threats).
    {
        Bitboard occupied = pos.pieces();
        int seatPenalty[SEAT_NB] = {0, 0, 0, 0};  // Track per-king penalties

        for (int seatIdx = 0; seatIdx < int(SEAT_NB); seatIdx++) {
            Seat seat = Seat(seatIdx);
            Color kc  = seat_color(seat);
            Color opp = ~kc;

            // Skip if this seat has no king (shouldn't happen, but be safe)
            if (pos.count<KING>(seat) == 0)
                continue;

            Square ksq = pos.square<KING>(seat);

            // King zone: king square + all adjacent squares
            Bitboard kingZone = PseudoAttacks[KING][ksq] | square_bb(ksq);

            // --- 7a. Count enemy attackers to king zone, weighted by piece type ---
            int attackUnits  = 0;
            int attackerCount = 0;

            Bitboard zone = kingZone;
            while (bool(zone)) {
                Square sq = pop_lsb(zone);
                Bitboard att = pos.attackers_to(sq, occupied) & pos.pieces(opp);
                if (bool(att)) {
                    attackerCount++;
                    attackUnits += popcount(att & pos.pieces(opp, QUEEN))  * 9;
                    attackUnits += popcount(att & pos.pieces(opp, ROOK))   * 5;
                    attackUnits += popcount(att & pos.pieces(opp, BISHOP)) * 4;
                    attackUnits += popcount(att & pos.pieces(opp, KNIGHT)) * 4;
                    attackUnits += popcount(att & pos.pieces(opp, PAWN))   * 1;
                }
            }

            // --- 7b. Pawn shelter: friendly pawns in front of the king ---
            int shelter = 0;
            File kf = file_of(ksq);
            Rank kr = rank_of(ksq);
            for (int df = -1; df <= 1; df++) {
                int ff = int(kf) + df;
                if (ff < 0 || ff >= int(FILE_NB)) continue;
                File f = File(ff);
                Bitboard filePawns = file_bb(f) & pos.pieces(kc, PAWN);
                // Check for a pawn in front of the king on this file
                Bitboard temp = filePawns;
                while (bool(temp)) {
                    Square ps = pop_lsb(temp);
                    Rank pr = rank_of(ps);
                    if ((kc == WHITE && pr > kr) || (kc == BLACK && pr < kr)) {
                        // Distance bonus: closer pawns shield better
                        int dist = std::abs(int(pr) - int(kr));
                        shelter += (dist <= 1) ? 20 : 10;
                        break;  // One pawn per file is enough
                    }
                }
            }

            // --- 7c. Safe escape squares (king mobility with X-ray through king) ---
            int escapeSquares = 0;
            Bitboard kRemoved = occupied ^ square_bb(ksq);  // X-ray: remove king
            Bitboard kingMoves = PseudoAttacks[KING][ksq] & ~pos.pieces(kc);
            while (bool(kingMoves)) {
                Square esq = pop_lsb(kingMoves);
                // Check if this escape square is attacked by the opponent
                if (!bool(pos.attackers_to(esq, kRemoved) & pos.pieces(opp)))
                    escapeSquares++;
            }

            // --- 7d. Compute king safety penalty ---
            int penalty = 0;

            if (attackerCount >= 3) {
                // Multiple enemy pieces attacking king zone: very dangerous
                penalty += attackUnits * 12;
                if (escapeSquares == 0)
                    penalty += 1500;  // Near-mate threat!
                else if (escapeSquares == 1)
                    penalty += 600;
                else if (escapeSquares == 2)
                    penalty += 150;
            } else if (attackerCount == 2) {
                penalty += attackUnits * 8;
                if (escapeSquares == 0)
                    penalty += 800;
                else if (escapeSquares == 1)
                    penalty += 250;
                else if (escapeSquares == 2)
                    penalty += 60;
            } else if (attackerCount == 1) {
                penalty += attackUnits * 3;
            }

            // Reduce penalty for good pawn shelter
            penalty = std::max(0, penalty - shelter);

            // --- 7e. Weak squares around king (f2/f7 type vulnerability) ---
            // Squares adjacent to king that are attacked by opponent but not
            // defended by any friendly pawn
            {
                Bitboard pawnDefense = (kc == WHITE)
                    ? pawn_attacks_bb<WHITE>(pos.pieces(kc, PAWN))
                    : pawn_attacks_bb<BLACK>(pos.pieces(kc, PAWN));
                Bitboard weakSquares = kingZone & ~pawnDefense & ~square_bb(ksq);
                Bitboard weakAttacked = BB_ZERO;
                Bitboard ws = weakSquares;
                while (bool(ws)) {
                    Square sq = pop_lsb(ws);
                    if (bool(pos.attackers_to(sq, occupied) & pos.pieces(opp)))
                        weakAttacked |= square_bb(sq);
                }
                penalty += popcount(weakAttacked) * 15;
            }

            // --- 7f. Persistent check penalty (team chess specific) ---
            // If this king is directly attacked by enemy pieces, it's in check
            // from a previous opponent move. In team chess, this is extremely
            // dangerous because the opponent gets ANOTHER move before the
            // checked team member can respond. Near-checkmate territory.
            {
                Bitboard directCheckers = pos.attackers_to(ksq, occupied) & pos.pieces(opp);
                if (bool(directCheckers)) {
                    int numCheckers = popcount(directCheckers);
                    // Huge penalty: king under direct fire
                    penalty += 1000 * numCheckers;

                    // Near-checkmate: in check with few/no escapes
                    if (escapeSquares == 0)
                        penalty += 4000;  // Almost certainly dead
                    else if (escapeSquares == 1)
                        penalty += 2000;  // Very likely to be mated
                    else if (escapeSquares == 2)
                        penalty += 800;

                    // Double check: must move king, escapes are limited
                    if (numCheckers >= 2 && escapeSquares <= 1)
                        penalty += 3000;
                }
            }

            seatPenalty[seatIdx] = penalty;

            // Apply from side-to-move perspective
            if (kc == us)
                score -= penalty;
            else
                score += penalty;
        }

        // --- 7f. Focus bonus: reward concentrated attack on weaker enemy king ---
        // In team chess, checkmate only requires mating ONE enemy king.
        // Bonus for having a much larger attack on one king vs the other.
        {
            Seat ourOpp1 = (us == WHITE) ? SEAT_B1 : SEAT_W1;
            Seat ourOpp2 = (us == WHITE) ? SEAT_B2 : SEAT_W2;
            int p1 = seatPenalty[int(ourOpp1)];  // Our pressure on enemy king 1
            int p2 = seatPenalty[int(ourOpp2)];  // Our pressure on enemy king 2
            int maxPressure = std::max(p1, p2);
            int minPressure = std::min(p1, p2);

            // Bonus for focusing fire: the weaker king being under heavy pressure
            // is worth more than spreading attacks evenly
            if (maxPressure > 100) {
                int focusBonus = (maxPressure - minPressure) / 4;
                // Extra bonus when one king is near-mate territory
                if (maxPressure >= 500)
                    focusBonus += maxPressure / 3;
                score += focusBonus;
            }
        }
    }

    // --- 8. NNUE blend ---
    // When NNUE weights are loaded, blend neural evaluation with classical.
    // NNUE learns patterns the classical eval misses (piece coordination,
    // attack patterns, team-specific dynamics).
    if (TeamNNUE::global_nnue().is_loaded()) {
        Value nnueScore = TeamNNUE::global_nnue().evaluate(pos);
        // Blend: 60% NNUE + 40% classical (NNUE is primary when available)
        score = (int(nnueScore) * 3 + score * 2) / 5;
    }

    // --- 9. Optimism blend ---
    score += optimism / 4;

    // --- 10. Shuffle dampening ---
    score -= score * pos.rule50_count() / 200;

    // Clamp to avoid tablebase range
    score = std::clamp(score, VALUE_TB_LOSS_IN_MAX_PLY + 1, VALUE_TB_WIN_IN_MAX_PLY - 1);

    return Value(score);
}


std::string trace(const Position& pos) {

    if (bool(pos.checkers()))
        return "Final evaluation: none (in check)";

    std::stringstream ss;
    ss << std::showpoint << std::showpos << std::fixed << std::setprecision(2);

    int mat = simple_eval(pos);
    ss << "Material     : " << 0.01 * mat << " (side to move)\n";

    Value v = evaluate(pos, VALUE_ZERO);
    ss << "Classical    : " << 0.01 * int(v) << " (side to move)\n";

    return ss.str();
}

}  // namespace Eval
}  // namespace Stockfish
