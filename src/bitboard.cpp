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
// TEAM CHESS — Bitboard initialization for 128-square (16×8) board
// No magic bitboards — sliding attacks are computed on the fly.
// =============================================================================

#include "bitboard.h"

#include <algorithm>
#include <cmath>
#include <string>

namespace Stockfish {

// Global lookup tables
uint8_t  SquareDistance[SQUARE_NB][SQUARE_NB];

Bitboard BetweenBB[SQUARE_NB][SQUARE_NB];
Bitboard LineBB[SQUARE_NB][SQUARE_NB];
Bitboard RayPassBB[SQUARE_NB][SQUARE_NB];


// Returns an ASCII representation of the 16×8 bitboard for debugging.
std::string Bitboards::pretty(Bitboard b) {

    std::string s;

    // Top border
    s += "+";
    for (int f = 0; f < FILE_NB; ++f)
        s += "---+";
    s += "\n";

    for (Rank r = RANK_8;; --r)
    {
        for (File f = FILE_A; f <= FILE_P; ++f)
            s += (b & make_square(f, r)) ? "| X " : "|   ";

        s += "| " + std::to_string(1 + int(r)) + "\n+";
        for (int f = 0; f < FILE_NB; ++f)
            s += "---+";
        s += "\n";

        if (r == RANK_1)
            break;
    }

    // File labels
    s += "  ";
    for (File f = FILE_A; f <= FILE_P; ++f)
        s += std::string(1, char('a' + int(f))) + "   ";
    s += "\n";

    return s;
}


// Initializes SquareDistance, LineBB, BetweenBB, and RayPassBB tables.
// Called at startup.
void Bitboards::init() {

    // --- SquareDistance ---
    for (Square s1 = SQ_A1; s1 <= SQ_P8; ++s1)
        for (Square s2 = SQ_A1; s2 <= SQ_P8; ++s2)
            SquareDistance[s1][s2] = uint8_t(std::max(distance<File>(s1, s2),
                                                      distance<Rank>(s1, s2)));

    // --- LineBB, BetweenBB, RayPassBB ---
    // For every pair of squares that a sliding piece (bishop/rook) can connect,
    // compute the line through both squares, the squares strictly between them,
    // and the "ray past" (the ray from s1 through s2 and beyond).
    for (Square s1 = SQ_A1; s1 <= SQ_P8; ++s1)
    {
        for (PieceType pt : {BISHOP, ROOK})
        {
            Bitboard pseudo_s1 = PseudoAttacks[pt][s1];

            for (Square s2 = SQ_A1; s2 <= SQ_P8; ++s2)
            {
                if (bool(pseudo_s1 & s2))
                {
                    // Line: all squares on the line through s1 and s2
                    LineBB[s1][s2] = (attacks_bb(pt, s1, BB_ZERO) & attacks_bb(pt, s2, BB_ZERO))
                                   | s1 | s2;

                    // Between: squares strictly between s1 and s2
                    BetweenBB[s1][s2] =
                      attacks_bb(pt, s1, square_bb(s2)) & attacks_bb(pt, s2, square_bb(s1));

                    // RayPass: ray from s1 through s2 and beyond
                    RayPassBB[s1][s2] =
                      attacks_bb(pt, s1, BB_ZERO) & (attacks_bb(pt, s2, square_bb(s1)) | s2);
                }
                BetweenBB[s1][s2] |= s2;
            }
        }
    }
}

}  // namespace Stockfish
