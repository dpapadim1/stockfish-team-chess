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
// TEAM CHESS — 128-bit Bitboard for 16×8 board
// =============================================================================

#ifndef BITBOARD_H_INCLUDED
#define BITBOARD_H_INCLUDED

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <initializer_list>
#include <array>

#include "types.h"

namespace Stockfish {

namespace Bitboards {

void        init();
std::string pretty(Bitboard b);

}  // namespace Stockfish::Bitboards

// =============================================================================
// FILE BITBOARDS — 16 files on the 128-square board
// =============================================================================
// A file has bits set at positions: f, f+16, f+32, f+48, f+64, f+80, f+96, f+112
// For f < 4:  bits in lo only up to sq 63, rest in hi.
// File A (f=0): squares 0,16,32,48 in lo; 64,80,96,112 in hi
// Pattern per half: one bit every 16 positions within 64 bits

// Helper: compute file bitboard at compile time
constexpr Bitboard make_file_bb(int f) {
    // file f has squares at ranks 0-7: f, f+16, f+32, f+48, f+64, f+80, f+96, f+112
    uint64_t lo = 0, hi = 0;
    for (int r = 0; r < 8; r++) {
        int sq = r * 16 + f;
        if (sq < 64)
            lo |= (1ULL << sq);
        else
            hi |= (1ULL << (sq - 64));
    }
    return {lo, hi};
}

constexpr Bitboard FileABB = make_file_bb(0);
constexpr Bitboard FileBBB = make_file_bb(1);
constexpr Bitboard FileCBB = make_file_bb(2);
constexpr Bitboard FileDBB = make_file_bb(3);
constexpr Bitboard FileEBB = make_file_bb(4);
constexpr Bitboard FileFBB = make_file_bb(5);
constexpr Bitboard FileGBB = make_file_bb(6);
constexpr Bitboard FileHBB = make_file_bb(7);
constexpr Bitboard FileIBB = make_file_bb(8);
constexpr Bitboard FileJBB = make_file_bb(9);
constexpr Bitboard FileKBB = make_file_bb(10);
constexpr Bitboard FileLBB = make_file_bb(11);
constexpr Bitboard FileMBB = make_file_bb(12);
constexpr Bitboard FileNBB = make_file_bb(13);
constexpr Bitboard FileOBB = make_file_bb(14);
constexpr Bitboard FilePBB = make_file_bb(15);

// =============================================================================
// RANK BITBOARDS — 8 ranks, each 16 bits wide
// =============================================================================
// Rank r has 16 consecutive bits starting at position r*16
// Rank 1: bits 0-15 (all in lo)
// Rank 4: bits 48-63 (all in lo)
// Rank 5: bits 64-79 (all in hi)
// Rank 8: bits 112-127 (all in hi)

constexpr Bitboard make_rank_bb(int r) {
    uint64_t mask16 = 0xFFFF; // 16 consecutive bits
    int start = r * 16;
    if (start < 64)
        return {mask16 << start, 0};
    else
        return {0, mask16 << (start - 64)};
}

constexpr Bitboard Rank1BB = make_rank_bb(0);
constexpr Bitboard Rank2BB = make_rank_bb(1);
constexpr Bitboard Rank3BB = make_rank_bb(2);
constexpr Bitboard Rank4BB = make_rank_bb(3);
constexpr Bitboard Rank5BB = make_rank_bb(4);
constexpr Bitboard Rank6BB = make_rank_bb(5);
constexpr Bitboard Rank7BB = make_rank_bb(6);
constexpr Bitboard Rank8BB = make_rank_bb(7);

// =============================================================================
// BOARD HALF BITBOARDS — for team chess territory
// =============================================================================

constexpr Bitboard make_half_bb(bool right_half) {
    // Each rank has 16 files: files 0-7 = left, files 8-15 = right
    uint64_t lo = 0, hi = 0;
    for (int r = 0; r < 8; r++) {
        for (int f = 0; f < 16; f++) {
            bool is_right = (f >= 8);
            if (is_right != right_half) continue;
            int sq = r * 16 + f;
            if (sq < 64) lo |= (1ULL << sq);
            else         hi |= (1ULL << (sq - 64));
        }
    }
    return {lo, hi};
}

constexpr Bitboard LeftHalfBB  = make_half_bb(false);  // files a-h
constexpr Bitboard RightHalfBB = make_half_bb(true);   // files i-p

// =============================================================================
// LOOKUP TABLES — initialized by Bitboards::init()
// =============================================================================

extern uint8_t  SquareDistance[SQUARE_NB][SQUARE_NB];

extern Bitboard BetweenBB[SQUARE_NB][SQUARE_NB];
extern Bitboard LineBB[SQUARE_NB][SQUARE_NB];
extern Bitboard RayPassBB[SQUARE_NB][SQUARE_NB];

// =============================================================================
// POPCOUNT for 128-bit Bitboard
// =============================================================================

inline int popcount(Bitboard b) {
#if defined(USE_POPCNT) && defined(_MSC_VER)
    return int(_mm_popcnt_u64(b.lo)) + int(_mm_popcnt_u64(b.hi));
#elif defined(USE_POPCNT) && defined(__GNUC__)
    return __builtin_popcountll(b.lo) + __builtin_popcountll(b.hi);
#else
    // Software popcount
    auto pc64 = [](uint64_t v) -> int {
        v = v - ((v >> 1) & 0x5555555555555555ULL);
        v = (v & 0x3333333333333333ULL) + ((v >> 2) & 0x3333333333333333ULL);
        v = (v + (v >> 4)) & 0x0F0F0F0F0F0F0F0FULL;
        return static_cast<int>((v * 0x0101010101010101ULL) >> 56);
    };
    return pc64(b.lo) + pc64(b.hi);
#endif
}

constexpr int constexpr_popcount(Bitboard b) {
    auto pc64 = [](uint64_t v) constexpr -> int {
        v = v - ((v >> 1) & 0x5555555555555555ULL);
        v = (v & 0x3333333333333333ULL) + ((v >> 2) & 0x3333333333333333ULL);
        v = (v + (v >> 4)) & 0x0F0F0F0F0F0F0F0FULL;
        return static_cast<int>((v * 0x0101010101010101ULL) >> 56);
    };
    return pc64(b.lo) + pc64(b.hi);
}

// =============================================================================
// LSB / MSB / POP_LSB for 128-bit Bitboard
// =============================================================================

inline Square lsb(Bitboard b) {
    assert(bool(b));
    if (b.lo) {
#if defined(__GNUC__)
        return Square(__builtin_ctzll(b.lo));
#elif defined(_MSC_VER) && defined(_WIN64)
        unsigned long idx;
        _BitScanForward64(&idx, b.lo);
        return Square(idx);
#else
        // Software fallback
        for (int i = 0; i < 64; i++)
            if ((b.lo >> i) & 1) return Square(i);
#endif
    }
    assert(b.hi);
#if defined(__GNUC__)
    return Square(64 + __builtin_ctzll(b.hi));
#elif defined(_MSC_VER) && defined(_WIN64)
    unsigned long idx;
    _BitScanForward64(&idx, b.hi);
    return Square(64 + idx);
#else
    for (int i = 0; i < 64; i++)
        if ((b.hi >> i) & 1) return Square(64 + i);
    return SQ_NONE;
#endif
}

inline Square msb(Bitboard b) {
    assert(bool(b));
    if (b.hi) {
#if defined(__GNUC__)
        return Square(127 - __builtin_clzll(b.hi));
#elif defined(_MSC_VER) && defined(_WIN64)
        unsigned long idx;
        _BitScanReverse64(&idx, b.hi);
        return Square(64 + idx);
#else
        for (int i = 63; i >= 0; i--)
            if ((b.hi >> i) & 1) return Square(64 + i);
#endif
    }
    assert(b.lo);
#if defined(__GNUC__)
    return Square(63 - __builtin_clzll(b.lo));
#elif defined(_MSC_VER) && defined(_WIN64)
    unsigned long idx;
    _BitScanReverse64(&idx, b.lo);
    return Square(idx);
#else
    for (int i = 63; i >= 0; i--)
        if ((b.lo >> i) & 1) return Square(i);
    return SQ_NONE;
#endif
}

inline Bitboard least_significant_square_bb(Bitboard b) {
    assert(bool(b));
    return b & (-b);
}

inline Square pop_lsb(Bitboard& b) {
    assert(bool(b));
    const Square s = lsb(b);
    b.clear(s);
    return s;
}

// =============================================================================
// RANK / FILE BITBOARD LOOKUPS
// =============================================================================

constexpr Bitboard rank_bb(Rank r) { return make_rank_bb(r); }
constexpr Bitboard rank_bb(Square s) { return rank_bb(rank_of(s)); }

constexpr Bitboard file_bb(File f) { return make_file_bb(f); }
constexpr Bitboard file_bb(Square s) { return file_bb(file_of(s)); }

// =============================================================================
// SHIFT — move a bitboard in a direction, respecting board edges
// =============================================================================

template<Direction D>
constexpr Bitboard shift(Bitboard b) {
    // For the 16×8 board: files a-p (0-15)
    // EAST: shift left by 1, masking off file P (rightmost) to prevent wrap
    // WEST: shift right by 1, masking off file A (leftmost) to prevent wrap
    // NORTH: shift left by 16 (one rank up)
    // SOUTH: shift right by 16 (one rank down)
    constexpr Bitboard NotFileA = ~FileABB;
    constexpr Bitboard NotFileP = ~FilePBB;

    if constexpr (D == NORTH)         return b << 16;
    else if constexpr (D == SOUTH)    return b >> 16;
    else if constexpr (D == NORTH + NORTH) return b << 32;
    else if constexpr (D == SOUTH + SOUTH) return b >> 32;
    else if constexpr (D == EAST)     return (b & NotFileP) << 1;
    else if constexpr (D == WEST)     return (b & NotFileA) >> 1;
    else if constexpr (D == NORTH_EAST) return (b & NotFileP) << 17;  // NORTH(16) + EAST(1)
    else if constexpr (D == NORTH_WEST) return (b & NotFileA) << 15;  // NORTH(16) + WEST(-1) = 15
    else if constexpr (D == SOUTH_EAST) return (b & NotFileP) >> 15;  // SOUTH(-16) + EAST(1) = -15
    else if constexpr (D == SOUTH_WEST) return (b & NotFileA) >> 17;  // SOUTH(-16) + WEST(-1) = -17
    else return BB_ZERO;
}

// =============================================================================
// PAWN ATTACKS
// =============================================================================

template<Color C>
constexpr Bitboard pawn_attacks_bb(Bitboard b) {
    return C == WHITE ? shift<NORTH_WEST>(b) | shift<NORTH_EAST>(b)
                      : shift<SOUTH_WEST>(b) | shift<SOUTH_EAST>(b);
}

// =============================================================================
// LINE / BETWEEN / RAY LOOKUP
// =============================================================================

inline Bitboard line_bb(Square s1, Square s2) {
    assert(is_ok(s1) && is_ok(s2));
    return LineBB[s1][s2];
}

inline Bitboard between_bb(Square s1, Square s2) {
    assert(is_ok(s1) && is_ok(s2));
    return BetweenBB[s1][s2];
}

// =============================================================================
// DISTANCE
// =============================================================================

template<typename T1 = Square>
inline int distance(Square x, Square y);

template<>
inline int distance<File>(Square x, Square y) {
    return std::abs(file_of(x) - file_of(y));
}

template<>
inline int distance<Rank>(Square x, Square y) {
    return std::abs(rank_of(x) - rank_of(y));
}

template<>
inline int distance<Square>(Square x, Square y) {
    return SquareDistance[x][y];
}

inline int edge_distance(File f) { return std::min(int(f), int(FILE_P) - int(f)); }

// =============================================================================
// ATTACK GENERATION — computed by walking rays (no magic bitboards for 128-sq)
// =============================================================================
// For the 128-square board, magic bitboards would require enormous tables.
// Instead, we use direct ray computation which is still fast enough.

namespace Bitboards {

constexpr Bitboard safe_destination(Square s, int step) {
    Square to = Square(int(s) + step);
    if (!is_ok(to)) return BB_ZERO;
    // Ensure we haven't wrapped around the board edge
    int fd = int(file_of(s)) - int(file_of(to));
    int rd = int(rank_of(s)) - int(rank_of(to));
    int file_diff = fd < 0 ? -fd : fd;
    int rank_diff = rd < 0 ? -rd : rd;
    if (file_diff > 2 || rank_diff > 2) return BB_ZERO;
    return square_bb(to);
}

// Compute sliding attack from square sq with given occupied bitboard
constexpr Bitboard sliding_attack(PieceType pt, Square sq, Bitboard occupied) {
    Bitboard  attacks = BB_ZERO;
    Direction RookDirections[4]   = {NORTH, SOUTH, EAST, WEST};
    Direction BishopDirections[4] = {NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST};

    for (Direction d : (pt == ROOK ? RookDirections : BishopDirections))
    {
        Square s = sq;
        while (bool(safe_destination(s, int(d))))
        {
            s = Square(int(s) + int(d));
            attacks |= s;
            if (bool(occupied & s))
                break;
        }
    }

    return attacks;
}

constexpr Bitboard knight_attack(Square sq) {
    Bitboard b = BB_ZERO;
    // Knight moves: (±1,±2) and (±2,±1) in file,rank
    // On 16-wide board: offsets are ±(2*16+1)=±33, ±(2*16-1)=±31, ±(16+2)=±18, ±(16-2)=±14
    for (int step : {-33, -31, -18, -14, 14, 18, 31, 33})
        b |= safe_destination(sq, step);
    return b;
}

constexpr Bitboard king_attack(Square sq) {
    Bitboard b = BB_ZERO;
    // King moves: (±1,0), (0,±1), (±1,±1) in file,rank
    // Offsets: ±1, ±16, ±15, ±17
    for (int step : {-17, -16, -15, -1, 1, 15, 16, 17})
        b |= safe_destination(sq, step);
    return b;
}

constexpr Bitboard pseudo_attacks(PieceType pt, Square sq) {
    switch (pt) {
    case ROOK:
    case BISHOP:
        return sliding_attack(pt, sq, BB_ZERO);
    case QUEEN:
        return sliding_attack(ROOK, sq, BB_ZERO) | sliding_attack(BISHOP, sq, BB_ZERO);
    case KNIGHT:
        return knight_attack(sq);
    case KING:
        return king_attack(sq);
    default:
        assert(false);
        return BB_ZERO;
    }
}

}  // namespace Bitboards

// =============================================================================
// ATTACK TABLES — computed at compile time
// =============================================================================

// Pawn attacks: indexed by [Color][Square]
inline constexpr auto PawnAttacks = []() constexpr {
    std::array<std::array<Bitboard, SQUARE_NB>, COLOR_NB> pa{};
    for (int s = SQ_A1; s <= int(SQ_P8); ++s) {
        pa[WHITE][s] = pawn_attacks_bb<WHITE>(square_bb(Square(s)));
        pa[BLACK][s] = pawn_attacks_bb<BLACK>(square_bb(Square(s)));
    }
    return pa;
}();

// Pseudo attacks: indexed by [PieceType][Square] — empty-board attacks for non-pawn pieces
inline constexpr auto PseudoAttacks = []() constexpr {
    std::array<std::array<Bitboard, SQUARE_NB>, PIECE_TYPE_NB> attacks{};
    for (int s = SQ_A1; s <= int(SQ_P8); ++s) {
        Square sq = Square(s);
        attacks[KING][s]   = Bitboards::pseudo_attacks(KING, sq);
        attacks[KNIGHT][s] = Bitboards::pseudo_attacks(KNIGHT, sq);
        attacks[BISHOP][s] = Bitboards::pseudo_attacks(BISHOP, sq);
        attacks[ROOK][s]   = Bitboards::pseudo_attacks(ROOK, sq);
        attacks[QUEEN][s]  = attacks[BISHOP][s] | attacks[ROOK][s];
    }
    return attacks;
}();

// =============================================================================
// ATTACK LOOKUP FUNCTIONS
// =============================================================================

// Pawn attacks for a single square
inline Bitboard pawn_attacks_bb(Color c, Square s) {
    assert(c < COLOR_NB && is_ok(s));
    return PawnAttacks[c][s];
}

// Piece pseudo-attacks on empty board
template<PieceType Pt>
inline Bitboard attacks_bb(Square s) {
    static_assert(Pt != PAWN, "Use pawn_attacks_bb(Color, Square) for pawn attacks");
    assert(is_ok(s));
    return PseudoAttacks[Pt][s];
}

// Sliding piece attacks with occupied bitboard
// For 128-square board, we compute on the fly (no magic bitboards)
template<PieceType Pt>
inline Bitboard attacks_bb(Square s, Bitboard occupied) {
    assert(Pt != PAWN && is_ok(s));

    if constexpr (Pt == BISHOP)
        return Bitboards::sliding_attack(BISHOP, s, occupied);
    else if constexpr (Pt == ROOK)
        return Bitboards::sliding_attack(ROOK, s, occupied);
    else if constexpr (Pt == QUEEN)
        return Bitboards::sliding_attack(BISHOP, s, occupied)
             | Bitboards::sliding_attack(ROOK, s, occupied);
    else
        return PseudoAttacks[Pt][s];
}

// Non-template version for sliding attacks with occupancy
inline Bitboard attacks_bb(PieceType pt, Square s, Bitboard occupied) {
    assert(pt != PAWN && is_ok(s));

    switch (pt) {
    case BISHOP: return attacks_bb<BISHOP>(s, occupied);
    case ROOK:   return attacks_bb<ROOK>(s, occupied);
    case QUEEN:  return attacks_bb<QUEEN>(s, occupied);
    default:     return PseudoAttacks[pt][s];
    }
}

// Attack lookup by Piece (includes color info for pawns)
inline Bitboard attacks_bb(Piece pc, Square s) {
    if (type_of(pc) == PAWN)
        return PawnAttacks[color_of(pc)][s];
    return PseudoAttacks[type_of(pc)][s];
}

inline Bitboard attacks_bb(Piece pc, Square s, Bitboard occupied) {
    if (type_of(pc) == PAWN)
        return PawnAttacks[color_of(pc)][s];
    return attacks_bb(type_of(pc), s, occupied);
}

}  // namespace Stockfish

#endif  // #ifndef BITBOARD_H_INCLUDED
