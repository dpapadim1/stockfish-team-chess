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
// TEAM CHESS — Move generation for 16×8 board, 4 armies
//
// Key difference: only the CURRENT SEAT's pieces are moved.
// Captures can target any piece of the opposing color (either opponent army).
// Moves must not capture pieces of the same color (teammate or own army).
// =============================================================================

#include "movegen.h"

#include <cassert>
#include <initializer_list>

#include "bitboard.h"
#include "position.h"

namespace Stockfish {

namespace {

// Helper: write pawn moves from a bitboard of destinations
template<Direction offset>
inline Move* splat_pawn_moves(Move* moveList, Bitboard to_bb) {
    while (bool(to_bb))
    {
        Square to   = pop_lsb(to_bb);
        *moveList++ = Move(to - offset, to);
    }
    return moveList;
}

// Helper: write all moves from a single source square to destinations
inline Move* splat_moves(Move* moveList, Square from, Bitboard to_bb) {
    while (bool(to_bb))
        *moveList++ = Move(from, pop_lsb(to_bb));
    return moveList;
}


template<GenType Type, Direction D, bool Enemy>
Move* make_promotions(Move* moveList, [[maybe_unused]] Square to) {

    constexpr bool all = Type == EVASIONS || Type == NON_EVASIONS;

    if constexpr (Type == CAPTURES || all)
        *moveList++ = Move::make<PROMOTION>(to - D, to, QUEEN);

    if constexpr ((Type == CAPTURES && Enemy) || (Type == QUIETS && !Enemy) || all)
    {
        *moveList++ = Move::make<PROMOTION>(to - D, to, ROOK);
        *moveList++ = Move::make<PROMOTION>(to - D, to, BISHOP);
        *moveList++ = Move::make<PROMOTION>(to - D, to, KNIGHT);
    }

    return moveList;
}


// Generate pawn moves for the current seat only 
template<Color Us, GenType Type>
Move* generate_pawn_moves(const Position& pos, Move* moveList, Bitboard target, Seat seat) {

    constexpr Color     Them     = ~Us;
    constexpr Bitboard  TRank7BB = (Us == WHITE ? Rank7BB : Rank2BB);
    constexpr Bitboard  TRank3BB = (Us == WHITE ? Rank3BB : Rank6BB);
    constexpr Direction Up       = pawn_push(Us);
    constexpr Direction UpRight  = (Us == WHITE ? NORTH_EAST : SOUTH_WEST);
    constexpr Direction UpLeft   = (Us == WHITE ? NORTH_WEST : SOUTH_EAST);

    const Bitboard emptySquares = ~pos.pieces();
    // Exclude kings from capturable enemies — kings are never capturable in team chess
    const Bitboard enemies      = (Type == EVASIONS ? pos.checkers() : pos.pieces(Them)) & ~pos.pieces(KING);

    // Only this seat's pawns can move
    Bitboard seatPawns   = pos.pieces(seat, PAWN);
    Bitboard pawnsOn7    = seatPawns & TRank7BB;
    Bitboard pawnsNotOn7 = seatPawns & ~TRank7BB;

    // Single and double pawn pushes, no promotions
    if constexpr (Type != CAPTURES)
    {
        Bitboard b1 = shift<Up>(pawnsNotOn7) & emptySquares;
        Bitboard b2 = shift<Up>(b1 & TRank3BB) & emptySquares;

        if constexpr (Type == EVASIONS)
        {
            b1 &= target;
            b2 &= target;
        }

        moveList = splat_pawn_moves<Up>(moveList, b1);
        moveList = splat_pawn_moves<Up + Up>(moveList, b2);
    }

    // Promotions and underpromotions
    if (bool(pawnsOn7))
    {
        Bitboard b1 = shift<UpRight>(pawnsOn7) & enemies;
        Bitboard b2 = shift<UpLeft>(pawnsOn7) & enemies;
        Bitboard b3 = shift<Up>(pawnsOn7) & emptySquares;

        if constexpr (Type == EVASIONS)
            b3 &= target;

        while (bool(b1))
            moveList = make_promotions<Type, UpRight, true>(moveList, pop_lsb(b1));

        while (bool(b2))
            moveList = make_promotions<Type, UpLeft, true>(moveList, pop_lsb(b2));

        while (bool(b3))
            moveList = make_promotions<Type, Up, false>(moveList, pop_lsb(b3));
    }

    // Standard and en passant captures
    if constexpr (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
    {
        Bitboard b1 = shift<UpRight>(pawnsNotOn7) & enemies;
        Bitboard b2 = shift<UpLeft>(pawnsNotOn7) & enemies;

        moveList = splat_pawn_moves<UpRight>(moveList, b1);
        moveList = splat_pawn_moves<UpLeft>(moveList, b2);

        if (pos.ep_square() != SQ_NONE)
        {
            // In team chess, the EP square may exist but not be capturable by
            // the current seat's pawns (EP is shared across all seats).
            if (Type == EVASIONS && bool(target & (pos.ep_square() + Up)))
                return moveList;

            b1 = pawnsNotOn7 & pawn_attacks_bb(Them, pos.ep_square());

            while (bool(b1))
                *moveList++ = Move::make<EN_PASSANT>(pop_lsb(b1), pos.ep_square());
        }
    }

    return moveList;
}


// Generate non-pawn, non-king moves for the current seat
template<Color Us, PieceType Pt>
Move* generate_moves(const Position& pos, Move* moveList, Bitboard target, Seat seat) {

    static_assert(Pt != KING && Pt != PAWN, "Unsupported piece type in generate_moves()");

    // Only move this seat's pieces of the given type
    Bitboard bb = pos.pieces(seat, Pt);

    while (bool(bb))
    {
        Square   from = pop_lsb(bb);
        Bitboard b    = attacks_bb<Pt>(from, pos.pieces()) & target;

        moveList = splat_moves(moveList, from, b);
    }

    return moveList;
}


// Generate all moves for the current seat
template<Color Us, GenType Type>
Move* generate_all(const Position& pos, Move* moveList) {

    static_assert(Type != LEGAL, "Unsupported type in generate_all()");

    Seat seat = pos.seat_to_move();

    // Use this seat's king for evasion calculations
    const Square ksq = pos.square<KING>(seat);
    Bitboard     target;

    // Skip generating non-king moves when in double check
    if (Type != EVASIONS || !more_than_one(pos.checkers()))
    {
        target = Type == EVASIONS     ? between_bb(ksq, lsb(pos.checkers()))
               : Type == NON_EVASIONS ? ~pos.pieces(Us)      // Can't capture own color
               : Type == CAPTURES     ? pos.pieces(~Us)       // Capture enemy pieces
                                      : ~pos.pieces();        // QUIETS: any empty square

        // In team chess, kings are never capturable (4 kings on board)
        target &= ~pos.pieces(KING);

        moveList = generate_pawn_moves<Us, Type>(pos, moveList, target, seat);
        moveList = generate_moves<Us, KNIGHT>(pos, moveList, target, seat);
        moveList = generate_moves<Us, BISHOP>(pos, moveList, target, seat);
        moveList = generate_moves<Us, ROOK>(pos, moveList, target, seat);
        moveList = generate_moves<Us, QUEEN>(pos, moveList, target, seat);
    }

    // King moves — only this seat's king
    // Also exclude enemy kings from king capture targets
    Bitboard kingTarget = (Type == EVASIONS ? ~pos.pieces(Us) : target) & ~pos.pieces(KING);
    Bitboard b = attacks_bb<KING>(ksq) & kingTarget;
    moveList = splat_moves(moveList, ksq, b);

    // Castling — seat-specific rights
    if ((Type == QUIETS || Type == NON_EVASIONS) && pos.can_castle(seat_castling(seat)))
    {
        CastlingRights oo  = seat_oo(seat);
        CastlingRights ooo = seat_ooo(seat);

        if (pos.can_castle(oo) && !pos.castling_impeded(oo))
            *moveList++ = Move::make<CASTLING>(ksq, pos.castling_rook_square(oo));

        if (pos.can_castle(ooo) && !pos.castling_impeded(ooo))
            *moveList++ = Move::make<CASTLING>(ksq, pos.castling_rook_square(ooo));
    }

    return moveList;
}

}  // namespace


// Generate pseudo-legal moves of the given type
template<GenType Type>
Move* generate(const Position& pos, Move* moveList) {

    static_assert(Type != LEGAL, "Unsupported type in generate()");
    assert((Type == EVASIONS) == bool(pos.checkers()));

    Color us = pos.side_to_move();

    return us == WHITE ? generate_all<WHITE, Type>(pos, moveList)
                       : generate_all<BLACK, Type>(pos, moveList);
}

// Explicit template instantiations
template Move* generate<CAPTURES>(const Position&, Move*);
template Move* generate<QUIETS>(const Position&, Move*);
template Move* generate<EVASIONS>(const Position&, Move*);
template Move* generate<NON_EVASIONS>(const Position&, Move*);

// generate<LEGAL> generates all legal moves
template<>
Move* generate<LEGAL>(const Position& pos, Move* moveList) {

    Color    us     = pos.side_to_move();
    Seat     seat   = pos.seat_to_move();
    Bitboard pinned = pos.blockers_for_king(us) & pos.pieces(us);
    Square   ksq    = pos.square<KING>(seat);
    Move*    cur    = moveList;

    moveList =
      bool(pos.checkers()) ? generate<EVASIONS>(pos, moveList) : generate<NON_EVASIONS>(pos, moveList);

    while (cur != moveList)
        if ((bool(pinned & cur->from_sq()) || cur->from_sq() == ksq || cur->type_of() == EN_PASSANT)
            && !pos.legal(*cur))
            *cur = *(--moveList);
        else
            ++cur;

    return moveList;
}

}  // namespace Stockfish
