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
// TEAM CHESS — Position representation for 16×8, 4 armies
//
// Key extensions:
//   • seatToMove (Seat) — which of the 4 armies is to move
//   • byArmyBB[SEAT_NB] — bitboard of pieces for each seat
//   • armyOf[SQUARE_NB] — which seat owns the piece on each square
//   • 4 kings tracked independently
//   • castling rights expanded to 8 (4 seats × 2 sides)
// =============================================================================

#ifndef POSITION_H_INCLUDED
#define POSITION_H_INCLUDED

#include <array>
#include <cassert>
#include <deque>
#include <iosfwd>
#include <memory>
#include <new>
#include <string>

#include "bitboard.h"
#include "types.h"

namespace Stockfish {

class TranspositionTable;
struct SharedHistories;

// StateInfo stores information needed to restore a Position object
// to its previous state when we retract a move.
struct StateInfo {

    // Copied when making a move
    Key    materialKey;
    Key    pawnKey;
    Key    minorPieceKey;
    Key    nonPawnKey[COLOR_NB];
    Value  nonPawnMaterial[COLOR_NB];
    int    castlingRights;
    int    rule50;
    int    pliesFromNull;
    Square epSquare;
    Seat   seatToMove;   // Which seat's turn it is

    // Not copied when making a move (will be recomputed anyhow)
    Key        key;
    Bitboard   checkersBB;
    StateInfo* previous;
    Bitboard   blockersForKing[COLOR_NB];
    Bitboard   blockersForSeatKing[SEAT_NB];  // Per-seat blocker tracking
    Bitboard   pinners[COLOR_NB];
    Bitboard   checkSquares[PIECE_TYPE_NB];
    Piece      capturedPiece;
    Seat       capturedSeat;   // Which seat owned the captured piece
    int        repetition;
};


using StateListPtr = std::unique_ptr<std::deque<StateInfo>>;


// Position class for Team Chess.
// Tracks the board state including 4 independent armies (seats).
class Position {
   public:
    static void init();

    Position()                           = default;
    Position(const Position&)            = delete;
    Position& operator=(const Position&) = delete;

    // FEN string input/output
    Position&   set(const std::string& fenStr, bool isChess960, StateInfo* si);
    Position&   set(const std::string& code, Color c, StateInfo* si);
    std::string fen() const;

    // Position representation
    Bitboard pieces() const;
    template<typename... PieceTypes>
    Bitboard pieces(PieceTypes... pts) const;
    Bitboard pieces(Color c) const;
    template<typename... PieceTypes>
    Bitboard pieces(Color c, PieceTypes... pts) const;
    Bitboard pieces(Seat s) const;         // Pieces belonging to a specific seat
    template<typename... PieceTypes>
    Bitboard pieces(Seat s, PieceTypes... pts) const;
    Piece    piece_on(Square s) const;
    Seat     seat_of(Square s) const;      // Which seat owns the piece on this square
    const std::array<Piece, SQUARE_NB>& piece_array() const;
    Square   ep_square() const;
    bool     empty(Square s) const;

    template<PieceType Pt>
    int count(Color c) const;
    template<PieceType Pt>
    int count() const;
    template<PieceType Pt>
    int count(Seat s) const;
    template<PieceType Pt>
    Square square(Color c) const;
    template<PieceType Pt>
    Square square(Seat s) const;           // King square for a specific seat

    // Castling
    bool   can_castle(CastlingRights cr) const;
    bool   castling_impeded(CastlingRights cr) const;
    Square castling_rook_square(CastlingRights cr) const;

    // Checking
    Bitboard checkers() const;
    Bitboard blockers_for_king(Color c) const;
    Bitboard blockers_for_king(Seat s) const;  // Per-seat blocker access
    Bitboard check_squares(PieceType pt) const;
    Bitboard pinners(Color c) const;

    // Attacks to/from a given square
    Bitboard attackers_to(Square s) const;
    Bitboard attackers_to(Square s, Bitboard occupied) const;
    bool     attackers_to_exist(Square s, Bitboard occupied, Color c) const;
    void     update_slider_blockers(Color c) const;
    template<PieceType Pt>
    Bitboard attacks_by(Color c) const;
    template<PieceType Pt>
    Bitboard attacks_by(Seat s) const;

    // Properties of moves
    bool  legal(Move m) const;
    bool  pseudo_legal(const Move m) const;
    bool  capture(Move m) const;
    bool  capture_stage(Move m) const;
    bool  gives_check(Move m) const;
    Piece moved_piece(Move m) const;
    Piece captured_piece() const;

    // Doing and undoing moves
    void do_move(Move m, StateInfo& newSt, const TranspositionTable* tt);
    void do_move(Move                      m,
                 StateInfo&                newSt,
                 bool                      givesCheck,
                 DirtyPiece&               dp,
                 DirtyThreats&             dts,
                 const TranspositionTable* tt,
                 const SharedHistories*    worker);
    void undo_move(Move m);
    void do_null_move(StateInfo& newSt);
    void undo_null_move();

    // Static Exchange Evaluation
    bool see_ge(Move m, int threshold = 0) const;

    // Accessing hash keys
    Key key() const;
    Key material_key() const;
    Key pawn_key() const;
    Key minor_piece_key() const;
    Key non_pawn_key(Color c) const;

    // Other properties of the position
    Color side_to_move() const;
    Seat  seat_to_move() const;
    int   game_ply() const;
    bool  is_chess960() const;
    bool  is_draw(int ply) const;
    bool  is_repetition(int ply) const;
    bool  upcoming_repetition(int ply) const;
    bool  has_repeated() const;
    int   rule50_count() const;
    Value non_pawn_material(Color c) const;
    Value non_pawn_material() const;

    // Position consistency check, for debugging
    bool pos_is_ok() const;
    bool material_key_is_ok() const;
    void flip();

    StateInfo* state() const;

    void put_piece(Piece pc, Square s, Seat seat, DirtyThreats* const dts = nullptr);
    void remove_piece(Square s, DirtyThreats* const dts = nullptr);
    void swap_piece(Square s, Piece pc, Seat seat, DirtyThreats* const dts = nullptr);

   private:
    // Initialization helpers
    void set_castling_right(Seat s, Square rfrom);
    Key  compute_material_key() const;
    void set_state() const;
    void set_check_info() const;

    // Other helpers
    template<bool PutPiece, bool ComputeRay = true>
    void update_piece_threats(Piece               pc,
                              Square              s,
                              DirtyThreats* const dts,
                              Bitboard            noRaysContaining = BB_ALL) const;
    void move_piece(Square from, Square to, DirtyThreats* const dts = nullptr);
    template<bool Do>
    void do_castling(Seat                seat,
                     Square              from,
                     Square&             to,
                     Square&             rfrom,
                     Square&             rto,
                     DirtyThreats* const dts = nullptr,
                     DirtyPiece* const   dp  = nullptr);
    Key  adjust_key50(Key k) const;

    // Data members
    std::array<Piece, SQUARE_NB>        board;
    std::array<Seat, SQUARE_NB>         armyOf;       // Which seat owns each square's piece
    std::array<Bitboard, PIECE_TYPE_NB> byTypeBB;
    std::array<Bitboard, COLOR_NB>      byColorBB;
    std::array<Bitboard, SEAT_NB + 1>   byArmyBB;     // Per-seat bitboards (+1 guard for SEAT_NONE)

    int          pieceCount[PIECE_NB];
    int          castlingRightsMask[SQUARE_NB];
    Square       castlingRookSquare[CASTLING_RIGHT_NB];
    Bitboard     castlingPath[CASTLING_RIGHT_NB];
    StateInfo*   st;
    int          gamePly;
    Color        sideToMove;       // Derived from seatToMove
    Seat         seatToMove_;      // Current seat's turn
    bool         chess960;
    DirtyPiece   scratch_dp;
    DirtyThreats scratch_dts;
};

std::ostream& operator<<(std::ostream& os, const Position& pos);

// =============================================================================
// INLINE IMPLEMENTATIONS
// =============================================================================

inline Color Position::side_to_move() const { return sideToMove; }
inline Seat  Position::seat_to_move() const { return seatToMove_; }

inline Piece Position::piece_on(Square s) const {
    assert(is_ok(s));
    return board[s];
}

inline Seat Position::seat_of(Square s) const {
    assert(is_ok(s));
    return armyOf[s];
}

inline const std::array<Piece, SQUARE_NB>& Position::piece_array() const { return board; }

inline bool Position::empty(Square s) const { return piece_on(s) == NO_PIECE; }

inline Piece Position::moved_piece(Move m) const { return piece_on(m.from_sq()); }

inline Bitboard Position::pieces() const { return byTypeBB[ALL_PIECES]; }

template<typename... PieceTypes>
inline Bitboard Position::pieces(PieceTypes... pts) const {
    return (byTypeBB[pts] | ...);
}

inline Bitboard Position::pieces(Color c) const { return byColorBB[c]; }

template<typename... PieceTypes>
inline Bitboard Position::pieces(Color c, PieceTypes... pts) const {
    return pieces(c) & pieces(pts...);
}

inline Bitboard Position::pieces(Seat s) const { return byArmyBB[s]; }

template<typename... PieceTypes>
inline Bitboard Position::pieces(Seat s, PieceTypes... pts) const {
    return pieces(s) & pieces(pts...);
}

template<PieceType Pt>
inline int Position::count(Color c) const {
    return pieceCount[make_piece(c, Pt)];
}

template<PieceType Pt>
inline int Position::count() const {
    return count<Pt>(WHITE) + count<Pt>(BLACK);
}

template<PieceType Pt>
inline int Position::count(Seat s) const {
    return popcount(pieces(s, Pt));
}

template<PieceType Pt>
inline Square Position::square(Color c) const {
    assert(count<Pt>(c) >= 1);
    return lsb(pieces(c, Pt));
}

template<PieceType Pt>
inline Square Position::square(Seat s) const {
    assert(count<Pt>(s) >= 1);
    return lsb(pieces(s, Pt));
}

inline Square Position::ep_square() const { return st->epSquare; }

inline bool Position::can_castle(CastlingRights cr) const { return st->castlingRights & cr; }

inline bool Position::castling_impeded(CastlingRights cr) const {
    return bool(pieces() & castlingPath[cr]);
}

inline Square Position::castling_rook_square(CastlingRights cr) const {
    return castlingRookSquare[cr];
}

inline Bitboard Position::attackers_to(Square s) const { return attackers_to(s, pieces()); }

template<PieceType Pt>
inline Bitboard Position::attacks_by(Color c) const {
    if constexpr (Pt == PAWN)
        return c == WHITE ? pawn_attacks_bb<WHITE>(pieces(WHITE, PAWN))
                          : pawn_attacks_bb<BLACK>(pieces(BLACK, PAWN));
    else
    {
        Bitboard threats   = BB_ZERO;
        Bitboard attackers = pieces(c, Pt);
        while (bool(attackers))
            threats |= attacks_bb<Pt>(pop_lsb(attackers), pieces());
        return threats;
    }
}

template<PieceType Pt>
inline Bitboard Position::attacks_by(Seat s) const {
    Color c = seat_color(s);
    if constexpr (Pt == PAWN)
        return c == WHITE ? pawn_attacks_bb<WHITE>(pieces(s, PAWN))
                          : pawn_attacks_bb<BLACK>(pieces(s, PAWN));
    else
    {
        Bitboard threats   = BB_ZERO;
        Bitboard attackers = pieces(s, Pt);
        while (bool(attackers))
            threats |= attacks_bb<Pt>(pop_lsb(attackers), pieces());
        return threats;
    }
}

inline Bitboard Position::checkers() const { return st->checkersBB; }

inline Bitboard Position::blockers_for_king(Color c) const { return st->blockersForKing[c]; }

inline Bitboard Position::blockers_for_king(Seat s) const { return st->blockersForSeatKing[s]; }

inline Bitboard Position::pinners(Color c) const { return st->pinners[c]; }

inline Bitboard Position::check_squares(PieceType pt) const { return st->checkSquares[pt]; }

inline Key Position::key() const { return adjust_key50(st->key); }

inline Key Position::adjust_key50(Key k) const {
    return st->rule50 < 14 ? k : k ^ make_key((st->rule50 - 14) / 8);
}

inline Key Position::pawn_key() const { return st->pawnKey; }

inline Key Position::material_key() const { return st->materialKey; }

inline Key Position::minor_piece_key() const { return st->minorPieceKey; }

inline Key Position::non_pawn_key(Color c) const { return st->nonPawnKey[c]; }

inline Value Position::non_pawn_material(Color c) const { return st->nonPawnMaterial[c]; }

inline Value Position::non_pawn_material() const {
    return non_pawn_material(WHITE) + non_pawn_material(BLACK);
}

inline int Position::game_ply() const { return gamePly; }

inline int Position::rule50_count() const { return st->rule50; }

inline bool Position::is_chess960() const { return chess960; }

inline bool Position::capture(Move m) const {
    assert(m.is_ok());
    return (!empty(m.to_sq()) && m.type_of() != CASTLING) || m.type_of() == EN_PASSANT;
}

inline bool Position::capture_stage(Move m) const {
    assert(m.is_ok());
    return capture(m) || m.promotion_type() == QUEEN;
}

inline Piece Position::captured_piece() const { return st->capturedPiece; }

inline void Position::put_piece(Piece pc, Square s, Seat seat, DirtyThreats* const dts) {
    board[s] = pc;
    armyOf[s] = seat;
    byTypeBB[ALL_PIECES] |= byTypeBB[type_of(pc)] |= s;
    byColorBB[color_of(pc)] |= s;
    byArmyBB[seat] |= s;
    pieceCount[pc]++;
    pieceCount[make_piece(color_of(pc), ALL_PIECES)]++;

    if (dts)
        update_piece_threats<true>(pc, s, dts);
}

inline void Position::remove_piece(Square s, DirtyThreats* const dts) {
    Piece pc   = board[s];
    Seat  seat = armyOf[s];

    if (dts)
        update_piece_threats<false>(pc, s, dts);

    byTypeBB[ALL_PIECES] ^= s;
    byTypeBB[type_of(pc)] ^= s;
    byColorBB[color_of(pc)] ^= s;
    byArmyBB[seat] ^= s;
    board[s]  = NO_PIECE;
    armyOf[s] = SEAT_NONE;
    pieceCount[pc]--;
    pieceCount[make_piece(color_of(pc), ALL_PIECES)]--;
}

inline void Position::move_piece(Square from, Square to, DirtyThreats* const dts) {
    Piece    pc     = board[from];
    Seat     seat   = armyOf[from];
    Bitboard fromTo = from | to;

    if (dts)
        update_piece_threats<false>(pc, from, dts, fromTo);

    byTypeBB[ALL_PIECES] ^= fromTo;
    byTypeBB[type_of(pc)] ^= fromTo;
    byColorBB[color_of(pc)] ^= fromTo;
    byArmyBB[seat] ^= fromTo;
    board[from] = NO_PIECE;
    board[to]   = pc;
    armyOf[from] = SEAT_NONE;
    armyOf[to]   = seat;

    if (dts)
        update_piece_threats<true>(pc, to, dts, fromTo);
}

inline void Position::swap_piece(Square s, Piece pc, Seat seat, DirtyThreats* const dts) {
    Piece old = board[s];

    remove_piece(s);

    if (dts)
        update_piece_threats<false, false>(old, s, dts);

    put_piece(pc, s, seat);

    if (dts)
        update_piece_threats<true, false>(pc, s, dts);
}

inline void Position::do_move(Move m, StateInfo& newSt, const TranspositionTable* tt = nullptr) {
    new (&scratch_dts) DirtyThreats;
    do_move(m, newSt, gives_check(m), scratch_dp, scratch_dts, tt, nullptr);
}

inline StateInfo* Position::state() const { return st; }

}  // namespace Stockfish

#endif  // #ifndef POSITION_H_INCLUDED
