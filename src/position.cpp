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
// TEAM CHESS — Position implementation for 16×8 board, 4 armies
// =============================================================================

#include "position.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstring>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

#include "bitboard.h"
#include "misc.h"
#include "movegen.h"
#include "tt.h"
#include "uci.h"

using std::string;

namespace Stockfish {

namespace Zobrist {

Key psq[PIECE_NB][SQUARE_NB];
Key enpassant[FILE_NB];
Key castling[CASTLING_RIGHT_NB];
Key side, noPawns;
Key seat[SEAT_NB];  // Extra Zobrist keys for seat to move

}

namespace {

constexpr std::string_view PieceToChar(" PNBRQK  pnbrqk");

static constexpr Piece Pieces[] = {W_PAWN, W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
                                   B_PAWN, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING};

// Starting FEN for team chess (16×8, 4 armies)
// Rank 8: b1 + b2 back ranks | Rank 7: b1 + b2 pawns
// Rank 2: w1 + w2 pawns      | Rank 1: w1 + w2 back ranks
constexpr auto StartFEN =
  "rnbqkbnrrnbqkbnr/pppppppppppppppp/88/88/88/88/PPPPPPPPPPPPPPPP/RNBQKBNRRNBQKBNR w1 KQkqABab - 0 1";

}  // namespace


// Returns an ASCII representation of the 16×8 position
std::ostream& operator<<(std::ostream& os, const Position& pos) {

    os << "\n +";
    for (int i = 0; i < FILE_NB; i++) os << "---+";
    os << "\n";

    for (Rank r = RANK_8;; --r)
    {
        for (File f = FILE_A; f <= FILE_P; ++f)
            os << " | " << PieceToChar[pos.piece_on(make_square(f, r))];

        os << " | " << (1 + int(r)) << "\n +";
        for (int i = 0; i < FILE_NB; i++) os << "---+";
        os << "\n";

        if (r == RANK_1)
            break;
    }

    os << "  ";
    for (File f = FILE_A; f <= FILE_P; ++f)
        os << " " << char('a' + int(f)) << "  ";

    os << "\nFen: " << pos.fen()
       << "\nKey: " << std::hex << std::uppercase << std::setfill('0')
       << std::setw(16) << pos.key() << std::setfill(' ') << std::dec
       << "\nSeat: ";

    constexpr const char* seatNames[] = {"w1", "b1", "w2", "b2"};
    os << seatNames[pos.seat_to_move()];

    os << "\nCheckers: ";
    for (Bitboard b = pos.checkers(); bool(b);)
        os << UCIEngine::square(pop_lsb(b)) << " ";

    return os;
}


// Cuckoo tables for repetition detection (sized for 128-square board)
inline int H1(Key h) { return h & 0x7fff; }
inline int H2(Key h) { return (h >> 16) & 0x7fff; }

std::array<Key, 32768>  cuckoo;
std::array<Move, 32768> cuckooMove;


// Initializes Zobrist keys and cuckoo tables
void Position::init() {

    PRNG rng(1070372);

    for (Piece pc : Pieces)
        for (Square s = SQ_A1; s <= SQ_P8; ++s)
            Zobrist::psq[pc][s] = rng.rand<Key>();

    // Zero out Zobrist for pawns on promotion ranks (they can't be there)
    // W_PAWN on rank 8 (squares SQ_A8 to SQ_P8, i.e. indices 112-127)
    std::fill_n(Zobrist::psq[W_PAWN] + SQ_A8, FILE_NB, Key(0));
    // B_PAWN on rank 1 (squares SQ_A1 to SQ_P1, i.e. indices 0-15)
    std::fill_n(Zobrist::psq[B_PAWN] + SQ_A1, FILE_NB, Key(0));

    for (File f = FILE_A; f <= FILE_P; ++f)
        Zobrist::enpassant[f] = rng.rand<Key>();

    for (int cr = NO_CASTLING; cr <= ANY_CASTLING; ++cr)
        Zobrist::castling[cr] = rng.rand<Key>();

    Zobrist::side    = rng.rand<Key>();
    Zobrist::noPawns = rng.rand<Key>();

    for (int s = 0; s < SEAT_NB; ++s)
        Zobrist::seat[s] = rng.rand<Key>();

    // Prepare the cuckoo tables
    cuckoo.fill(0);
    cuckooMove.fill(Move::none());
    [[maybe_unused]] int count = 0;
    for (Piece pc : Pieces)
        for (Square s1 = SQ_A1; s1 <= SQ_P8; ++s1)
            for (Square s2 = Square(s1 + 1); s2 <= SQ_P8; ++s2)
                if ((type_of(pc) != PAWN) && bool(attacks_bb(type_of(pc), s1, BB_ZERO) & s2))
                {
                    Move move = Move(s1, s2);
                    Key  key  = Zobrist::psq[pc][s1] ^ Zobrist::psq[pc][s2] ^ Zobrist::side;
                    int  i    = H1(key);
                    while (true)
                    {
                        std::swap(cuckoo[i], key);
                        std::swap(cuckooMove[i], move);
                        if (move == Move::none())
                            break;
                        i = (i == H1(key)) ? H2(key) : H1(key);
                    }
                    count++;
                }
    // Note: cuckoo count will differ from standard chess due to 128 squares
}


// Determine which seat owns a piece based on board position (for initial setup)
// Determine seat ownership from piece color + file half.
// Left half (files a-h) = army 1, right half (files i-p) = army 2.
static Seat initial_seat(Square sq, Piece pc) {
    bool right = is_right_half(sq);
    Color c = color_of(pc);
    if (c == WHITE)
        return right ? SEAT_W2 : SEAT_W1;
    else
        return right ? SEAT_B2 : SEAT_B1;
}


// Initializes the position from a team chess FEN string.
// Team Chess FEN format:
//   1) Piece placement (16 files, 8 ranks, additive digit empty counts)
//   2) Active seat: w1/b1/w2/b2
//   3) Castling: K/Q (w1), k/q (b1), A/B (w2), a/b (b2), or '-'
//   4) En passant: standard with files a-p, or '-'
//   5) Halfmove clock
//   6) Fullmove number
Position& Position::set(const string& fenStr, bool isChess960, StateInfo* si) {

    unsigned char      col, row, token;
    size_t             idx;
    Square             sq = SQ_A8;  // Start at top-left (a8)
    std::istringstream ss(fenStr);

    std::memset(reinterpret_cast<char*>(this), 0, sizeof(Position));
    std::fill(armyOf.begin(), armyOf.end(), SEAT_NONE);
    std::memset(si, 0, sizeof(StateInfo));
    st = si;

    ss >> std::noskipws;

    // 1. Piece placement
    while ((ss >> token) && !isspace(token))
    {
        if (isdigit(token))
            sq += (token - '0') * EAST;  // Advance the given number of files

        else if (token == '/')
            sq += 2 * SOUTH;  // 16-wide: go back 16 (end of rank) + 16 (next rank) = 32 = 2*SOUTH... 
            // Actually: after processing a rank, sq is at the start of the NEXT file
            // past the end of the rank. We need to go to the start of the next lower rank.
            // After 16 files, sq is at file 16 of current rank.
            // 2 * SOUTH = -32. From rank r, file 16: sq = r*16 + 16.
            // After correction: sq = r*16 + 16 - 32 = (r-2)*16 + 16 = (r-1)*16.
            // That's the start of rank r-1. ✓

        else if (is_ok(sq) && (idx = PieceToChar.find(token)) != string::npos)
        {
            Seat seat = initial_seat(sq, Piece(idx));
            put_piece(Piece(idx), sq, seat);
            ++sq;
        }
    }

    // 2. Active seat (w1/b1/w2/b2)
    {
        string seatStr;
        ss >> std::skipws >> seatStr;
        if (seatStr == "w1")      { seatToMove_ = SEAT_W1; sideToMove = WHITE; }
        else if (seatStr == "b1") { seatToMove_ = SEAT_B1; sideToMove = BLACK; }
        else if (seatStr == "w2") { seatToMove_ = SEAT_W2; sideToMove = WHITE; }
        else if (seatStr == "b2") { seatToMove_ = SEAT_B2; sideToMove = BLACK; }
        else { seatToMove_ = SEAT_W1; sideToMove = WHITE; }  // Default
        si->seatToMove = seatToMove_;
    }

    // 3. Castling availability
    // K/Q = w1, k/q = b1, A/B = w2, a/b = b2
    {
        string castStr;
        ss >> castStr;
        for (char ch : castStr)
        {
            if (ch == '-') break;
            Square rsq;
            switch (ch) {
            case 'K': // w1 kingside
                rsq = rook_start_sq(SEAT_W1, true);
                set_castling_right(SEAT_W1, rsq);
                break;
            case 'Q': // w1 queenside
                rsq = rook_start_sq(SEAT_W1, false);
                set_castling_right(SEAT_W1, rsq);
                break;
            case 'k': // b1 kingside
                rsq = rook_start_sq(SEAT_B1, true);
                set_castling_right(SEAT_B1, rsq);
                break;
            case 'q': // b1 queenside
                rsq = rook_start_sq(SEAT_B1, false);
                set_castling_right(SEAT_B1, rsq);
                break;
            case 'A': // w2 kingside
                rsq = rook_start_sq(SEAT_W2, true);
                set_castling_right(SEAT_W2, rsq);
                break;
            case 'B': // w2 queenside
                rsq = rook_start_sq(SEAT_W2, false);
                set_castling_right(SEAT_W2, rsq);
                break;
            case 'a': // b2 kingside
                rsq = rook_start_sq(SEAT_B2, true);
                set_castling_right(SEAT_B2, rsq);
                break;
            case 'b': // b2 queenside
                rsq = rook_start_sq(SEAT_B2, false);
                set_castling_right(SEAT_B2, rsq);
                break;
            }
        }
    }

    // 4. En passant square
    bool enpassant = false;
    {
        string epStr;
        ss >> epStr;
        if (epStr != "-" && epStr.length() >= 2)
        {
            col = epStr[0];
            row = epStr[1];
            if (col >= 'a' && col <= 'p' && (row >= '1' && row <= '8'))
            {
                st->epSquare = make_square(File(col - 'a'), Rank(row - '1'));
                enpassant = true;
            }
        }
    }
    if (!enpassant)
        st->epSquare = SQ_NONE;

    // 5-6. Halfmove clock and fullmove number
    ss >> std::skipws >> st->rule50 >> gamePly;
    gamePly = std::max(4 * (gamePly - 1), 0) + int(seatToMove_);  // 4-ply per full move

    // 7. Seat ownership override (optional 7th field)
    // Format: string of digits '0'-'3', one per piece in board reading order
    // 0=SEAT_W1, 1=SEAT_B1, 2=SEAT_W2, 3=SEAT_B2
    {
        string ownerStr;
        ss >> ownerStr;
        if (!ownerStr.empty() && ownerStr[0] >= '0' && ownerStr[0] <= '3')
        {
            // Collect all occupied squares in board reading order (a8..p8, a7..p7, ...)
            std::vector<Square> occupiedSquares;
            for (Rank r = RANK_8;; --r) {
                for (File f = FILE_A; f <= FILE_P; ++f) {
                    Square s = make_square(f, r);
                    if (!empty(s))
                        occupiedSquares.push_back(s);
                }
                if (r == RANK_1) break;
            }

            size_t count = std::min(ownerStr.size(), occupiedSquares.size());
            for (size_t i = 0; i < count; i++)
            {
                int seatIdx = ownerStr[i] - '0';
                if (seatIdx < 0 || seatIdx >= int(SEAT_NB)) continue;

                Square s = occupiedSquares[i];
                Seat newSeat = Seat(seatIdx);
                Seat oldSeat = armyOf[s];

                if (newSeat != oldSeat)
                {
                    // Re-assign the piece's seat
                    byArmyBB[oldSeat] ^= s;
                    byArmyBB[newSeat] |= s;
                    armyOf[s] = newSeat;
                }
            }
        }
    }

    chess960 = isChess960;
    set_state();

    return *this;
}


// Sets a position from endgame code (simplified for team chess)
Position& Position::set(const string& code, Color c, StateInfo* si) {
    // For team chess, just use the starting position
    return set(StartFEN, false, si);
}


// Generates FEN string for the current position
string Position::fen() const {

    int                emptyCnt;
    std::ostringstream ss;

    for (Rank r = RANK_8;; --r)
    {
        for (File f = FILE_A; f <= FILE_P; ++f)
        {
            for (emptyCnt = 0; f <= FILE_P && empty(make_square(f, r)); ++f)
                ++emptyCnt;

            if (emptyCnt)
            {
                // Output empty count: use additive digits (max 8 per digit)
                while (emptyCnt > 8) { ss << '8'; emptyCnt -= 8; }
                if (emptyCnt > 0) ss << emptyCnt;
            }

            if (f <= FILE_P)
                ss << PieceToChar[piece_on(make_square(f, r))];
        }

        if (r == RANK_1)
            break;
        ss << '/';
    }

    // Active seat
    constexpr const char* seatNames[] = {"w1", "b1", "w2", "b2"};
    ss << ' ' << seatNames[seatToMove_] << ' ';

    // Castling
    bool anyCastling = false;
    if (can_castle(W1_OO))  { ss << 'K'; anyCastling = true; }
    if (can_castle(W1_OOO)) { ss << 'Q'; anyCastling = true; }
    if (can_castle(B1_OO))  { ss << 'k'; anyCastling = true; }
    if (can_castle(B1_OOO)) { ss << 'q'; anyCastling = true; }
    if (can_castle(W2_OO))  { ss << 'A'; anyCastling = true; }
    if (can_castle(W2_OOO)) { ss << 'B'; anyCastling = true; }
    if (can_castle(B2_OO))  { ss << 'a'; anyCastling = true; }
    if (can_castle(B2_OOO)) { ss << 'b'; anyCastling = true; }
    if (!anyCastling) ss << '-';

    ss << (ep_square() == SQ_NONE ? " - " : " " + UCIEngine::square(ep_square()) + " ")
       << st->rule50 << " " << 1 + gamePly / 4;  // 4 plies per full move

    // 7. Seat ownership string (one digit per piece in board reading order)
    ss << ' ';
    for (Rank r = RANK_8;; --r) {
        for (File f = FILE_A; f <= FILE_P; ++f) {
            Square s = make_square(f, r);
            if (!empty(s))
                ss << int(seat_of(s));
        }
        if (r == RANK_1) break;
    }

    return ss.str();
}


// Sets castling rights for a specific seat
void Position::set_castling_right(Seat seat, Square rfrom) {

    Color c = seat_color(seat);
    Square kfrom = square<KING>(seat);

    bool kingSide = (rfrom > kfrom);
    CastlingRights cr = kingSide ? seat_oo(seat) : seat_ooo(seat);

    st->castlingRights |= cr;
    castlingRightsMask[kfrom] |= cr;
    castlingRightsMask[rfrom] |= cr;
    castlingRookSquare[cr] = rfrom;

    // Compute castling destination squares based on seat
    // Each seat's king moves to its own g/c file equivalent
    Square kto, rto;
    if (seat == SEAT_W1 || seat == SEAT_B1) {
        // Left half: standard g1/c1 or g8/c8
        kto = relative_square(c, kingSide ? SQ_G1 : SQ_C1);
        rto = relative_square(c, kingSide ? SQ_F1 : SQ_D1);
    } else {
        // Right half: mirror on right side — o1/k1 or n1/l1 equivalent
        // w2 king on m1 castles kingside to o1 (file 14), rook to n1 (file 13)
        // w2 king on m1 castles queenside to k1 (file 10), rook to l1 (file 11)
        kto = relative_square(c, kingSide ? make_square(FILE_O, RANK_1) : make_square(FILE_K, RANK_1));
        rto = relative_square(c, kingSide ? make_square(FILE_N, RANK_1) : make_square(FILE_L, RANK_1));
    }

    castlingPath[cr] = (between_bb(rfrom, rto) | between_bb(kfrom, kto)) & ~(square_bb(kfrom) | square_bb(rfrom));
}


// Sets check info (squares that give check to the opponent's kings)
void Position::set_check_info() const {

    update_slider_blockers(WHITE);
    update_slider_blockers(BLACK);

    // Check squares against BOTH opponent kings.
    // In team chess, giving check to either enemy king is meaningful,
    // so we union the check squares from both enemy kings.
    Color them = ~sideToMove;

    // Determine both opponent seats
    Seat oppSeat1 = (them == BLACK) ? SEAT_B1 : SEAT_W1;
    Seat oppSeat2 = (them == BLACK) ? SEAT_B2 : SEAT_W2;

    // Initialize with first opponent king
    Square ksq1 = square<KING>(oppSeat1);
    st->checkSquares[PAWN]   = pawn_attacks_bb(them, ksq1);
    st->checkSquares[KNIGHT] = attacks_bb<KNIGHT>(ksq1);
    st->checkSquares[BISHOP] = attacks_bb<BISHOP>(ksq1, pieces());
    st->checkSquares[ROOK]   = attacks_bb<ROOK>(ksq1, pieces());

    // Union with second opponent king (if it exists)
    if (count<KING>(oppSeat2) > 0) {
        Square ksq2 = square<KING>(oppSeat2);
        st->checkSquares[PAWN]   |= pawn_attacks_bb(them, ksq2);
        st->checkSquares[KNIGHT] |= attacks_bb<KNIGHT>(ksq2);
        st->checkSquares[BISHOP] |= attacks_bb<BISHOP>(ksq2, pieces());
        st->checkSquares[ROOK]   |= attacks_bb<ROOK>(ksq2, pieces());
    }

    st->checkSquares[QUEEN]  = st->checkSquares[BISHOP] | st->checkSquares[ROOK];
    st->checkSquares[KING]   = BB_ZERO;
}


// Computes hash keys and other derived state
void Position::set_state() const {

    st->key               = 0;
    st->minorPieceKey     = 0;
    st->nonPawnKey[WHITE] = st->nonPawnKey[BLACK] = 0;
    st->pawnKey                                   = Zobrist::noPawns;
    st->nonPawnMaterial[WHITE] = st->nonPawnMaterial[BLACK] = VALUE_ZERO;

    Bitboard seatPieces = pieces(seatToMove_);
    Bitboard seatKings = seatPieces & pieces(KING);

    if (!bool(seatKings)) {
        st->checkersBB = BB_ZERO;
    } else {
        Square ksq = square<KING>(seatToMove_);
        st->checkersBB = attackers_to(ksq) & pieces(~sideToMove);
    }

    set_check_info();

    for (Bitboard b = pieces(); bool(b);)
    {
        Square s  = pop_lsb(b);
        Piece  pc = piece_on(s);
        st->key ^= Zobrist::psq[pc][s];

        if (type_of(pc) == PAWN)
            st->pawnKey ^= Zobrist::psq[pc][s];
        else
        {
            st->nonPawnKey[color_of(pc)] ^= Zobrist::psq[pc][s];

            if (type_of(pc) != KING)
            {
                st->nonPawnMaterial[color_of(pc)] += PieceValue[pc];

                if (type_of(pc) <= BISHOP)
                    st->minorPieceKey ^= Zobrist::psq[pc][s];
            }
        }
    }

    if (st->epSquare != SQ_NONE)
        st->key ^= Zobrist::enpassant[file_of(st->epSquare)];

    // Use seat-specific Zobrist key
    st->key ^= Zobrist::seat[seatToMove_];

    st->key ^= Zobrist::castling[st->castlingRights];
    st->materialKey = compute_material_key();
}

Key Position::compute_material_key() const {
    Key k = 0;
    for (Piece pc : Pieces)
        for (int cnt = 0; cnt < pieceCount[pc]; ++cnt)
            k ^= Zobrist::psq[pc][FILE_NB + cnt];
    return k;
}


// Computes blockers and pinners for the kings of color c
// In team chess, we must check BOTH friendly kings (both seats)
// so pins to either king are correctly detected.
void Position::update_slider_blockers(Color c) const {

    st->blockersForKing[c] = BB_ZERO;
    st->pinners[~c]        = BB_ZERO;

    // Determine the two seats for this color
    Seat seat1 = (c == WHITE) ? SEAT_W1 : SEAT_B1;
    Seat seat2 = (c == WHITE) ? SEAT_W2 : SEAT_B2;

    // Initialize per-seat blockers
    st->blockersForSeatKing[seat1] = BB_ZERO;
    st->blockersForSeatKing[seat2] = BB_ZERO;

    // Process blockers/pinners for each king of this color
    auto processKing = [&](Seat s) {
        if (count<KING>(s) == 0)
            return;
        Square ksq = square<KING>(s);

        Bitboard snipers = ((attacks_bb<ROOK>(ksq) & pieces(QUEEN, ROOK))
                            | (attacks_bb<BISHOP>(ksq) & pieces(QUEEN, BISHOP)))
                         & pieces(~c);
        Bitboard occupancy = pieces() ^ snipers;

        while (bool(snipers))
        {
            Square   sniperSq = pop_lsb(snipers);
            Bitboard b        = between_bb(ksq, sniperSq) & occupancy;

            if (bool(b) && !more_than_one(b))
            {
                st->blockersForKing[c] |= b;
                st->blockersForSeatKing[s] |= b;  // Per-seat tracking
                if (bool(b & pieces(c)))
                    st->pinners[~c] |= sniperSq;
            }
        }
    };

    processKing(seat1);
    processKing(seat2);
}


// Computes all pieces attacking a given square
Bitboard Position::attackers_to(Square s, Bitboard occupied) const {

    return (attacks_bb<ROOK>(s, occupied) & pieces(ROOK, QUEEN))
         | (attacks_bb<BISHOP>(s, occupied) & pieces(BISHOP, QUEEN))
         | (pawn_attacks_bb(BLACK, s) & pieces(WHITE, PAWN))
         | (pawn_attacks_bb(WHITE, s) & pieces(BLACK, PAWN))
         | (attacks_bb<KNIGHT>(s) & pieces(KNIGHT))
         | (attacks_bb<KING>(s) & pieces(KING));
}

bool Position::attackers_to_exist(Square s, Bitboard occupied, Color c) const {

    return (bool(attacks_bb<ROOK>(s) & pieces(c, ROOK, QUEEN))
            && bool(attacks_bb<ROOK>(s, occupied) & pieces(c, ROOK, QUEEN)))
        || (bool(attacks_bb<BISHOP>(s) & pieces(c, BISHOP, QUEEN))
            && bool(attacks_bb<BISHOP>(s, occupied) & pieces(c, BISHOP, QUEEN)))
        || bool(((pawn_attacks_bb(~c, s) & pieces(PAWN))
             | (attacks_bb<KNIGHT>(s) & pieces(KNIGHT))
             | (attacks_bb<KING>(s) & pieces(KING)))
            & pieces(c));
}


// Tests whether a pseudo-legal move is legal
bool Position::legal(Move m) const {

    assert(m.is_ok());

    Color  us   = sideToMove;
    Square from = m.from_sq();
    Square to   = m.to_sq();

    assert(color_of(moved_piece(m)) == us);

    // En passant
    if (m.type_of() == EN_PASSANT)
    {
        Square   ksq      = square<KING>(seatToMove_);
        Square   capsq    = to - pawn_push(us);
        Bitboard occupied = (pieces() ^ from ^ capsq) | to;

        assert(to == ep_square());
        assert(moved_piece(m) == make_piece(us, PAWN));
        assert(piece_on(capsq) == make_piece(~us, PAWN));
        assert(piece_on(to) == NO_PIECE);

        return !bool(attacks_bb<ROOK>(ksq, occupied) & pieces(~us, QUEEN, ROOK))
            && !bool(attacks_bb<BISHOP>(ksq, occupied) & pieces(~us, QUEEN, BISHOP));
    }

    // Castling
    if (m.type_of() == CASTLING)
    {
        Seat seat = seatToMove_;
        bool kingSide = to > from;
        Square kto;
        if (seat == SEAT_W1 || seat == SEAT_B1) {
            kto = relative_square(us, kingSide ? SQ_G1 : SQ_C1);
        } else {
            kto = relative_square(us, kingSide ? make_square(FILE_O, RANK_1) : make_square(FILE_K, RANK_1));
        }
        Direction step = kto > from ? WEST : EAST;

        for (Square s = kto; s != from; s += step)
            if (attackers_to_exist(s, pieces(), ~us))
                return false;

        return !chess960 || !bool(blockers_for_king(seatToMove_) & m.to_sq());
    }

    // King moves
    if (type_of(piece_on(from)) == KING)
        return !attackers_to_exist(to, pieces() ^ from, ~us);

    // Non-king moves: only check pins against our OWN seat's king,
    // not the teammate's king. In team chess, you can expose your
    // teammate's king — only your own king matters for legality.
    return !bool(blockers_for_king(seatToMove_) & from)
        || bool(line_bb(from, to) & square_bb(square<KING>(seatToMove_)));
}


// Tests whether a pseudo-legal move is valid
bool Position::pseudo_legal(const Move m) const {

    Color  us   = sideToMove;
    Square from = m.from_sq();
    Square to   = m.to_sq();
    Piece  pc   = moved_piece(m);

    if (m.type_of() != NORMAL)
        return bool(checkers()) ? MoveList<EVASIONS>(*this).contains(m)
                                : MoveList<NON_EVASIONS>(*this).contains(m);

    if (pc == NO_PIECE || color_of(pc) != us)
        return false;

    // Must be current seat's piece
    if (seat_of(from) != seatToMove_)
        return false;

    if (bool(pieces(us) & to))
        return false;

    if (type_of(pc) == PAWN)
    {
        if (bool((Rank8BB | Rank1BB) & to))
            return false;

        const bool isCapture    = bool(pawn_attacks_bb(us, from) & pieces(~us) & to);
        const bool isSinglePush = (from + pawn_push(us) == to) && empty(to);
        const bool isDoublePush = (from + 2 * pawn_push(us) == to)
                               && (relative_rank(us, from) == RANK_2) && empty(to)
                               && empty(to - pawn_push(us));

        if (!(isCapture || isSinglePush || isDoublePush))
            return false;
    }
    else if (!bool(attacks_bb(type_of(pc), from, pieces()) & to))
        return false;

    if (bool(checkers()))
    {
        if (type_of(pc) != KING)
        {
            if (more_than_one(checkers()))
                return false;

            if (!bool(between_bb(square<KING>(seatToMove_), lsb(checkers())) & to))
                return false;
        }
        else if (attackers_to_exist(to, pieces() ^ from, ~us))
            return false;
    }

    return true;
}


// Tests whether a pseudo-legal move gives check
bool Position::gives_check(Move m) const {

    assert(m.is_ok());
    assert(color_of(moved_piece(m)) == sideToMove);

    Square from = m.from_sq();
    Square to   = m.to_sq();

    // Direct check via check_squares (already unioned from both enemy kings)
    if (bool(check_squares(type_of(piece_on(from))) & to))
        return true;

    // Discovered check: use per-seat blockers to correctly detect when
    // a piece moves off the blocking line for a SPECIFIC enemy king.
    if (bool(blockers_for_king(~sideToMove) & from))
    {
        if (m.type_of() == CASTLING)
            return true;  // Castling always reveals discovered check when piece is a blocker

        // Check each enemy king independently
        Color them = ~sideToMove;
        Seat oppSeat1 = (them == BLACK) ? SEAT_B1 : SEAT_W1;
        Seat oppSeat2 = (them == BLACK) ? SEAT_B2 : SEAT_W2;

        if (count<KING>(oppSeat1) > 0 && bool(blockers_for_king(oppSeat1) & from)) {
            Square ksq = square<KING>(oppSeat1);
            if (!bool(line_bb(from, to) & square_bb(ksq)))
                return true;  // Discovered check on enemy king 1
        }
        if (count<KING>(oppSeat2) > 0 && bool(blockers_for_king(oppSeat2) & from)) {
            Square ksq = square<KING>(oppSeat2);
            if (!bool(line_bb(from, to) & square_bb(ksq)))
                return true;  // Discovered check on enemy king 2
        }
    }

    switch (m.type_of())
    {
    case NORMAL :
        return false;

    case PROMOTION :
        return bool(attacks_bb(m.promotion_type(), to, pieces() ^ from) & pieces(~sideToMove, KING));

    case EN_PASSANT : {
        // Check both enemy kings for discovered check via en passant
        Square   capsq = make_square(file_of(to), rank_of(from));
        Bitboard b     = (pieces() ^ from ^ capsq) | to;
        Color    them  = ~sideToMove;
        Seat oppSeat1 = (them == BLACK) ? SEAT_B1 : SEAT_W1;
        Seat oppSeat2 = (them == BLACK) ? SEAT_B2 : SEAT_W2;

        // Check enemy king 1
        if (count<KING>(oppSeat1) > 0) {
            Square ksq = square<KING>(oppSeat1);
            if (bool(attacks_bb<ROOK>(ksq, b) & pieces(sideToMove, QUEEN, ROOK))
              | bool(attacks_bb<BISHOP>(ksq, b) & pieces(sideToMove, QUEEN, BISHOP)))
                return true;
        }
        // Check enemy king 2
        if (count<KING>(oppSeat2) > 0) {
            Square ksq = square<KING>(oppSeat2);
            if (bool(attacks_bb<ROOK>(ksq, b) & pieces(sideToMove, QUEEN, ROOK))
              | bool(attacks_bb<BISHOP>(ksq, b) & pieces(sideToMove, QUEEN, BISHOP)))
                return true;
        }
        return false;
    }
    default : { // CASTLING
        Seat seat = seatToMove_;
        bool kingSide = to > from;
        Square rto;
        if (seat == SEAT_W1 || seat == SEAT_B1)
            rto = relative_square(sideToMove, kingSide ? SQ_F1 : SQ_D1);
        else
            rto = relative_square(sideToMove, kingSide ? make_square(FILE_N, RANK_1) : make_square(FILE_L, RANK_1));

        return bool(check_squares(ROOK) & rto);
    }
    }
}


// Makes a move and updates the position state
void Position::do_move(Move                      m,
                       StateInfo&                newSt,
                       bool                      givesCheck,
                       DirtyPiece&               dp,
                       DirtyThreats&             dts,
                       const TranspositionTable* tt,
                       const SharedHistories*    history) {

    assert(m.is_ok());
    assert(&newSt != st);

    Key k = st->key ^ Zobrist::seat[seatToMove_];  // Remove current seat key

    std::memcpy(&newSt, st, offsetof(StateInfo, key));
    newSt.previous = st;
    st             = &newSt;

    ++gamePly;
    ++st->rule50;
    ++st->pliesFromNull;

    Color  us       = sideToMove;
    Color  them     = ~us;
    Seat   mySeat   = seatToMove_;
    Square from     = m.from_sq();
    Square to       = m.to_sq();
    Piece  pc       = piece_on(from);
    Piece  captured = m.type_of() == EN_PASSANT ? make_piece(them, PAWN) : piece_on(to);

    dp.pc             = pc;
    dp.from           = from;
    dp.to             = to;
    dp.add_sq         = SQ_NONE;
    dts.us            = us;
    dts.prevKsq       = square<KING>(mySeat);
    dts.threatenedSqs = dts.threateningSqs = BB_ZERO;

    assert(color_of(pc) == us);
    assert(captured == NO_PIECE || color_of(captured) == (m.type_of() != CASTLING ? them : us));
    assert(type_of(captured) != KING);

    if (m.type_of() == CASTLING)
    {
        assert(pc == make_piece(us, KING));
        assert(captured == make_piece(us, ROOK));

        Square rfrom, rto;
        do_castling<true>(mySeat, from, to, rfrom, rto, &dts, &dp);

        k ^= Zobrist::psq[captured][rfrom] ^ Zobrist::psq[captured][rto];
        st->nonPawnKey[us] ^= Zobrist::psq[captured][rfrom] ^ Zobrist::psq[captured][rto];
        captured = NO_PIECE;
    }
    else if (captured)
    {
        Square capsq = to;

        if (type_of(captured) == PAWN)
        {
            if (m.type_of() == EN_PASSANT)
            {
                capsq -= pawn_push(us);

                assert(pc == make_piece(us, PAWN));
                assert(to == st->epSquare);
                assert(relative_rank(us, to) == RANK_6);
                assert(piece_on(to) == NO_PIECE);
                assert(piece_on(capsq) == make_piece(them, PAWN));

                remove_piece(capsq, &dts);
            }

            st->pawnKey ^= Zobrist::psq[captured][capsq];
        }
        else
        {
            st->nonPawnMaterial[them] -= PieceValue[captured];
            st->nonPawnKey[them] ^= Zobrist::psq[captured][capsq];

            if (type_of(captured) <= BISHOP)
                st->minorPieceKey ^= Zobrist::psq[captured][capsq];
        }

        // Record the captured piece's seat for undo
        st->capturedSeat = seat_of(capsq);

        dp.remove_pc = captured;
        dp.remove_sq = capsq;

        k ^= Zobrist::psq[captured][capsq];
        st->materialKey ^=
          Zobrist::psq[captured][FILE_NB + pieceCount[captured] - (m.type_of() != EN_PASSANT)];

        st->rule50 = 0;
    }
    else
    {
        dp.remove_sq = SQ_NONE;
        st->capturedSeat = SEAT_NONE;
    }

    k ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];

    if (st->epSquare != SQ_NONE)
    {
        k ^= Zobrist::enpassant[file_of(st->epSquare)];
        st->epSquare = SQ_NONE;
    }

    k ^= Zobrist::castling[st->castlingRights];
    st->castlingRights &= ~(castlingRightsMask[from] | castlingRightsMask[to]);
    k ^= Zobrist::castling[st->castlingRights];

    if (m.type_of() != CASTLING)
    {
        if (captured && m.type_of() != EN_PASSANT)
        {
            remove_piece(from, &dts);
            swap_piece(to, pc, mySeat, &dts);
        }
        else
            move_piece(from, to, &dts);
    }

    // Pawn special handling
    if (type_of(pc) == PAWN)
    {
        // Double pawn push: set en passant square
        // On 16-file board, double push means rank difference of 2 → sq difference of 32
        if ((int(to) ^ int(from)) == 2 * FILE_NB)  // 32
        {
            Square   epSquare = to - pawn_push(us);
            Bitboard pawns    = pawn_attacks_bb(us, epSquare) & pieces(them, PAWN);

            if (bool(pawns))
            {
                st->epSquare = epSquare;
                k ^= Zobrist::enpassant[file_of(epSquare)];
            }
        }

        else if (m.type_of() == PROMOTION)
        {
            Piece     promotion     = make_piece(us, m.promotion_type());
            PieceType promotionType = type_of(promotion);

            assert(relative_rank(us, to) == RANK_8);
            assert(type_of(promotion) >= KNIGHT && type_of(promotion) <= QUEEN);

            swap_piece(to, promotion, mySeat, &dts);

            dp.add_pc = promotion;
            dp.add_sq = to;
            dp.to     = SQ_NONE;

            k ^= Zobrist::psq[promotion][to];
            st->materialKey ^= Zobrist::psq[promotion][FILE_NB + pieceCount[promotion] - 1]
                             ^ Zobrist::psq[pc][FILE_NB + pieceCount[pc]];
            st->nonPawnKey[us] ^= Zobrist::psq[promotion][to];

            if (promotionType <= BISHOP)
                st->minorPieceKey ^= Zobrist::psq[promotion][to];

            st->nonPawnMaterial[us] += PieceValue[promotion];
        }

        st->pawnKey ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];
        st->rule50 = 0;
    }
    else
    {
        st->nonPawnKey[us] ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];

        if (type_of(pc) <= BISHOP)
            st->minorPieceKey ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];
    }

    // Advance to next seat
    seatToMove_ = next_seat(seatToMove_);
    sideToMove  = seat_color(seatToMove_);
    st->seatToMove = seatToMove_;

    // Add new seat key
    k ^= Zobrist::seat[seatToMove_];

    st->key = k;

    if (tt)
        prefetch(tt->first_entry(key()));

    st->capturedPiece = captured;

    // In 4-player team chess, the next seat's king can be in check from
    // the TEAMMATE of the seat that just moved (a persistent check from
    // a previous turn). The givesCheck shortcut from standard 2-player
    // chess is invalid here — we must ALWAYS compute checkersBB.
    if (count<KING>(seatToMove_) > 0)
        st->checkersBB = attackers_to(square<KING>(seatToMove_)) & pieces(~sideToMove);
    else
        st->checkersBB = BB_ZERO;

    set_check_info();

    // Repetition detection
    st->repetition = 0;
    int end        = std::min(st->rule50, st->pliesFromNull);
    if (end >= 4)
    {
        StateInfo* stp = st->previous->previous;
        for (int i = 4; i <= end; i += 2)
        {
            stp = stp->previous->previous;
            if (stp->key == st->key)
            {
                st->repetition = stp->repetition ? -i : i;
                break;
            }
        }
    }

    dts.ksq = square<KING>(mySeat);

    assert(dp.pc != NO_PIECE);
}


// Unmakes a move, restoring the position state
void Position::undo_move(Move m) {

    assert(m.is_ok());

    // Restore seat/side
    seatToMove_ = prev_seat(seatToMove_);
    sideToMove  = seat_color(seatToMove_);

    Color  us   = sideToMove;
    Seat   seat = seatToMove_;
    Square from = m.from_sq();
    Square to   = m.to_sq();
    Piece  pc   = piece_on(to);

    assert(empty(from) || m.type_of() == CASTLING);
    assert(type_of(st->capturedPiece) != KING);

    if (m.type_of() == PROMOTION)
    {
        assert(relative_rank(us, to) == RANK_8);
        assert(type_of(pc) == m.promotion_type());

        remove_piece(to);
        pc = make_piece(us, PAWN);
        put_piece(pc, to, seat);
    }

    if (m.type_of() == CASTLING)
    {
        Square rfrom, rto;
        do_castling<false>(seat, from, to, rfrom, rto);
    }
    else
    {
        move_piece(to, from);

        if (st->capturedPiece)
        {
            Square capsq = to;

            if (m.type_of() == EN_PASSANT)
            {
                capsq -= pawn_push(us);

                assert(type_of(pc) == PAWN);
                assert(to == st->previous->epSquare);
                assert(relative_rank(us, to) == RANK_6);
                assert(piece_on(capsq) == NO_PIECE);
                assert(st->capturedPiece == make_piece(~us, PAWN));
            }

            put_piece(st->capturedPiece, capsq, st->capturedSeat);
        }
    }

    st = st->previous;
    --gamePly;
}


// Helper template for dirty threat tracking
template<bool PutPiece>
inline void add_dirty_threat(
  DirtyThreats* const dts, Piece pc, Piece threatened, Square s, Square threatenedSq) {
    if (PutPiece)
    {
        dts->threatenedSqs |= threatenedSq;
        dts->threateningSqs |= s;
    }

    dts->list.push_back({pc, threatened, s, threatenedSq, PutPiece});
}


// Updates piece threat tables (non-AVX512 version for 128-square board)
template<bool PutPiece, bool ComputeRay>
void Position::update_piece_threats(Piece                     pc,
                                    Square                    s,
                                    DirtyThreats* const       dts,
                                    [[maybe_unused]] Bitboard noRaysContaining) const {
    const Bitboard occupied     = pieces();
    const Bitboard rookQueens   = pieces(ROOK, QUEEN);
    const Bitboard bishopQueens = pieces(BISHOP, QUEEN);
    const Bitboard knights      = pieces(KNIGHT);
    const Bitboard kings        = pieces(KING);
    const Bitboard whitePawns   = pieces(WHITE, PAWN);
    const Bitboard blackPawns   = pieces(BLACK, PAWN);

    const Bitboard rAttacks = attacks_bb<ROOK>(s, occupied);
    const Bitboard bAttacks = attacks_bb<BISHOP>(s, occupied);

    Bitboard threatened = attacks_bb(pc, s, occupied) & occupied;
    Bitboard sliders    = (rookQueens & rAttacks) | (bishopQueens & bAttacks);
    Bitboard incoming_threats =
      (PseudoAttacks[KNIGHT][s] & knights) | (pawn_attacks_bb(WHITE, s) & blackPawns)
      | (pawn_attacks_bb(BLACK, s) & whitePawns) | (PseudoAttacks[KING][s] & kings);

    while (bool(threatened))
    {
        Square threatenedSq = pop_lsb(threatened);
        Piece  threatenedPc = piece_on(threatenedSq);

        assert(threatenedSq != s);
        assert(threatenedPc);

        add_dirty_threat<PutPiece>(dts, pc, threatenedPc, s, threatenedSq);
    }

    if constexpr (ComputeRay)
    {
        while (bool(sliders))
        {
            Square sliderSq = pop_lsb(sliders);
            Piece  slider   = piece_on(sliderSq);

            const Bitboard ray        = RayPassBB[sliderSq][s] & ~BetweenBB[sliderSq][s];
            const Bitboard discovered = ray & (rAttacks | bAttacks) & occupied;

            assert(!more_than_one(discovered));
            if (bool(discovered) && (RayPassBB[sliderSq][s] & noRaysContaining) != noRaysContaining)
            {
                const Square threatenedSq = lsb(discovered);
                const Piece  threatenedPc = piece_on(threatenedSq);
                add_dirty_threat<!PutPiece>(dts, slider, threatenedPc, sliderSq, threatenedSq);
            }

            add_dirty_threat<PutPiece>(dts, slider, pc, sliderSq, s);
        }
    }
    else
    {
        incoming_threats |= sliders;
    }

    while (bool(incoming_threats))
    {
        Square srcSq = pop_lsb(incoming_threats);
        Piece  srcPc = piece_on(srcSq);

        assert(srcSq != s);
        assert(srcPc != NO_PIECE);

        add_dirty_threat<PutPiece>(dts, srcPc, pc, srcSq, s);
    }
}


// Castling helper — handles both do and undo
template<bool Do>
void Position::do_castling(Seat                seat,
                           Square              from,
                           Square&             to,
                           Square&             rfrom,
                           Square&             rto,
                           DirtyThreats* const dts,
                           DirtyPiece* const   dp) {

    Color us = seat_color(seat);
    bool kingSide = to > from;
    rfrom         = to;  // Castling is encoded as "king captures friendly rook"

    // Compute destination squares based on seat
    if (seat == SEAT_W1 || seat == SEAT_B1) {
        rto = relative_square(us, kingSide ? SQ_F1 : SQ_D1);
        to  = relative_square(us, kingSide ? SQ_G1 : SQ_C1);
    } else {
        rto = relative_square(us, kingSide ? make_square(FILE_N, RANK_1) : make_square(FILE_L, RANK_1));
        to  = relative_square(us, kingSide ? make_square(FILE_O, RANK_1) : make_square(FILE_K, RANK_1));
    }

    assert(!Do || dp);

    if (Do)
    {
        dp->to        = to;
        dp->remove_pc = dp->add_pc = make_piece(us, ROOK);
        dp->remove_sq              = rfrom;
        dp->add_sq                 = rto;
    }

    remove_piece(Do ? from : to, dts);
    remove_piece(Do ? rfrom : rto, dts);
    put_piece(make_piece(us, KING), Do ? to : from, seat, dts);
    put_piece(make_piece(us, ROOK), Do ? rto : rfrom, seat, dts);
}


// Null move — advances the seat without making a move
void Position::do_null_move(StateInfo& newSt) {

    assert(!bool(checkers()));
    assert(&newSt != st);

    std::memcpy(&newSt, st, sizeof(StateInfo));

    newSt.previous = st;
    st             = &newSt;

    if (st->epSquare != SQ_NONE)
    {
        st->key ^= Zobrist::enpassant[file_of(st->epSquare)];
        st->epSquare = SQ_NONE;
    }

    st->key ^= Zobrist::seat[seatToMove_];
    seatToMove_ = next_seat(seatToMove_);
    sideToMove  = seat_color(seatToMove_);
    st->seatToMove = seatToMove_;
    st->key ^= Zobrist::seat[seatToMove_];

    st->pliesFromNull = 0;

    // Recompute checkersBB for the new seat — in 4-player chess,
    // the next seat's king may be under attack from a teammate's pieces
    if (count<KING>(seatToMove_) > 0)
        st->checkersBB = attackers_to(square<KING>(seatToMove_)) & pieces(~sideToMove);
    else
        st->checkersBB = BB_ZERO;

    set_check_info();

    st->repetition = 0;
}


void Position::undo_null_move() {

    // In 4-player chess, the position after null move CAN have checkers
    // (from teammate's pieces), so we don't assert !checkers() here.

    st = st->previous;
    seatToMove_ = prev_seat(seatToMove_);
    sideToMove  = seat_color(seatToMove_);
}


// Static Exchange Evaluation
bool Position::see_ge(Move m, int threshold) const {

    assert(m.is_ok());

    if (m.type_of() != NORMAL)
        return VALUE_ZERO >= threshold;

    Square from = m.from_sq(), to = m.to_sq();

    assert(piece_on(from) != NO_PIECE);

    int swap = PieceValue[piece_on(to)] - threshold;
    if (swap < 0)
        return false;

    swap = PieceValue[piece_on(from)] - swap;
    if (swap <= 0)
        return true;

    assert(color_of(piece_on(from)) == sideToMove);
    Bitboard occupied  = pieces() ^ from ^ to;
    Color    stm       = sideToMove;
    Bitboard attackers = attackers_to(to, occupied);
    Bitboard stmAttackers, bb;
    int      res = 1;

    while (true)
    {
        stm = ~stm;
        attackers &= occupied;

        if (!bool(stmAttackers = attackers & pieces(stm)))
            break;

        if (bool(pinners(~stm) & occupied))
        {
            stmAttackers &= ~blockers_for_king(stm);

            if (!bool(stmAttackers))
                break;
        }

        res ^= 1;

        if (bool(bb = stmAttackers & pieces(PAWN)))
        {
            if ((swap = PawnValue - swap) < res)
                break;
            occupied ^= least_significant_square_bb(bb);
            attackers |= attacks_bb<BISHOP>(to, occupied) & pieces(BISHOP, QUEEN);
        }
        else if (bool(bb = stmAttackers & pieces(KNIGHT)))
        {
            if ((swap = KnightValue - swap) < res)
                break;
            occupied ^= least_significant_square_bb(bb);
        }
        else if (bool(bb = stmAttackers & pieces(BISHOP)))
        {
            if ((swap = BishopValue - swap) < res)
                break;
            occupied ^= least_significant_square_bb(bb);
            attackers |= attacks_bb<BISHOP>(to, occupied) & pieces(BISHOP, QUEEN);
        }
        else if (bool(bb = stmAttackers & pieces(ROOK)))
        {
            if ((swap = RookValue - swap) < res)
                break;
            occupied ^= least_significant_square_bb(bb);
            attackers |= attacks_bb<ROOK>(to, occupied) & pieces(ROOK, QUEEN);
        }
        else if (bool(bb = stmAttackers & pieces(QUEEN)))
        {
            swap = QueenValue - swap;
            assert(swap >= res);
            occupied ^= least_significant_square_bb(bb);
            attackers |= (attacks_bb<BISHOP>(to, occupied) & pieces(BISHOP, QUEEN))
                       | (attacks_bb<ROOK>(to, occupied) & pieces(ROOK, QUEEN));
        }
        else
            return (bool(attackers & ~pieces(stm))) ? res ^ 1 : res;
    }

    return bool(res);
}


// Draw detection
bool Position::is_draw(int ply) const {

    if (st->rule50 > 99 && (!bool(checkers()) || MoveList<LEGAL>(*this).size()))
        return true;

    return is_repetition(ply);
}


bool Position::is_repetition(int ply) const { return st->repetition && st->repetition < ply; }


bool Position::has_repeated() const {

    StateInfo* stc = st;
    int        end = std::min(st->rule50, st->pliesFromNull);
    while (end-- >= 4)
    {
        if (stc->repetition)
            return true;

        stc = stc->previous;
    }
    return false;
}


bool Position::upcoming_repetition(int ply) const {

    int j;

    int end = std::min(st->rule50, st->pliesFromNull);

    if (end < 3)
        return false;

    Key        originalKey = st->key;
    StateInfo* stp         = st->previous;
    Key        other       = originalKey ^ stp->key ^ Zobrist::seat[seatToMove_];

    for (int i = 3; i <= end; i += 2)
    {
        stp = stp->previous;
        other ^= stp->key ^ stp->previous->key ^ Zobrist::seat[seatToMove_];
        stp = stp->previous;

        if (other != 0)
            continue;

        Key moveKey = originalKey ^ stp->key;
        if ((j = H1(moveKey), cuckoo[j] == moveKey) || (j = H2(moveKey), cuckoo[j] == moveKey))
        {
            Move   move = cuckooMove[j];
            Square s1   = move.from_sq();
            Square s2   = move.to_sq();

            if (!bool((between_bb(s1, s2) ^ s2) & pieces()))
            {
                if (ply > i)
                    return true;

                if (stp->repetition)
                    return true;
            }
        }
    }
    return false;
}


// Flip is simplified for team chess
void Position::flip() {
    // Not meaningful for 4-army chess; stub
}


bool Position::material_key_is_ok() const { return compute_material_key() == st->materialKey; }


// Position consistency checks
bool Position::pos_is_ok() const {

    constexpr bool Fast = true;

    if ((sideToMove != WHITE && sideToMove != BLACK))
        assert(0 && "pos_is_ok: Default");

    // Check that each seat has exactly one king
    for (Seat s = SEAT_W1; s < Seat(SEAT_NB); ++s)
    {
        Color c = seat_color(s);
        Bitboard seatKings = pieces(s, KING);
        if (popcount(seatKings) != 1)
        {
            assert(0 && "pos_is_ok: Kings");
            return false;
        }
    }

    if (Fast)
        return true;

    // Pawns shouldn't be on promotion ranks
    if (bool(pieces(PAWN) & (Rank1BB | Rank8BB)))
        assert(0 && "pos_is_ok: Pawns");

    // Piece count per side: team chess allows up to 32 pieces per color (2 armies of 16)
    if (bool(pieces(WHITE) & pieces(BLACK)) || (pieces(WHITE) | pieces(BLACK)) != pieces()
        || popcount(pieces(WHITE)) > 32 || popcount(pieces(BLACK)) > 32)
        assert(0 && "pos_is_ok: Bitboards");

    // Check army bitboards consistency
    for (Seat s = SEAT_W1; s < Seat(SEAT_NB); ++s)
    {
        if (bool(pieces(s) & ~pieces(seat_color(s))))
            assert(0 && "pos_is_ok: Army/Color mismatch");
    }

    for (PieceType p1 = PAWN; p1 <= KING; ++p1)
        for (PieceType p2 = PAWN; p2 <= KING; ++p2)
            if (p1 != p2 && bool(pieces(p1) & pieces(p2)))
                assert(0 && "pos_is_ok: Bitboards");

    for (Piece pc : Pieces)
        if (pieceCount[pc] != popcount(pieces(color_of(pc), type_of(pc)))
            || pieceCount[pc] != int(std::count(board.begin(), board.end(), pc)))
            assert(0 && "pos_is_ok: Pieces");

    assert(material_key_is_ok() && "pos_is_ok: materialKey");

    return true;
}

}  // namespace Stockfish
