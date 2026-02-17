# Stockfish - Team Chess (4-Player, 16x8 Board)

A modified version of [Stockfish](https://github.com/official-stockfish/Stockfish), adapted for **4-player team chess** (2v2) on a 16x8 board.

Licensed under **GPLv3**, same as upstream Stockfish.

## What is Team Chess?

Team Chess is a chess variant where four players (two teams of two) play on a **16-file x 8-rank board**. Each team has two full sets of pieces placed side by side. Players take turns in order: White1, Black1, White2, Black2. A team wins by checkmating **either** enemy king.

`
  a b c d e f g h i j k l m n o p
8 r n b q k b n r r n b q k b n r   <- Black (seats B1 + B2)
7 p p p p p p p p p p p p p p p p
6 . . . . . . . . . . . . . . . .
5 . . . . . . . . . . . . . . . .
4 . . . . . . . . . . . . . . . .
3 . . . . . . . . . . . . . . . .
2 P P P P P P P P P P P P P P P P
1 R N B Q K B N R R N B Q K B N R   <- White (seats W1 + W2)
  a b c d e f g h i j k l m n o p
`

## Key Modifications from Upstream Stockfish

### Board and Bitboard
- **128-square board** (16 files x 8 ranks) using dual 64-bit bitboard pairs (lo/hi)
- All bitboard operations adapted: shifts, pop_lsb, popcount, square iteration
- No magic bitboards - direct ray-based slider attack generation

### 4-Seat Game Logic
- **4 seats** (W1, B1, W2, B2) with round-robin turn order
- Custom **FEN format** with 7th field encoding piece ownership (which seat owns each piece)
- Each player can only move their own pieces (not their teammate's)
- Castling rights extended for all four rooks per side (KQkqABab)

### Move Generation and Legality
- checkersBB always recomputed in do_move() - teammate's pieces can give persistent check
- Per-seat blocker/pinner tracking (blockersForSeatKing[])
- Pin detection uses seat-specific king, not just color-level king
- gives_check() tests against **both** enemy kings

### Evaluation
- **Classical evaluation** tuned for 4-player team chess:
  - King safety assessed for all 4 kings
  - Persistent check penalty (huge penalty when a king is under direct fire)
  - Concentrated attack bonuses (focus firepower on the weaker enemy king)
  - Team-aware piece coordination
- **Simplified NNUE** (optional, loads at runtime):
  - Architecture: 3,072 -> 64 -> 32 -> 1
  - Features: SeatPieceSquare (4 seats x 6 piece types x 128 squares)
  - Blended 60% NNUE + 40% classical when weights are loaded
  - Weights loaded via `loadnnue <path>` UCI command

### Search
- Null-move correctly recomputes checkersBB for next seat
- No invalid assertions for 4-player (e.g., undo_null_move checker assert removed)

## Building

### Windows (MSVC)
```bat
cd src
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
cl.exe /std:c++17 /EHsc /O2 /Fe:stockfish_team.exe /I. ^
  main.cpp engine.cpp uci.cpp search.cpp thread.cpp timeman.cpp tt.cpp ^
  movegen.cpp movepick.cpp position.cpp bitboard.cpp evaluate.cpp ^
  tune.cpp ucioption.cpp benchmark.cpp score.cpp memory.cpp misc.cpp ^
  nnue\network.cpp nnue\nnue_misc.cpp nnue\nnue_accumulator.cpp ^
  nnue\features\half_ka_v2_hm.cpp nnue\features\full_threats.cpp ^
  syzygy\tbprobe.cpp ws2_32.lib advapi32.lib
```

## UCI Commands

Standard Stockfish UCI protocol, plus:
- `loadnnue <path>` - Load Team Chess NNUE weights from a binary file

## Custom FEN Format

```
rnbqkbnrrnbqkbnr/pppppppppppppppp/88/88/88/88/PPPPPPPPPPPPPPPP/RNBQKBNRRNBQKBNR w1 KQkqABab - 0 1 1111111133333333111111113333333300000000222222220000000022222222
```

The 7th field encodes piece ownership: 0=W1, 1=B1, 2=W2, 3=B2, listed in board order (a8 to p8, a7 to p7, ..., a1 to p1), one digit per piece.

## Credits

- Original [Stockfish](https://github.com/official-stockfish/Stockfish) by the Stockfish developers
- Team Chess modifications by [dpapadim1](https://github.com/dpapadim1)

## License

Stockfish is free software, and distributed under the **GNU General Public License version 3** (GPLv3). See [Copying.txt](Copying.txt) for the full license text.
