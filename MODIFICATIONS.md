# Modifications to Stockfish for Team Chess

This directory contains a modified version of Stockfish adapted for
4-player team chess on a 16×8 board.

**Original:** [official-stockfish/Stockfish](https://github.com/official-stockfish/Stockfish)  
**Modified:** [dpapadim1/stockfish-team-chess](https://github.com/dpapadim1/stockfish-team-chess)  
**License:** GNU General Public License v3.0 (see Copying.txt)

All modifications are the work of dpapadim1, first published 2025.
Original `.original` backups are retained alongside each modified file
as diff references.

---

## Modified Files

### src/types.h
- Extended board dimensions from 8×8 to 16×8 (128 squares)
- Added 4-seat turn system: `W1, B1, W2, B2`
- Added 128-bit bitboard type (`Bitboard128`) for extended board
- Added per-seat piece ownership encoding

### src/bitboard.h / src/bitboard.cpp
- Replaced 64-bit bitboard operations with 128-bit equivalents
- Updated attack tables, ray calculations, and magic bitboards for 16×8 geometry
- Updated `Square`, `File`, `Rank` mappings for 16 columns × 8 rows

### src/position.h / src/position.cpp  *(position.cpp is new; no .original)*
- Added 4-seat position state (`sideToMove` cycles W1→B1→W2→B2)
- Per-seat king tracking (each seat has its own king)
- Team-checkmate logic: either opposing king in checkmate ends the game
- Per-seat pin detection (players protect only their own king)
- Custom FEN parser with single-digit empty-square encoding

### src/movegen.h / src/movegen.cpp
- Updated legal move generation to respect 4-seat turn order
- Kings only consider checks by opposing team pieces
- En passant and castling adapted for extended board coordinates

### src/evaluate.h / src/evaluate.cpp
- Evaluation tuned for team chess: piece coordination between team members
- Threat detection across both opponent kings

### src/search.h / src/search.cpp
- Root search respects 4-seat turn cycling
- Mate detection triggers on either opposing king

### src/uci.h / src/uci.cpp
- Added custom UCI commands:
  - `getfen` — returns current position as team chess FEN
  - `geteval` — returns static evaluation of current position
- Updated `position` command to parse team chess FEN format

### src/tt.h / src/tt.cpp
- Transposition table keys updated for 128-square board hashing

### src/movepick.h / src/movepick.cpp
- Move ordering updated for team chess piece values and board geometry

### src/history.h
- History heuristics updated for 16×8 board indexing

### src/nnue/ (multiple files)
- NNUE input features adapted for 16×8 board (128 input squares)
- Custom network `team_chess_nnue.bin` trained on team chess self-play data
- Modified: `network.h`, `network.cpp`, `nnue_accumulator.h`, `nnue_accumulator.cpp`,
  `nnue_architecture.h`, `nnue_common.h`, `nnue_feature_transformer.h`,
  `nnue_misc.h`, `nnue_misc.cpp`, `simd.h`
- Modified features: `full_threats.h`, `full_threats.cpp`,
  `half_ka_v2_hm.h`, `half_ka_v2_hm.cpp`

### src/syzygy/tbprobe.cpp
- Updated square/piece encoding to match 128-square board layout

---

## Added Files

| File | Purpose |
|------|---------|
| `src/engine.cpp` | New entry point wiring engine to team chess rules |
| `src/engine.h` | Engine API for team chess (new file) |
| `src/score.cpp` | Score display helpers for team chess |
| `src/score.h` | Score type declarations |
| `src/shm.h` / `src/shm_linux.h` | Shared memory helpers for multi-process use |
| `src/team_chess_nnue.bin` | NNUE weights trained on team chess self-play |
| `src/build.bat` | Windows build script |
| `src/build_debug.bat` | Windows debug build script |
| Various `*.bat` test scripts | Engine testing and validation scripts |
