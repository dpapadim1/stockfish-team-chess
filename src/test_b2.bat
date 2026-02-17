@echo off
REM Simulate: b1 plays first, then b2 plays (same process, sequential)
REM Position after w1: e2->e4 (for b1's turn)
REM Then position after w1: e2->e4, b1: d7->d5, w2: k2->k4 (for b2's turn)
(
echo uci
echo isready
echo ucinewgame
echo isready
echo position fen rnbqkbnrrnbqkbnr/pppppppppppppppp/88/88/4P83/88/PPPP1PPPPPPPPPPP/RNBQKBNRRNBQKBNR b1 KQkqABab e3 0 1
echo go depth 5
timeout /T 3 /NOBREAK >NUL
echo ucinewgame
echo isready
echo position fen rnbqkbnrrnbqkbnr/ppp1pppppppppppp/88/3pP3P83/88/88/PPPP1PPPP1PPPPPP/RNBQKBNRRNBQKBNR b2 KQkqABab k3 0 1
echo go depth 5
timeout /T 5 /NOBREAK >NUL
echo quit
) | stockfish_team.exe
echo EXITCODE: %ERRORLEVEL%
