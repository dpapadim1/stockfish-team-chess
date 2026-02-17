@echo off
echo uci > c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt
echo isready >> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt
echo position startpos >> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt
echo go depth 3 >> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt
echo quit >> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt
c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\stockfish_team.exe < c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\uci_cmds.txt > c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\test_stdout.txt 2> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\test_stderr.txt
echo EXITCODE: %ERRORLEVEL%
