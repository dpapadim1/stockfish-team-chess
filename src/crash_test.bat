@echo off
c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\stockfish_team.exe < c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\crash_test.txt > c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\crash_stdout.txt 2> c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\crash_stderr.txt
echo EXITCODE: %ERRORLEVEL%
