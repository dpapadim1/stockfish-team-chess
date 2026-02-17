@echo off
echo quit | "c:\Users\Dimitris\git_team_chess\team_chess\Stockfish\src\stockfish_team.exe" >stdout.txt 2>stderr.txt
echo EXITCODE=%ERRORLEVEL%
