@echo off
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" >nul 2>&1
cl.exe /std:c++17 /EHsc /Od /Zi /GS /Fe:stockfish_debug.exe /I. main.cpp engine.cpp uci.cpp search.cpp thread.cpp timeman.cpp tt.cpp movegen.cpp movepick.cpp position.cpp bitboard.cpp evaluate.cpp tune.cpp ucioption.cpp benchmark.cpp score.cpp memory.cpp misc.cpp nnue\network.cpp nnue\nnue_misc.cpp nnue\nnue_accumulator.cpp nnue\features\half_ka_v2_hm.cpp nnue\features\full_threats.cpp syzygy\tbprobe.cpp ws2_32.lib advapi32.lib /link /STACK:8388608
echo EXIT_CODE: %ERRORLEVEL%
