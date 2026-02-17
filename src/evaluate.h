/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2026 The Stockfish developers

  Team Chess variant — classical evaluation (no NNUE)
*/

#ifndef EVALUATE_H_INCLUDED
#define EVALUATE_H_INCLUDED

#include <string>
#include "types.h"

namespace Stockfish {

class Position;

namespace Eval {

// Simple material evaluation
int simple_eval(const Position& pos);

// Full classical evaluation — returns score from the side-to-move perspective
Value evaluate(const Position& pos, int optimism = 0);

// Trace evaluation for debugging
std::string trace(const Position& pos);

}  // namespace Eval

}  // namespace Stockfish

#endif  // #ifndef EVALUATE_H_INCLUDED
