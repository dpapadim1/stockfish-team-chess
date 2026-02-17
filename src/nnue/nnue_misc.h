// NNUE stub for team chess — EvalFile and misc types
#ifndef NNUE_MISC_H_INCLUDED
#define NNUE_MISC_H_INCLUDED

#include <cstddef>
#include <string>

#include "../misc.h"
#include "../types.h"

// Default net names — unused, but required by UCI options in engine.cpp
#define EvalFileDefaultNameBig   "none"
#define EvalFileDefaultNameSmall "none"

namespace Stockfish {

class Position;

namespace Eval::NNUE {

struct EvalFile {
    // Trivially-copyable stub (no std::string so Networks stays trivially-destructible
    // for SystemWideSharedConstant in shm.h)
    char defaultName[64]  = {};
    char current[64]      = {};
    char netDescription[64] = {};
};

struct NnueEvalTrace {};

struct Networks;
struct AccumulatorCaches;

inline std::string trace(Position& /*pos*/, const Networks& /*networks*/,
                         AccumulatorCaches& /*caches*/) {
    return "NNUE disabled — classical evaluation only\n";
}

}  // namespace Eval::NNUE
}  // namespace Stockfish

template<>
struct std::hash<Stockfish::Eval::NNUE::EvalFile> {
    std::size_t operator()(const Stockfish::Eval::NNUE::EvalFile& ef) const noexcept {
        std::size_t h = 0;
        Stockfish::hash_combine(h, std::string(ef.defaultName));
        Stockfish::hash_combine(h, std::string(ef.current));
        return h;
    }
};

#endif
