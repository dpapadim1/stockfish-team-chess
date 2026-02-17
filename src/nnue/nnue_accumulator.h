// NNUE stub for team chess — AccumulatorStack and AccumulatorCaches
#ifndef NNUE_ACCUMULATOR_H_INCLUDED
#define NNUE_ACCUMULATOR_H_INCLUDED

#include <cstddef>
#include <utility>

#include "../types.h"

namespace Stockfish::Eval::NNUE {

// Stub AccumulatorCaches — no NNUE cache needed
struct AccumulatorCaches {
    template<typename Networks>
    AccumulatorCaches(const Networks&) {}

    template<typename Networks>
    void clear(const Networks&) {}
};

// Stub AccumulatorStack — no NNUE accumulator needed
class AccumulatorStack {
   public:
    void reset() noexcept {}

    std::pair<DirtyPiece&, DirtyThreats&> push() noexcept {
        return {scratchDp, scratchDts};
    }

    void pop() noexcept {}

   private:
    DirtyPiece   scratchDp  = {};
    DirtyThreats scratchDts = {};
};

}  // namespace Stockfish::Eval::NNUE

#endif
