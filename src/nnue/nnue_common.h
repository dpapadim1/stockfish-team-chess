// NNUE stub for team chess — classical evaluation only
#ifndef NNUE_COMMON_H_INCLUDED
#define NNUE_COMMON_H_INCLUDED

#include <cstdint>

namespace Stockfish::Eval::NNUE {

using IndexType = std::uint32_t;

// Stub dimension constants
constexpr IndexType TransformedFeatureDimensionsBig   = 1;
constexpr IndexType TransformedFeatureDimensionsSmall = 1;
constexpr IndexType PSQTBuckets                       = 1;
constexpr IndexType L2Big                             = 1;
constexpr IndexType L3Big                             = 1;
constexpr IndexType L2Small                           = 1;
constexpr IndexType L3Small                           = 1;

using BiasType       = std::int16_t;
using PSQTWeightType = std::int32_t;

enum class EmbeddedNNUEType { BIG, SMALL };

}  // namespace Stockfish::Eval::NNUE

#endif
