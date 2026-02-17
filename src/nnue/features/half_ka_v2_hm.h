#ifndef NNUE_FEATURES_HALF_KA_V2_HM_H_INCLUDED
#define NNUE_FEATURES_HALF_KA_V2_HM_H_INCLUDED

#include <cstdint>
#include "../../types.h"
#include "../nnue_common.h"

namespace Stockfish { class Position; }

namespace Stockfish::Eval::NNUE::Features {
// Stub - NNUE disabled for team chess
class HalfKAv2_hm {
   public:
    static constexpr const char* Name = "HalfKAv2_hm(Friend)";
    static constexpr std::uint32_t HashValue = 0x5f234cb8u;
    static constexpr IndexType Dimensions = 1;
    static constexpr int KingBuckets[SQUARE_NB] = {};
    static IndexType make_index(Square, Square, Piece, Square) { return 0; }
};
}
#endif
