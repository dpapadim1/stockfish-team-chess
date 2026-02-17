#ifndef NNUE_FEATURES_FULL_THREATS_INCLUDED
#define NNUE_FEATURES_FULL_THREATS_INCLUDED

#include <cstdint>
#include "../../types.h"
#include "../nnue_common.h"

namespace Stockfish { class Position; }

namespace Stockfish::Eval::NNUE::Features {
// Stub - NNUE disabled for team chess
class FullThreats {
   public:
    static constexpr const char* Name = "Full_Threats(Friend)";
    static constexpr std::uint32_t HashValue = 0x8f234cb8u;
    static constexpr IndexType Dimensions = 1;
    static IndexType make_index(Square, Square, Piece, Square) { return 0; }
};
}
#endif
