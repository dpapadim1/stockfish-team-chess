// Stub - Syzygy tablebases disabled for team chess (16x8 board)
#include "tbprobe.h"
#include <string>
#include <vector>
#include <functional>

namespace Stockfish {
class Position;
namespace Search { struct RootMove; using RootMoves = std::vector<RootMove>; }
class OptionsMap;
}

namespace Stockfish::Tablebases {

int MaxCardinality = 0;

void init(const std::string&) {}

WDLScore probe_wdl(Position&, ProbeState* result) {
    *result = FAIL;
    return WDLDraw;
}

int probe_dtz(Position&, ProbeState* result) {
    *result = FAIL;
    return 0;
}

bool root_probe(Position&, Search::RootMoves&, bool, bool, const std::function<bool()>&) {
    return false;
}

bool root_probe_wdl(Position&, Search::RootMoves&, bool) {
    return false;
}

Config rank_root_moves(const OptionsMap&, Position&, Search::RootMoves&, bool, const std::function<bool()>&) {
    return Config{};
}

}
