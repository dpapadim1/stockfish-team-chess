// NNUE stub for team chess — Networks struct
#ifndef NETWORK_H_INCLUDED
#define NETWORK_H_INCLUDED

#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <string_view>

#include "../misc.h"
#include "../types.h"
#include "nnue_accumulator.h"
#include "nnue_misc.h"

namespace Stockfish::Eval::NNUE {

// Stub Network — provides interface but no real NNUE
template<typename ArchT, typename FeatTransT>
struct Network {
    void load(const std::string& /*dir*/, const std::string& /*file*/) {}
    void save(const std::string& /*file*/) const {}
    void verify(const std::string& /*evalFile*/,
                const std::function<void(std::string_view)>& /*onVerify*/) const {}

    std::size_t get_content_hash() const noexcept { return 0; }

    EvalFile evalFile = {};

    // Stub featureTransformer — needed by AccumulatorCaches in original code
    struct {
        int biases[1] = {};
    } featureTransformer;
};

using SmallFeatureTransformer  = int;   // stub
using SmallNetworkArchitecture = int;   // stub
using BigFeatureTransformer    = int;   // stub
using BigNetworkArchitecture   = int;   // stub

using NetworkBig   = Network<BigNetworkArchitecture, BigFeatureTransformer>;
using NetworkSmall = Network<SmallNetworkArchitecture, SmallFeatureTransformer>;

struct Networks {
    Networks() = default;
    Networks(EvalFile /*bigFile*/, EvalFile /*smallFile*/) {}

    NetworkBig   big;
    NetworkSmall small;
};

}  // namespace Stockfish::Eval::NNUE

template<typename ArchT, typename FeatTransT>
struct std::hash<Stockfish::Eval::NNUE::Network<ArchT, FeatTransT>> {
    std::size_t operator()(const Stockfish::Eval::NNUE::Network<ArchT, FeatTransT>& n)
      const noexcept {
        return n.get_content_hash();
    }
};

template<>
struct std::hash<Stockfish::Eval::NNUE::Networks> {
    std::size_t operator()(const Stockfish::Eval::NNUE::Networks& nets) const noexcept {
        std::size_t h = 0;
        Stockfish::hash_combine(h, nets.big);
        Stockfish::hash_combine(h, nets.small);
        return h;
    }
};

#endif
