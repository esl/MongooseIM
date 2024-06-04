/**
 * @file mongoose_mam_id.cpp
 * @author konrad.zemek@erlang-solutions.com
 * @copyright 2017 Erlang Solutions Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <erl_nif.h>

#include <atomic>
#include <cstdint>

static std::atomic<std::uint_fast64_t> counter{ 0 };

extern "C" {
static ERL_NIF_TERM next_unique(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifUInt64 given_candidate;
    if (!enif_get_uint64(env, argv[0], &given_candidate))
        return enif_make_badarg(env);

    std::uint_fast64_t candidate = static_cast<std::uint_fast64_t>(given_candidate);
    std::uint_fast64_t current = candidate - 1;

    while (!counter.compare_exchange_weak(current, candidate, std::memory_order_relaxed))
        {
            if (current >= candidate)
                candidate = current + 1;
        }

    return enif_make_uint64(env, candidate);
}

static ErlNifFunc nif_funcs[] = { { "next_unique", 1, next_unique } };

ERL_NIF_INIT(mongoose_mam_id, nif_funcs, nullptr, nullptr, nullptr, nullptr)
}
