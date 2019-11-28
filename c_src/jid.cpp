#include "erl_nif.h"
#include <cstring>

ERL_NIF_TERM
mk_error(ErlNifEnv* env)
{
    ERL_NIF_TERM ret;
    enif_make_existing_atom(env, "error", &ret, ERL_NIF_LATIN1);
    return ret;
}

static ERL_NIF_TERM
from_binary_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    const unsigned size = bin.size;
    unsigned commercial_at = -1;
    unsigned slash = size;
    for (unsigned i = 0; i < size ; ++i) {
        switch(bin.data[i]) {
            case '/':
                if (slash == size) {
                    slash = i;
                    goto end_loop;
                }
                break;
            case '@':
                if (commercial_at == -1) {
                    commercial_at = i;
                } else
                    return mk_error(env);
                break;
        }
    }
end_loop:
    if (commercial_at == 0 || slash == 0) {
        return mk_error(env);
    }

    unsigned host_size = slash - commercial_at - 1;
    if (host_size == 0) return mk_error(env);
    ERL_NIF_TERM host;
    unsigned char *host_data = enif_make_new_binary(env, host_size, &host);
    std::memcpy(host_data, &(bin.data[commercial_at+1]), host_size);

    ERL_NIF_TERM resource;
    unsigned res_size = slash >= size-1 ? 0 : size-1-slash;
    unsigned char *res_data = enif_make_new_binary(env, res_size, &resource);
    std::memcpy(res_data, &(bin.data[slash + 1]), res_size);

    ERL_NIF_TERM user;
    unsigned user_size = commercial_at == -1 ? 0 : commercial_at;
    unsigned char *user_data = enif_make_new_binary(env, user_size, &user);
    std::memcpy(user_data, &(bin.data[0]), user_size);

    return enif_make_tuple3(
            env,
            user,
            host,
            resource);
}

static ErlNifFunc jid_nif_funcs[] = {
    {"from_binary_nif", 1, from_binary_nif}
};

ERL_NIF_INIT(jid, jid_nif_funcs, NULL, NULL, NULL, NULL);
