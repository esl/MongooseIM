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
                    // If we found a slash first, we don't care about commercial_ats anymore
                    // https://tools.ietf.org/html/rfc7622#section-3.2
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
    std::memcpy(host_data, &(bin.data[commercial_at + 1]), host_size);

    ERL_NIF_TERM resource;
    unsigned res_size = slash >= size - 1 ? 0 : size - slash - 1;
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


static ERL_NIF_TERM
to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1)
        return enif_make_badarg(env);
    if (enif_is_binary(env, argv[0]))
        return argv[0];

    int arity;
    const ERL_NIF_TERM *tuple;
    if (!enif_get_tuple(env, argv[0], &arity, &tuple))
        return enif_make_badarg(env);

    ErlNifBinary user = {};
    ErlNifBinary server = {};
    ErlNifBinary resource = {};
    switch(arity) {
        case 7: // It is a #jid record, so from
                // {jid, User, Server, Resource, LUser, LServer, LResource}
                // extract fields 1, 2, and 3 (U, S, and R, it's 0-indexed)
            if (!enif_inspect_binary(env, tuple[1], &user)
                    || !enif_inspect_binary(env, tuple[2], &server)
                    || !enif_inspect_binary(env, tuple[3], &resource))
                return enif_make_badarg(env);
            break;
        case 2: // {User, Server}
            if (!enif_inspect_binary(env, tuple[0], &user)
                    || !enif_inspect_binary(env, tuple[1], &server))
                return enif_make_badarg(env);
            break;
        case 3: // {User, Server, Resource}
            if (!enif_inspect_binary(env, tuple[0], &user)
                    || !enif_inspect_binary(env, tuple[1], &server)
                    || !enif_inspect_binary(env, tuple[2], &resource))
                return enif_make_badarg(env);
            break;
        default:
            return enif_make_badarg(env);
    }
    // Process those three binaries
    unsigned index = 0;
    unsigned len =
        server.size + // there's always a server
        (user.size == 0 ? 0 : user.size + 1) + // user plus commercial_at, if any
        (resource.size == 0 ? 0 : resource.size + 1); // res plus slash, if any

    ERL_NIF_TERM final_jid;
    unsigned char *final_jid_data = enif_make_new_binary(env, len, &final_jid);
    std::memset(final_jid_data, 0, len);
    // copy the user field and the commercial_at if there's a user
    if (user.size != 0) {
        std::memcpy(final_jid_data, user.data, user.size);
        std::memcpy(final_jid_data + user.size, "@", 1);
        index += user.size + 1;
    }
    // always copy the server field
    std::memcpy(final_jid_data + index, server.data, server.size);
    index += server.size;
    // copy the slash and the resource field if there's a resource
    if (resource.size != 0) {
        std::memcpy(final_jid_data + index, "/", 1);
        std::memcpy(final_jid_data + index + 1, resource.data, resource.size);
    }
    return final_jid;
}

static ErlNifFunc jid_nif_funcs[] = {
    {"to_binary", 1, to_binary},
    {"from_binary_nif", 1, from_binary_nif}
};

ERL_NIF_INIT(jid, jid_nif_funcs, NULL, NULL, NULL, NULL);
