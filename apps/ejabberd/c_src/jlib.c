#include <string.h>
#include <stdbool.h>
#include <erl_nif.h>
#include <ei.h>

void save_binary(ErlNifEnv *env, unsigned char *data, int posFirst, int posAfterLast,
        ERL_NIF_TERM *binary) {
    unsigned char * buffer;
    buffer = enif_make_new_binary(env, posAfterLast-posFirst, binary);
    memcpy(buffer, data+posFirst, sizeof(unsigned char)*(posAfterLast-posFirst));
}

bool step3(ErlNifEnv *env, ErlNifBinary *inBin, int initPos, ERL_NIF_TERM *resource) {
    save_binary(env, inBin->data, initPos, inBin->size, resource);
    return 1;
}

bool step2(ErlNifEnv* env, ErlNifBinary *inBin, int initPos, ERL_NIF_TERM *server,
        ERL_NIF_TERM *resource) {
    int pos;

    if((initPos == inBin->size) || (inBin->data[initPos] == '/'))
        return 0;

    for(pos = initPos; pos < inBin->size; pos++) {
        if(inBin->data[pos] == '@')
            return 0;
        if(inBin->data[pos] == '/') {
            save_binary(env, inBin->data, initPos, pos, server);
            return step3(env, inBin, pos+1, resource);
        }
    }
    save_binary(env, inBin->data, initPos, pos, server);
    save_binary(env, inBin->data, 0, 0, resource);
    return 1;
}

bool step1(ErlNifEnv* env, ErlNifBinary *inBin, ERL_NIF_TERM *name,
        ERL_NIF_TERM *server, ERL_NIF_TERM *resource) {
    int pos;
    
    if((inBin->size == 0) || (inBin->data[0] == '@') || (inBin->data[0] == '/'))
        return 0;

    for(pos = 0; pos < inBin->size; pos++) {
        if(inBin->data[pos] == '@') {
            save_binary(env, inBin->data, 0, pos, name);
            return step2(env, inBin, pos+1, server, resource);
        }
        if(inBin->data[pos] == '/') {
            save_binary(env, inBin->data, 0, 0, name);
            save_binary(env, inBin->data, 0, pos, server);
            return step3(env, inBin, pos+1, resource);
        }
    }
    save_binary(env, inBin->data, 0, 0, name);
    save_binary(env, inBin->data, 0, pos, server);
    save_binary(env, inBin->data, 0, 0, resource);
    return 1;
}

static ERL_NIF_TERM binary_to_jid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary inBin;
    ERL_NIF_TERM name, server, resource;
    ERL_NIF_TERM errorAtom;

    if (!enif_is_binary(env, argv[0]))
        return enif_make_badarg(env);

    enif_inspect_binary(env, argv[0], &inBin);

    if(step1(env, &inBin, &name, &server, &resource)) {
        return enif_make_tuple(env, 3, name, server, resource);
    } else {
        enif_make_existing_atom(env, "error", &errorAtom, ERL_NIF_LATIN1);
        return errorAtom;
    }
}


static ErlNifFunc nif_funcs[] = {
    {"binary_to_jid0", 1, binary_to_jid}
};

ERL_NIF_INIT(jlib, nif_funcs, NULL, NULL, NULL, NULL);

