-module(zlib_driver_SUITE).

-compile(export_all).

all()->
    [stanza_1024].

stanza_1024(_)->
    DeflateCmd = 1,
    InflateCmd = 2,
    %% Just random 1024 bytes data
    Data = <<"iGzHvZgQbX6oTJYViq9Xy3MncZtDlypeFWNF61owolz8boGSqsr5wxtfT42QSJIMShM6MZInMHBUh9unSYpuwqfzlLIZ3ZNfjje7YsLhaITQzXXNABsXdvjUMU437FJppLeLVob9M4ZcUpAOdMSpCB1t6KKgWBmNEdkbo9VL2seDdpYT3AcZlGOsy73pk7og3KirclNJM6NFI2wARifUdE9ShEJhlncjOQnG3ExCnjGsRSGcJl8eh6Vjsq8pavfhOTYyE6z4xBPG4jKtZCiqVO1uaWQjeTOPZnhPdnVThcrpVS1GiB33xZz9p6pRw2ricGbsbZoUNIQQqBsL5VGZeNQhRLAoiNsT1S3kJl43Mm6rvIy8rM5Jm9y4NXvkZcjgPpmfySmgXSygWshbjGbaUlwub2ZCe7C5Z1vQL7n5DutDefQcFXJScMWiOW11ye9UIx9u29qVp2HhzhJ1dtAheBBk6unVNTVkQJoyMcdVAFSQ4yob66WKm33xkrtxgvyAFesdWwXFc6VNGxtXjBYu2Hd8jyDSAzl9wK3CRF9rraKSLSv3ycTFty16noELhJwzQQ2zjkQerms7MR98nmmGFLA1DgaZXSwQnWl4A3k8mTvxsWfhVgZDdkcvwqmFB3JHJsKfUuakt921dJ3uXI5ywsV3nVi3Skpuj3OiV9mfZzNp2J8mE7zxX5RtgV3phcfIEA52gBb8TngbL8hnPPkJy1tTaYYtNCcZEHaLd8XKUPa2aAr7rMdLzqhiaGPY6JWIncDWKGW8EOCOXXKVIgXtFEhAjZlc6e7LSOvkBbJTkh3hoS9Ow9daLJrnIGIMDRT1NxhHX9iYzvUAs9c78zZjTA7CWFWRhj5Ui7koQrMXzIszmVDTGe3y5Ml6bNMMUtZUT2eK7QSnU4wFwFqeviN56XN4qugVMW9J6YbVY5bZA77fpUW92TUHXA2jhXldNpFL8cc82p5XNNQERaqBDA15EHuotqaVDZGCYPkvPLqXlx9UjOLJltDAAKYlifQQonPW">>,

    case erl_ddll:load_driver(filename:join(code:priv_dir(mongooseim), "lib"), ejabberd_zlib_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> error(erl_ddll:format_error(ErrorDesc))
    end,

    Port = open_port({spawn, ejabberd_zlib_drv}, [binary]),

    <<0, Deflated/binary>> = port_control(Port, DeflateCmd, Data),
    <<0, Data/binary>> = port_control(Port, InflateCmd, Deflated),

    ok.
