-ifndef(MONGOOSE_RSM_HRL).
-define(MONGOOSE_RSM_HRL, true).

-record(rsm_in, {max :: non_neg_integer() | undefined | error,
                 index :: non_neg_integer() | undefined | error,
                 direction :: before | aft | undefined,
                 id :: binary() | integer() | undefined %% id is empty, if cdata does not exist
                }).

-record(rsm_out, {count :: non_neg_integer() | undefined,
                  index :: non_neg_integer() | undefined,
                  first :: binary() | undefined,
                  last  :: binary() | undefined
                 }).

-endif.
