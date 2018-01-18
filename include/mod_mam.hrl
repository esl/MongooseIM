-ifndef(MOD_MAM_HRL).
-define(MOD_MAM_HRL, true).

-record(mam_borders, {after_id  :: non_neg_integer() | undefined,
                      before_id :: non_neg_integer() | undefined,
                      from_id   :: non_neg_integer() | undefined,
                      to_id     :: non_neg_integer() | undefined
                     }).

-endif.
