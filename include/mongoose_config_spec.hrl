-ifndef(MONGOOSEIM_CONFIG_SPEC_HRL).
-define(MONGOOSEIM_CONFIG_SPEC_HRL, true).

-record(section, {items,
                  validate_keys = any,
                  required = [],
                  validate = any,
                  process,
                  format = default}).
-record(list, {items,
               validate = any,
               process,
               format = default}).
-record(option, {type,
                 validate = any,
                 process,
                 format = default}).

-endif.
