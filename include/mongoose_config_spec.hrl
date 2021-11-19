-ifndef(MONGOOSEIM_CONFIG_SPEC_HRL).
-define(MONGOOSEIM_CONFIG_SPEC_HRL, true).

-record(section, {items :: #{mongoose_config_parser_toml:toml_key() | default =>
                                 mongoose_config_spec:config_node()},
                  validate_keys = any :: mongoose_config_validator:validator(),
                  required = [] :: [mongoose_config_parser_toml:toml_key()] | all,
                  validate = any :: mongoose_config_validator:section_validator(),
                  process :: undefined | mongoose_config_parser_toml:list_processor(),
                  format = default :: mongoose_config_spec:format(),
                  defaults = #{} :: #{mongoose_config_parser_toml:toml_key() =>
                                         mongoose_config_parser_toml:config_part()}}).

-record(list, {items :: mongoose_config_spec:config_node(),
               validate = any :: mongoose_config_validator:list_validator(),
               process :: undefined | mongoose_config_parser_toml:list_processor(),
               format = default :: mongoose_config_spec:format()}).

-record(option, {type :: mongoose_config_spec:option_type(),
                 validate = any :: mongoose_config_validator:validator(),
                 process :: undefined | mongoose_config_parser_toml:processor(),
                 format = default :: mongoose_config_spec:format()}).

-endif.
