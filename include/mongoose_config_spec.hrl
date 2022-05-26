-ifndef(MONGOOSEIM_CONFIG_SPEC_HRL).
-define(MONGOOSEIM_CONFIG_SPEC_HRL, true).

-record(section, {items = #{} :: #{mongoose_config_parser_toml:toml_key() | default =>
                                       mongoose_config_spec:config_node()},
                  validate_keys = any :: mongoose_config_validator:validator(),
                  required = [] :: [mongoose_config_parser_toml:toml_key()] | all,
                  validate = any :: mongoose_config_validator:section_validator(),
                  format_items = map :: mongoose_config_spec:format_items(),
                  process :: undefined | mongoose_config_parser_toml:list_processor(),
                  defaults = #{} :: #{mongoose_config_parser_toml:toml_key() =>
                                         mongoose_config_parser_toml:config_part()},
                  wrap = default :: mongoose_config_spec:wrapper(),
                  include = when_present :: always | when_present}).

-record(list, {items :: mongoose_config_spec:config_node(),
               validate = any :: mongoose_config_validator:list_validator(),
               format_items = list :: mongoose_config_spec:format_items(),
               process :: undefined | mongoose_config_parser_toml:list_processor(),
               wrap = default :: mongoose_config_spec:wrapper()}).

-record(option, {type :: mongoose_config_spec:option_type(),
                 validate = any :: mongoose_config_validator:validator(),
                 process :: undefined | mongoose_config_parser_toml:processor(),
                 wrap = default :: mongoose_config_spec:wrapper()}).

-endif.
