## Module Description

This module limits message passing between domains.

## Options

### `modules.mod_domain_isolation.extra_domains`

Allows to specify more domains, the rules are applied to.
No filtering is done to subdomains, unless they are specified in the `extra_domains` list.

 * **Syntax:** a list of domain templates (as lists)
 * **Default:** `[]`
 * **Example:** `extra_domains = ["muclight.@HOST@"]`
