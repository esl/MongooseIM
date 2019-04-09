In order for MongooseIM nodes to communicate with each other, they have to share a common secret - i.e. a cookie - which is a feature of the underlying Erlang VM.
The cookie itself is an UTF8 string that is up to 255 characters in size.
Thanks to the cookie, MongooseIM nodes can determine if they are allowed to communicate with each other and with no cookie no communication would flow between the nodes - a feature especially useful when you are running more than one applications on a single machine. 

For ease of deployment and staging, each MongooseIM node is configured with a predefined erlang cookie.
However, one should remember that for production environments this cookie should be reconfigured to a new secret cookie, as this will secure your system from intrusion.
You can change the cookie by changing the parameters of the `-setcookie` parameter in the `vm.args` file.

Nonetheless, one should remember that communication between Erlang nodes is unencrypted by default, hence, the cookie is vulnerable to sniffing.
If one has access to a MongooseIM cookie and figures out the hostname of a node, one can execute shell commands remotely on that node.
Therefore, one should either provide privacy at the network layer (strongly recommended) or disable port 4369 for ultimate security.

