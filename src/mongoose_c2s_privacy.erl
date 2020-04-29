-module(mongoose_c2s_privacy).
-author("bartlomiejgorny").
%% API
-include("mod_privacy.hrl").
-include("jlib.hrl").

-record(privacy_state, {userlist}).

-type privacy_state() :: #privacy_state{}.

-export([initialise_state/1, check_packet/4, check_packet/6]).

%% temporary
-export([get_privacy_list/1, set_privacy_list/2]).


-spec initialise_state(jid:jid()) -> privacy_state().
initialise_state(JID) ->
    UserList = mongoose_hooks:privacy_get_user_list(JID#jid.server, #userlist{}, JID#jid.user),
    #privacy_state{userlist = UserList}.

check_packet(Acc, To, Dir, StateData) ->
    Jid = ejabberd_c2s_state:jid(StateData),
    UserList = get_userlist(StateData),
    mongoose_privacy:privacy_check_packet(Acc,
                                          ejabberd_c2s_state:server(StateData),
                                          Jid#jid.user,
                                          UserList,
                                          To,
                                          Dir).

check_packet(Acc, Packet, From, To, Dir, StateData) ->
    UserList = get_userlist(StateData),
    Jid = ejabberd_c2s_state:jid(StateData),
    mongoose_privacy:privacy_check_packet({Acc, Packet},
                                          ejabberd_c2s_state:server(StateData),
                                          Jid#jid.user,
                                          UserList,
                                          From,
                                          To,
                                          Dir).

get_userlist(StateData) ->
    #privacy_state{userlist = UserList} = ejabberd_c2s_state:get_handler_state(mod_privacy, StateData),
    UserList.

%% temporary

get_privacy_list(StateData) ->
    case ejabberd_c2s_state:get_handler_state(mod_privacy, StateData) of
        empty_state -> [];
        #privacy_state{userlist = UserList} -> UserList
    end.

set_privacy_list(UserList, StateData) ->
    ejabberd_c2s_state:set_handler_state(mod_privacy, #privacy_state{userlist = UserList}, StateData).
