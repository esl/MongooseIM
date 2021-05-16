-module(gen_iq_component).

%% API
-export([register_iq_handler/4,
         unregister_iq_handler/3,
         stop_iq_handler/1,
         handle/5]).

-callback register_iq_handler(Domain :: jid:server(), Namespace :: binary(),
                              IQHandler :: mongoose_iq_handler:t()) -> ok.

-callback unregister_iq_handler(Domain :: jid:server(), Namespace :: binary()) -> ok.
%%====================================================================
%% API
%%====================================================================
-spec register_iq_handler(Component :: module(),
                          Domain :: jid:server(),
                          Namespace :: binary(),
                          IQHandler :: mongoose_iq_handler:t()) -> ok.
register_iq_handler(Component, Domain, Namespace, IQHandler) ->
    Component:register_iq_handler(Domain, Namespace, IQHandler).

-spec unregister_iq_handler(Component :: module(),
                            Domain :: jid:server(),
                            Namespace :: binary()) -> ok.
unregister_iq_handler(Component, Domain, Namespace) ->
    Component:unregister_iq_handler(Domain, Namespace).

-spec stop_iq_handler(IQHandler :: mongoose_iq_handler:t()) -> any().
stop_iq_handler(IQHandler) ->
    %% TODO: this function is required only for correct implementation of the legacy
    %% gen_iq_handler:remove_iq_handler/3 interface, get rid of it once gen_iq_handler
    %% module is removed.
    case mongoose_iq_handler:extra(IQHandler) of
        #{delete_on_unregister := true} ->
            mongoose_iq_handler:delete(IQHandler);
        _ ->
            ok
    end.


-spec handle(IQHandler :: mongoose_iq_handler:t(),
             Acc :: mongoose_acc:t(),
             From :: jid:jid(),
             To :: jid:jid(),
             IQ :: jlib:iq()) -> mongoose_acc:t().
handle(IQHandler, Acc, From, To, IQ) ->
    mongoose_iq_handler:process_iq(IQHandler, Acc, From, To, IQ).
