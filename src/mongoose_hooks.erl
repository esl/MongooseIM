-module(mongoose_hooks).

-export([privacy_check_packet/6]).

-spec privacy_check_packet(LServer, Acc, User, PrivacyList, FromToNameType, Dir) -> Result when
      LServer :: jid:lserver(), Acc :: mongoose_acc:t(), User :: jid:luser(),
      PrivacyList :: mongoose_privacy:userlist(),
      FromToNameType :: {jid:jid(), jid:jid(), binary(), binary()},
      Dir :: in | out,
      Result :: mongoose_acc:t().
privacy_check_packet(LServer, Acc, User, PrivacyList, FromToNameType, Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
                            LServer,
                            mongoose_acc:set(hook, result, allow, Acc),
                            [User,
                             LServer,
                             PrivacyList,
                             FromToNameType,
                             Dir]).
