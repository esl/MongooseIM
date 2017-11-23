-module(ejabberd_loglevel_SUITE_helper).
-compile([export_all,
          {parse_transform, lager_transform}]).

%% We have to enable parse transforms for this module,
%% otherwise we won't be able to test custom log level feature of ejabberd_loglevel.

log(_, critical, Fmt, Args) -> lager:critical(Fmt, Args);
log(_, error, Fmt, Args)    -> lager:error(Fmt, Args);
log(_, warning, Fmt, Args)  -> lager:warning(Fmt, Args);
log(_, info, Fmt, Args)     -> lager:info(Fmt, Args);
log(_, debug, Fmt, Args)    -> lager:debug(Fmt, Args).
