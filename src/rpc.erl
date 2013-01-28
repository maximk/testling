-module(rpc).
-compile(export_all).

%%
%% A stub to make big_SUITE happy
%%

call('nonode@nohost', Mod, Func, Args) ->
	apply(Mod, Func, Args).

%%EOF
