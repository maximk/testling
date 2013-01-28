-module(test).
-compile(export_all).

-define(REASON_DEPTH, 36).

-define(line, erlang:display(?LINE),).
-define(heap_binary_size, 64).
-define(t, test_server).

id(I) -> I.

-include_lib("kernel/include/file.hrl").

-define(FILE_MODULE, file).
-define(config,test_server:lookup_config).

%-----------------------------------------------------------------------------

all() ->
	Suites = [{lists_SUITE,[]},
			  {tuple_SUITE,[]},
			  {list_bif_SUITE,[]},
			  {binary_SUITE,[{data_dir,"binary_SUITE_data"},{skip,[deep]}]},
			  {bs_bincomp_SUITE,[]},
			  {bs_bit_binaries_SUITE,[]}
		],

	Results = lists:foldr(fun({Suite,Config}, Results) ->
		Stats = run_tests(Suite, Config),
		Result = {Suite,Stats},
		[Result|Results]
	end, [], Suites),

	{TOk,TFailed,TSkipped} =
   	lists:foldl(fun({_,{Failed,Skipped,T}}, {TOk,TFailed,TSkipped}) ->
		{TOk + T -Failed -Skipped, TFailed +Failed, TSkipped +Skipped}
	end, {0,0,0}, Results),

	io:format("\n~-25s~8s~8s~8s~8s\n", ["Suite","Total","OK","Failed","Skipped"]),
	io:format("~s\n", [lists:duplicate(25 +8 +8 +8 +8, $-)]),
	lists:foreach(fun({Suite,{Failed,Skipped,T}}) ->
		io:format("~-25s~8w~8w~8w~8w\n", [Suite,T,T -Failed -Skipped,Failed,Skipped])
	end, Results),
	GrandTotal = TOk +TFailed +TSkipped,
	io:format("~s\n", [lists:duplicate(25 +8 +8 +8 +8, $-)]),
	io:format("~-25s~8w~8w~8w~8w\n", ["TOTAL",GrandTotal,TOk,TFailed,TSkipped]),
	ok.

run_tests(Module) ->
run_tests(Module, []).

run_tests(Module, Config) ->
	process_flag(trap_exit, true),

	Started = now(),
	case Module:all() of
	{skip,Reason} ->
		io:format("Suite ~w skipped: ~w~n", [Module,Reason]),
		{0,0,0};

	AllTests ->
		NewConfig = case catch Module:init_per_suite(Config) of
			{'EXIT',_} -> Config;
			X -> X
		end,

		Result = go(AllTests, Module, NewConfig),
	   
		case Result	of
		{0,Skipped,Total} ->
			io:format("Suite ~w ok (~w total ~w skipped)~n",
									[Module,Total,Skipped]);
		{Failed,Skipped,Total} ->
			io:format("Suite ~w completes with ~w error(s) (~w total ~w skipped)~n",
									[Module,Failed,Total,Skipped])
		end,
		catch Module:end_per_suite(NewConfig),
		Elapsed = elapsed(Started, now()),
		io:format("~w millisecond(s) elapsed~n", [Elapsed]),
		Result
	end.

elapsed({Mega1,Sec1,Usec1}, {Mega2,Sec2,Usec2}) ->
	(Mega2 - Mega1) * 1000000000 +
   	(Sec2 - Sec1) * 1000 +
   	(Usec2 - Usec1) div 1000.

go(Tests, Module, Config) ->
	go(Tests, Module, Config, 0, 0, 0).

go([], _, _, Failed, Skipped, Total) ->
	{Failed,Skipped,Total};
go([{group,Group}|Tests], Module, Config, Failed, Skipped, Total) ->
	{_,_,GroupTests} = lists:keyfind(Group, 1, Module:groups()),
	go(GroupTests ++ Tests, Module, Config, Failed, Skipped, Total);
go([{group,Group,_}|Tests], Module, Config, Failed, Skipped, Total) ->
	{_,_,GroupTests} = proplists:get(Group, Module:groups()),
	go(GroupTests ++ Tests, Module, Config, Failed, Skipped, Total);
go([Test|Tests], Module, Config, Failed, Skipped, Total)
		when is_atom(Test) ->

	case go_1(Module, Test, Config) of
	{failed,Reason} ->
		case get(test_server_loc) of
		undefined -> 
			io:format("Test ~w (~w) failed: ~P~n",
				[Test,Module,Reason,?REASON_DEPTH]);
		{_Mod,Line} ->
			io:format("Test ~w (~w) failed at line ~w: ~P~n",
				[Test,Module,Line,Reason,?REASON_DEPTH]);
		X ->
			io:format("Unexpected test_server_loc: ~p~n", [X])
		end,

		go(Tests, Module, Config, Failed+1, Skipped, Total+1);

	{skipped,Reason} ->
		io:format("Test ~w (~s) skipped: ~p~n", [Test,Module,Reason]),
		go(Tests, Module, Config, Failed, Skipped+1, Total+1);

	{comment,Comment} ->
		io:format("Test ~w (~w) ok: ~s", [Test,Module,Comment]),
		go(Tests, Module, Config, Failed, Skipped, Total+1);

	_ ->	%% ok
		io:format("Test ~w (~w) ok~n", [Test,Module]),
		go(Tests, Module, Config, Failed, Skipped, Total+1)
	end.

go_1(Module, Test, Config) ->
	SkipList = proplists:get_value(skip, Config, []),
	case lists:member(Test, SkipList) of
	true ->
		{skipped,"see Config"};
	false ->
		%%io:format("Spawn ~w:~w()~n", [Module,Test]),
		Self = self(),
		Runner = erlang:spawn_link(fun() ->
			BetterConfig = case catch Module:init_per_testcase(Test, Config) of
				{'EXIT',_} -> Config;
				X -> X
			end,
			Result = Module:Test(BetterConfig),
			catch Module:end_per_testcase(Test, BetterConfig),
			Self ! {self(),Result}
		end),
		Result = go_2(Runner),
		Result
	end.

go_2(Runner) ->
	receive
	{'EXIT',_,normal} ->
		go_2(Runner);
	{'EXIT',Runner,Reason} ->
		{failed,Reason};
	{Runner,Result} ->
		Result
	end.

%%EOF
