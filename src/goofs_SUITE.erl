-module(goofs_SUITE).
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

all() ->
	[create_delete,read_write_file,seek_rewrite,
	 append,truncate].

create_delete(Config) when is_list(Config) ->

	Names = ["file" ++ integer_to_list(N)
					|| N <- lists:seq(1, 200)],

	lists:foreach(fun(Name) ->
		ok = file:write_file(Name, <<>>)
	end, shuffle(Names)),

	lists:foreach(fun(Name) ->
		{ok,_} = file:read_file_info(Name)
	end, Names),

	lists:foreach(fun(Name) ->
		ok = file:delete(Name)
	end, Names),

	lists:foreach(fun(Name) ->
		{error,enoent} = file:read_file_info(Name)
	end, Names),

	ok = goofs:check_consistency().

read_write_file(Config) when is_list(Config) ->
	Name = "cat",

	lists:foldl(fun(_, Size) ->
		D = crypto:rand_bytes(Size),
		ok = file:write_file(Name, D),
		ok = goofs:check_consistency(),
		{ok,D} = file:read_file(Name),
		Size *2
	end, 1000, [a,a,a,a,a,a,a,a,a,a]),

	ok = goofs:check_consistency().

seek_rewrite(Config) when is_list(Config) ->
	Name = "cat",

	NumPlots = 259,
	PlotSz = 137,

	Len = NumPlots *PlotSz,
	ok = file:write_file(Name, <<0:(Len)/unit:8>>),

	Pattern = crypto:rand_bytes(PlotSz),

	{ok,F} = file:open(Name, [write]),
	lists:foreach(fun(P) ->
		{ok,_} = file:position(F, P *PlotSz),
		ok = file:write(F, Pattern)
	end, shuffle(lists:seq(0, NumPlots -1))),
	ok = file:close(F),

	Exp = list_to_binary(lists:duplicate(NumPlots, Pattern)),
	{ok,Exp} = file:read_file(Name),

	ok = goofs:check_consistency().

append(Config) when is_list(Config) ->
	Name = "cat",

	NumPlots = 255,
	PlotSz = 137,

	{ok,F} = file:open(Name, [write,append]),
	Stap = lists:foldl(fun(P, Pats) ->
		Pat = list_to_binary(lists:duplicate(PlotSz, P)),
		ok = file:write(F, Pat),
		[Pat|Pats]
	end, [], shuffle(lists:seq(0, NumPlots -1))),
	ok = file:close(F),

	Exp = list_to_binary(lists:reverse(Stap)),
	{ok,Exp} = file:read_file(Name),

	ok = goofs:check_consistency().

truncate(Config) when is_list(Config) ->
	Name = "cat",
	Size = 256000,
	Data = crypto:rand_bytes(Size),
	ok = file:write_file(Name, Data),

	chip_it(Name, Data, Size, 16000),

	ok = goofs:check_consistency().

chip_it(_, _, Sz, _) when Sz < 0 ->
	ok;

chip_it(Name, Data, Size, Step) ->

	{ok,F} = file:open(Name, [read,write]),
	{ok,_} = file:position(F, Size),
	ok = file:truncate(F),
	ok = file:close(F),

	Data1 = binary:part(Data, 0, Size),
	{ok,Data1} = file:read_file(Name),

	N = random:uniform(Step),
	chip_it(Name, Data, Size -N, Step).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% shuffle(List1) -> List2
%% Takes a list and randomly shuffles it. Relies on random:uniform
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffle(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.

%%EOF
