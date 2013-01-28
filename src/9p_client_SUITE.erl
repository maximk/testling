-module('9p_client_SUITE').
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
-include("9p.hrl").

all() ->
	[{group,attach},{group,stat},{group,walk},
	 {group,read},{group,write},{group,dir},
 	 remove_tmp].

groups() ->
	[{attach,[],[one_attach,three_attaches]},
	 {stat,[],[stat_root,stat_tmp,stat_file]},
 	 {walk,[],[walks,clone]},
	 {read,[],[read_1,read_2]},
 	 {write,[],[write_1]},
 	 {dir,[],[read_dir]}].

init_per_suite(Config) ->
	{IP1,Port1} = ?config(loc1, Config),
	{IP2,Port2} = ?config(loc2, Config),

	L1 = {loc1,'9p_tcp',{IP1,Port1}},
	L2 = {loc2,'9p_tcp',{IP2,Port2}},

	SrvConf = [{listeners,[L1,L2]}],
	'9p_server':start_link(SrvConf),

	'9p_server':add_export(<<"z1">>, zoku_export, z1),
	'9p_server':add_export(<<"z2">>, zoku_export, z2),

	Config.

end_per_suite(_Config) ->
	case whereis('9p_server') of
	undefined ->
		ok;
	Pid ->
		exit(Pid, kill),
		ok
	end.

one_attach(Config) when is_list(Config) ->
	one_attach(diod, [<<"/">>], Config),
	one_attach(loc1, [<<"/">>], Config),
	one_attach(loc2, [<<"/">>], Config).

one_attach(Type, AttachTo, Config) ->
	ConnPid = connect(Type, AttachTo, Config),
	[{_Fid,_}] = wait_for_attached(AttachTo, ConnPid),
	terminate_wait(ConnPid).

three_attaches(Config) when is_list(Config) ->
	three_attaches(diod, [<<"/">>,<<"/tmp">>,<<"/usr">>], Config),
	three_attaches(loc1, [<<"/">>,{<<"/z1">>,<<"will">>},<<"/z2">>], Config),
	three_attaches(loc2, [<<"/">>,{<<"/z1">>,<<"will">>},<<"/z2">>], Config).

three_attaches(Type, AttachTo, Config) ->
	ConnPid = connect(Type, AttachTo, Config),
	Result = wait_for_attached(AttachTo, ConnPid),
	true = length(AttachTo) =:= length(Result),
	terminate_wait(ConnPid).

stat_root(Config) when is_list(Config) ->
	stat_root(diod, Config),
	stat_root(loc1, Config),
	stat_root(loc2, Config).

stat_root(Type, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [<<"/">>], Config),

	{ok,Attrs} = '9p':getattr(ConnPid, Fid),
	%% root is the owner
	if Type =:= diod ->
		{uid,0} = lists:keyfind(uid, 1, Attrs);
	true ->
		ok
	end,

	terminate_wait(ConnPid).

stat_tmp(Config) when is_list(Config) ->
	stat_tmp(diod, Config).

stat_tmp(Type, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [<<"/tmp">>], Config),

	{ok,Attrs} = '9p':getattr(ConnPid, Fid),
	{mode,Mode} = lists:keyfind(mode, 1, Attrs),
	true = Mode band 8#777 =:= 8#777, %% writable

	terminate_wait(ConnPid).

stat_file(Config) when is_list(Config) ->
	stat_file(diod, <<"/etc/passwd">>, Config),
	stat_file(loc1, <<"/z1/b">>, Config),
	stat_file(loc2, <<"/z1/b">>, Config).

stat_file(Type, File, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [File], Config),

	{ok,Attrs} = '9p':getattr(ConnPid, Fid),
	{size,Sz} = lists:keyfind(size, 1, Attrs),
	true = Sz > 0, %% not empty

	terminate_wait(ConnPid).

walks(Config) when is_list(Config) ->
	walks(diod, [[<<"..">>],
				 [<<"tmp">>],
				 [<<"tmp">>,<<"..">>,<<"etc">>]], Config),
	LocWalks = [[<<"..">>],
				[<<"z2">>],
				[<<"z2">>,<<"..">>]],
	walks(loc1, LocWalks, Config),
	walks(loc2, LocWalks, Config).

walks(Type, Wss, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [<<"/">>], Config),

	lists:foreach(fun(Ws) ->
		{ok,NewFid,Qids} = '9p':walk(ConnPid, Fid, Ws),
		true = length(Qids) =:= length(Ws),
		ok = '9p':clunk(ConnPid, NewFid)
	end, Wss),

	terminate_wait(ConnPid).

clone(Config) when is_list(Config) ->
	clone(diod, Config),
	clone(loc1, Config),
	clone(loc2, Config).

clone(Type, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [<<"/">>], Config),
	{ok,CloneFid,[]} = '9p':walk(ConnPid, Fid, []),
	'9p':clunk(ConnPid, CloneFid),
	'9p':clunk(ConnPid, Fid),	%% redundant, auto-clunked by terminate

	ok = '9p':terminate(ConnPid),
	receive {'EXIT',ConnPid,terminated} -> ok end.

read_1(Config) when is_list(Config) ->
	read_1(diod, <<"/etc/passwd">>, 100, Config),
	read_1(loc1, <<"/z1/b">>, 100, Config),
	read_1(loc2, <<"/z1/b">>, 100, Config).

read_1(Type, File, Count, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [File], Config),
	{ok,_,_} = '9p':open(ConnPid, Fid, [rdonly]),
	{ok,Data} = '9p':read(ConnPid, Fid, Count),
	true = size(Data) =< Count,

	terminate_wait(ConnPid).

read_2(Config) when is_list(Config) ->
	read_2(diod, <<"/etc/passwd">>, Config),
	read_2(loc1, <<"/z1/b">>, Config),
	read_2(loc2, <<"/z1/b">>, Config).

read_2(Type, File, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [File], Config),
	{ok,_,_} = '9p':open(ConnPid, Fid, [rdonly]),
	{ok,_Data} = '9p':read_all(ConnPid, Fid),
	eof = '9p':read(ConnPid, Fid),

	terminate_wait(ConnPid).

write_1(Config) when is_list(Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(diod, [<<"/tmp">>], Config),
	{ok,_,_} = '9p':create(ConnPid, Fid, <<"coocoo">>, [rdwr]),
	Data = crypto:rand_bytes(1000),
	ok ='9p':write(ConnPid, Fid, Data),

	terminate_wait(ConnPid).

read_dir(Config) when is_list(Config) ->
	read_dir(diod, [<<"tmp">>,<<"etc">>], Config),
	read_dir(loc1, [<<"z1">>,<<"z2">>], Config),
	read_dir(loc2, [<<"z1">>,<<"z2">>], Config).

read_dir(Type, Expect, Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(Type, [<<"/">>], Config),
	{ok,_,_} = '9p':open(ConnPid, Fid, [rdonly]),
	{ok,Ents} = '9p':read_dir(ConnPid, Fid),

	lists:foreach(fun(Dir) ->
		{<<?QTDIR,_/binary>>,_,_,_} = lists:keyfind(Dir, 4, Ents)
	end, Expect),

	terminate_wait(ConnPid).

remove_tmp(Config) when is_list(Config) ->
	{ConnPid,[{Fid,_}]} = connect_wait(diod, [<<"/tmp">>], Config),
	{ok,NewFid,[]} = '9p':walk(ConnPid, Fid, []),
	{ok,_,_} = '9p':create(ConnPid, NewFid, <<"coocoo">>, [rdwr]),
	ok = '9p':clunk(ConnPid, NewFid),

	{ok,Fid1,_} = '9p':walk(ConnPid, Fid, [<<"coocoo">>]),
	ok = '9p':remove(ConnPid, Fid1),

	terminate_wait(ConnPid).

%%-------- helpers ------------------------------------------------------------

ver(diod) -> '9P2000.L';
ver(_) -> '9P2000.e'.

connect(Type, AttachTo, Config) ->
	process_flag(trap_exit, true),
	EP = ?config(Type, Config),
	'9p':start_link('9p_tcp', EP,
		AttachTo,
		[{version,ver(Type)},
		 {auth_user,<<"u">>},
		 {mounter,self()}]).

connect_wait(Type, AttachTo, Config) ->
	ConnPid = connect(Type, AttachTo, Config),
	Res = wait_for_attached(AttachTo, ConnPid),
	{ConnPid,Res}.

wait_for_attached(AttachTo, ConnPid) ->
	wait_for_attached(AttachTo, ConnPid, []).

wait_for_attached([], _, Acc) ->
	Acc;
wait_for_attached(AttachTo, ConnPid, Acc) ->
	receive 
	{'9p_attached',ConnPid,Fid,AName,_Type} ->
		case lists:member(AName, AttachTo) of
		true ->
			wait_for_attached(lists:delete(AName, AttachTo),
					ConnPid, [{Fid,AName}|Acc]);
		false ->
			wait_for_attached(lists:keydelete(AName, 1, AttachTo),
					ConnPid, [{Fid,AName}|Acc])
		end
	after 3000 ->
		erlang:error(timeout)
	end.

terminate_wait(ConnPid) ->
	'9p':terminate(ConnPid),
	receive
	{'EXIT',ConnPid,terminated} ->
		ok
	end.

%%EOF
