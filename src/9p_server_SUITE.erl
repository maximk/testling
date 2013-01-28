-module('9p_server_SUITE').
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
-include("9p.hrl").

-define(VER_E, <<"9P2000.e">>).
-define(VER_U, <<"9P2000.u">>).

-define(STD_SESSION_KEY, <<3,1,4,1,5,9,2,6>>).

all() ->
	[version,{group,session},{group,proto_error},
	 {group,auth},{group,attach},{group,walk},create,
	 {group,read_write},remove,
	 {group,stat},{group,times},{group,pipeline}].

groups() ->
	[{proto_error,[],[notag_1,notag_2,
				      nofid_1]},
	 {session,[],[session_1,session_2]},
	 {auth,[],[auth_common,local_auth,
			   auth_mumble,auth_munge]},
   	 {attach,[],[attach_common,attach_alt,
		 		 attach_file_1,attach_file_2]},
	 {walk,[],[walk_1,walk_up,
			   walk_auth]},
     {read_write,[],[read_file_1,read_file_2,
					 read_dir,write]},
	 {stat,[],[stat_1,stat_2,
			   wstat]},
 	 {times,[],[atime,mtime]},
 	 {pipeline,[],[sread,swrite]}].

init_per_suite(Config) ->
	IP = ?config(ip, Config),
	Port = ?config(port, Config),

	L1 = {local,'9p_tcp',{{127,0,0,1},Port}},
	L2 = {other,'9p_tcp',{IP,Port}},

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

init_per_testcase(Case, Config) ->
	IP = if Case =/= local_auth -> ?config(ip, Config);
		true -> {127,0,0,1} end,
	Port = ?config(port, Config),
	ModConf = {IP,Port},
	{ok,Conn} = '9p_tcp':connect(ModConf),
	[{conn,Conn}|Config].

end_per_testcase(Case, Config) ->
	IP = if Case =/= local_auth -> ?config(ip, Config);
		true -> {127,0,0,1} end,
	Port = ?config(port, Config),
	ModConf = {IP,Port},
	Conn = ?config(conn, Config),
	ok = '9p_tcp':close(Conn, ModConf).

version(Config) when is_list(Config) ->
	version_msize(0, Config),
	version_msize(1, Config),
	version_msize(4096, Config),
	version_msize(8192, Config),
	version_msize(16#ffffffff, Config),

	?line {rversion,_,_,?VER_E} =
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),

	?line {rversion,_,_,?VER_U} =
	rtrip({tversion,?NOTAG,4096,?VER_U}, Config),

	?line {rversion,_,_,<<"unknown">>} =
	rtrip({tversion,?NOTAG,4096,<<"9P2001">>}, Config),

	?line {rversion,_,_,<<"unknown">>} =
	rtrip({tversion,?NOTAG,4096,<<"9P2000.L">>}, Config).

version_msize(RSize, Config) ->
	?line {rversion,?NOTAG,MSize,?VER_E} =
	rtrip({tversion,?NOTAG,RSize,?VER_E}, Config),
	true = MSize =< RSize.

session_1(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {ropen,0,_Qid,_} =
	rtrip({topen,0,1,0}, Config),

	%%
	%% Hijack the connection using ?STD_SESSION_KEY
	%%

	IP = ?config(ip, Config),
	Port = ?config(port, Config),
	ModConf = {IP,Port},
	{ok,NewConn} = '9p_tcp':connect(ModConf),

	NewConfig = [{conn,NewConn}
			|lists:keydelete(conn, 1, Config)],
	
	rtrip({tversion,?NOTAG,4096,?VER_E}, NewConfig),

	?line {rsession,?NOTAG} =
	rtrip({tsession,?NOTAG,?STD_SESSION_KEY}, NewConfig),

	%% fid 1 must be valid now - read it
	
	?line {rread,0,<<1,2,3,4,5,6,7>>} =
	rtrip({tread,0,1,0,10}, NewConfig),

	ok = '9p_tcp':close(NewConn, ModConf).

session_2(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	%%
	%% Use wrong session key
	%%

	IP = ?config(ip, Config),
	Port = ?config(port, Config),
	ModConf = {IP,Port},
	{ok,NewConn} = '9p_tcp':connect(ModConf),

	NewConfig = [{conn,NewConn}
			|lists:keydelete(conn, 1, Config)],
	
	rtrip({tversion,?NOTAG,4096,?VER_E}, NewConfig),

	?line {rerror,?NOTAG,_} =
	rtrip({tsession,?NOTAG,<<1,1,1,1,1,1,1,1>>}, NewConfig),

	ok = '9p_tcp':close(NewConn, ModConf).

notag_1(Config) when is_list(Config) ->
	?line {rerror,_,_} =
	rtrip({tversion,0,4096,?VER_E}, Config).

notag_2(Config) when is_list(Config) ->
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),

	%% any message except version must have a valid tag
	?line {rerror,?NOTAG,_} =
	rtrip({tauth,?NOTAG,0,<<"u">>,<<"/">>}, Config).

nofid_1(Config) when is_list(Config) ->
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),

	%% invalid afid
	?line {rerror,0,_} =
	rtrip({tauth,0,?NOFID,<<"u">>,<<"/">>}, Config).

local_auth(Config) ->
	%% 9P2000.e
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),

	%% local connections are auto-authenticated
	?line {rerror,0,_} =
	rtrip({tauth,0,0,<<"u">>,<<"/">>}, Config).

auth_common(Config) ->
	%% 9P2000.e
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),

	?line {rerror,0,_} =
	rtrip({tauth,0,0,<<"u">>,<<>>}, Config),

	?line {rauth,0,<<?QTAUTH,_/binary>>} =
	rtrip({tauth,0,0,<<>>,<<"/">>}, Config),
	rtrip({tclunk,0,0}, Config),

	?line {rauth,0,<<?QTAUTH,_/binary>>} =
	rtrip({tauth,0,0,<<"u">>,<<"/">>}, Config),

	%% 9P2000.u
	rtrip({tversion,?NOTAG,4096,?VER_U}, Config),

	?line {rerror,0,_,_} =
	rtrip({tauth,0,0,<<"u">>,<<>>,0}, Config),

	?line {rauth,0,<<?QTAUTH,_/binary>>} =
	rtrip({tauth,0,0,<<>>,<<"/">>,0}, Config),
	rtrip({tclunk,0,0}, Config),

	?line {rauth,0,<<?QTAUTH,_/binary>>} =
	rtrip({tauth,0,0,<<"u">>,<<"/">>,0}, Config).

auth_mumble(Config) ->
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),
	rtrip({tauth,0,0,<<"u">>,<<"/">>}, Config),

	SK = <<1,2,3,4,5,6,7,8>>,
	Mumble = '9p_auth':mumble(SK),
	Count = size(Mumble),
	
	?line {rwrite,0,Count} =
	rtrip({twrite,0,0,0,Mumble}, Config),

	?line {rclunk,0} =
	rtrip({tclunk,0,0}, Config).

auth_munge(Config) when is_list(Config) ->
	%% TODO
	ok.

attach_common(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/">>}, Config),
	rtrip({tclunk,0,1}, Config),

	?line {rerror,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z1/a/coo">>}, Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"will">>,<<"/z1">>}, Config),
	rtrip({tclunk,0,1}, Config),

	%% UName =/= <<"will">>
	?line {rerror,0,_} =
	rtrip({tattach,0,1,0,<<"tom">>,<<"/z1">>}, Config),

	%% auth user == <<"u">>
	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z2">>}, Config),
	rtrip({tclunk,0,1}, Config).

attach_alt(Config) when is_list(Config) ->
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),
	rtrip({tauth,0,0,<<"uuu">>,<<"/p">>}, Config),
	SK = <<1,2,3,4,5,6,7,8>>,
	Mumble = '9p_auth':mumble(SK),
	rtrip({twrite,0,0,0,Mumble}, Config),

	%% auth user =/= <<"u">>
	?line {rerror,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z2">>}, Config).

attach_file_1(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),
	rtrip({tclunk,0,1}, Config),

	%% UName =/= <<"bert">>
	?line {rerror,0,_} =
	rtrip({tattach,0,1,0,<<"tom">>,<<"/z1/a">>}, Config).

attach_file_2(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z1/b">>}, Config),
	rtrip({tclunk,0,1}, Config),

	%% /z1/c does not exist
	?line {rerror,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z1/c">>}, Config).

walk_1(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/">>}, Config),

	%% clone fid
	?line {rwalk,0,[]} =
	rtrip({twalk,0,1,7,[]}, Config),
	rtrip({tclunk,0,7}, Config),

	%% files exists
	?line {rwalk,0,[Q1,Q2]} =
	rtrip({twalk,0,1,2,[<<"z2">>,<<"bilbo">>]}, Config),

	%% partial walk
	?line {rwalk,0,[Q1]} =
	rtrip({twalk,0,1,3,[<<"z2">>,<<"gandalf">>]}, Config),

	%% newfid == fid
	?line {rwalk,0,[Q1,Q2]} =
	rtrip({twalk,0,1,1,[<<"z2">>,<<"bilbo">>]}, Config).

walk_up(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,RootQid} =
	rtrip({tattach,0,1,0,<<>>,<<"/">>}, Config),

	?line {rerror,0,_} =
	rtrip({twalk,0,1,7,[<<"..">>]}, Config),

	%% partial walk
	?line {rwalk,0,[_Q1,RootQid]} =
	rtrip({twalk,0,1,3,[<<"z2">>,<<"..">>]}, Config).

walk_auth(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/">>}, Config),

	%% /z1/a exists, /z1 not authorized
	?line {rerror,0,_} =
	rtrip({twalk,0,1,3,[<<"z1">>,<<"a">>]}, Config),

	%% /z2/frodo exists, file not authorized
	?line {rwalk,0,[_Q1]} =
	rtrip({twalk,0,1,7,[<<"z2">>,<<"frodo">>]}, Config).

read_dir(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"will">>,<<"/z1">>}, Config),

	?line {ropen,0,_Qid,_} =
	rtrip({topen,0,1,0}, Config),

	%%
	%% The directory contains two files /z1/a and /z1/b.
	%% stat struct is about 60 bytes long.
	%%

	?line {rread,0,Stat1Stat2} =
	rtrip({tread,0,1,0,1024}, Config),

	?line {rread,0,Stat1} = 
	rtrip({tread,0,1,0,80}, Config),

	%% misaligned offset
	?line {rerror,0,_} =
	rtrip({tread,0,1,1,1024}, Config),

	Offset = size(Stat1),
	?line {rread,0,Stat2} =
	rtrip({tread,0,1,Offset,1024}, Config),

	Stat1Stat2 = list_to_binary([Stat1,Stat2]),

	{<<"a">>,7} = valid_stat(Stat1),
	{<<"b">>,100} = valid_stat(Stat2).

valid_stat(<<TSz:16/little,Stat:(TSz)/binary>>) ->
	case Stat of
	<<_Type:16/little,
	  _Dev:32/little,
	  _Qid:13/binary,
	  _Mode:32/little,
	  _Atime:32/little,
	  _Mtime:32/little,
	  Len:64/little,
	  (NSz):16/little,Name:(NSz)/binary,
	  (USz):16/little,_User:(USz)/binary,
	  (GSz):16/little,_Group:(GSz)/binary,
	  (MSz):16/little,_Muser:(MSz)/binary,
	  (ESz):16/little,_Ext:(ESz)/binary,
	  _NumUser:32/little,
	  _NumGroup:32/little,
	  _NumMuser:32/little>> ->
	  {Name,Len};

	<<_Type:16/little,
	  _Dev:32/little,
	  _Qid:13/binary,
	  _Mode:32/little,
	  _Atime:32/little,
	  _Mtime:32/little,
	  Len:64/little,
	  (NSz):16/little,Name:(NSz)/binary,
	  (USz):16/little,_User:(USz)/binary,
	  (GSz):16/little,_Group:(GSz)/binary,
	  (MSz):16/little,_Muser:(MSz)/binary>> ->
	  {Name,Len}
	end.

read_file_1(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {ropen,0,_Qid,_} =
	rtrip({topen,0,1,0}, Config),

	?line {rread,0,<<1,2,3,4,5,6,7>>} =
	rtrip({tread,0,1,0,10}, Config).

read_file_2(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z1/b">>}, Config),

	?line {ropen,0,_Qid,_} =
	rtrip({topen,0,1,0}, Config),

	%% chunked cached data
	D1 = list_to_binary(lists:duplicate(20, 1)),
	D2 = list_to_binary(lists:duplicate(20, 2)),
	D5 = list_to_binary(lists:duplicate(20, 5)),

	%% misaligned
	?line {rerror,0,_} =
	rtrip({tread,0,1,21,100}, Config),

	?line {rread,0,D1} =
	rtrip({tread,0,1,0,39}, Config),

	?line {rread,0,<<D1:20/binary,D2:20/binary>>} =
	rtrip({tread,0,1,0,40}, Config),

	?line {rread,0,D5} =
	rtrip({tread,0,1,80,100}, Config).

create(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"will">>,<<"/z1">>}, Config),

	?line {rcreate,0,_Qid,_Iounit} =
	rtrip({tcreate,0,1,<<"new1">>,0,0}, Config),

	?line {rerror,0,_} =
	rtrip({tcreate,0,1,<<"old1">>,0,0}, Config).

write(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),
	rtrip({topen,0,1,0}, Config),

	?line {rwrite,0,3} =
	rtrip({twrite,0,1,0,<<1,2,3>>}, Config),

	?line {rwrite,0,0} =
	rtrip({twrite,0,1,0,<<"abc">>}, Config).

remove(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {rremove,0} =
	rtrip({tremove,0,1}, Config),
	%% fid should be clunked by now

	%% file not removable
	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/z1/b">>}, Config),

	?line {rerror,0,_} =
	rtrip({tremove,0,1}, Config).

stat_1(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<>>,<<"/">>}, Config),

	?line {rstat,0,Stat} =
	rtrip({tstat,0,1}, Config),

	#stat{qid = <<?QTDIR,_/binary>>,
		  length =0,
	  	  name = <<"/">>} =Stat.

stat_2(Config) when is_list(Config) ->
	preamble(Config),

	?line {rattach,0,_} =
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {rstat,0,Stat} =
	rtrip({tstat,0,1}, Config),

	#stat{qid = <<?QTFILE,_/binary>>,
		  length =7,
	  	  name = <<"a">>} =Stat.

wstat(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {rstat,0,Stat} =
	rtrip({tstat,0,1}, Config),

	?line {rwstat,0} =
	rtrip({twstat,0,1,Stat#stat{length =1234}}, Config).

mtime(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),
	rtrip({topen,0,1,0}, Config),

	{Mega,Secs,_} = now(),
	TS = Mega*1000000 +Secs,

	rtrip({twrite,0,1,0,<<1,2,3>>}, Config),

	?line {rstat,0,Stat} =
	rtrip({tstat,0,1}, Config),
	true = Stat#stat.mtime >= TS.

atime(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),
	rtrip({topen,0,1,0}, Config),

	{Mega,Secs,_} = now(),
	TS = Mega*1000000 +Secs,
	
	rtrip({tread,0,1,0,10}, Config),

	?line {rstat,0,Stat} =
	rtrip({tstat,0,1}, Config),
	true = Stat#stat.atime >= TS.

sread(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"will">>,<<"/z1">>}, Config),

	?line {rsread,0,Data} =
	rtrip({tsread,0,1,[<<"b">>]}, Config),
	true = size(Data) =:= 100,

	?line {rerror,0,_} =
	rtrip({tsread,0,1,[<<"a">>]}, Config).

swrite(Config) when is_list(Config) ->
	preamble(Config),
	rtrip({tattach,0,1,0,<<"bert">>,<<"/z1/a">>}, Config),

	?line {rswrite,0,3} =
	rtrip({tswrite,0,1,[],<<1,2,3>>}, Config),

	?line {rerror,0,_} =
	rtrip({tswrite,0,1,[<<"..">>],<<1,2,3>>}, Config).

%%------ helpers --------------------------------------------------------------

rtrip(Q, Config) ->
	IP = ?config(ip, Config),
	Port = ?config(port, Config),
	ModConf = {IP,Port},
	Conn = ?config(conn, Config),
	ok = '9p_tcp':send(Conn, '9p':encode(Q), ModConf),
	{ok,Packet} = '9p_tcp':recv(Conn, ModConf),
	'9p':decode(Packet).

preamble(Config) ->
	rtrip({tversion,?NOTAG,4096,?VER_E}, Config),
	rtrip({tauth,0,0,<<"u">>,<<"/p">>}, Config),
	SK = ?STD_SESSION_KEY,
	Mumble = '9p_auth':mumble(SK),
	rtrip({twrite,0,0,0,Mumble}, Config).

%%EOF
