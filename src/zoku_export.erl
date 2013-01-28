-module(zoku_export).

%%
%% @doc A callback module used by 9p_SUITE. Modify in sync.
%%

%% Callbacks
-export([top_granted/3,file_granted/4]).
-export([list_dir/2,exists/3]).
-export([create/3,remove/3]).
-export([read/5,write/5]).
-export([file_size/3,truncate/5]).

top_granted(undefined, _, z2) ->
   	true; %% allow walks
top_granted(User, _Conn, z1) ->
	User =:= <<"will">>;
top_granted(_User, Conn, z2) ->
	AuthUser = '9p_info':auth_user(Conn),
	AuthUser =:= <<"u">> orelse
		AuthUser =:= undefined;	%% auto-authenticated
top_granted(_, _, _) ->
	false.

file_granted(<<"a">>, <<"bert">>, _Conn, z1) -> true;
file_granted(<<"a">>, _, _Conn, z1) -> false;
file_granted(<<"frodo">>, _, _Conn, z2) -> false;
file_granted(_File, _User, _Conn, _ModConf) -> true.

list_dir(_Conn, z1) -> [<<"a">>,<<"b">>];
list_dir(_Conn, z2) -> [<<"bilbo">>].

exists(<<"a">>, _Conn, z1) -> true;
exists(<<"b">>, _Conn, z1) -> true;
exists(<<"bilbo">>, _Conn, z2) -> true;
exists(<<"frodo">>, _Conn, z2) -> true;
exists(_File, _Conn, _ModConf) -> false.

create(<<"new1">>, _Conn, z1) -> true;
create(_Name, _Conn, _ModConf) -> false.

remove(<<"a">>, _Conn, z1) -> true;
remove(_Name, _Conn, _ModConf) -> false.

read(<<"a">>, 0, Count, _Conn, z1) when Count >= 7 ->
	<<1,2,3,4,5,6,7>>;
read(<<"b">>, _Offset, _Count, _Conn, z1) ->
	D1 = list_to_binary(lists:duplicate(20, 1)),
	D2 = list_to_binary(lists:duplicate(20, 2)),
	D3 = list_to_binary(lists:duplicate(20, 3)),
	D4 = list_to_binary(lists:duplicate(20, 4)),
	D5 = list_to_binary(lists:duplicate(20, 5)),
	{cache,[D1,D2,D3,D4,D5]}.

write(<<"a">>, 0, <<1,2,3>>, _Conn, z1) -> 3;
write(_File, _Offset, _Data, _Conn, _ModConf) -> 0.

file_size(<<"a">>, _Conn, z1) -> 7;
file_size(<<"b">>, _Conn, z1) -> 100;
file_size(_File, _Conn, _ModConf) -> 0.

truncate(<<"a">>, _OldSize, Size, _Conn, _ModConf) -> Size;
truncate(_File, OldSize, _Size, _Conn, _ModConf) -> OldSize.

%%EOF
