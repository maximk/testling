-module(bench).

-export([run_suite/3, run/1]).

run_suite(Name, Passes, Messages) ->
    Results = run_suite1(Passes, Messages, []),
    Avg = lists:sum(Results) / Passes,
    StdDev = math:sqrt(lists:sum([math:pow(X - Avg, 2) || X <- Results]) /
Passes),
    Summary = [{avg, Avg}, {stddev, StdDev}, {min, lists:min(Results)},
               {max, lists:max(Results)}],
    io:format("[{name, ~p}, {summary, ~p}, {details, ~p}].~n",
			[Name, Summary, Results]).

run_suite1(0, _Messages, Results) ->
    lists:reverse(Results);
run_suite1(Passes, Messages, Results) ->
    erlang:garbage_collect(),
    run_suite1(Passes - 1, Messages, [run(Messages) | Results]).

run(Count) ->
    Me = self(),
    Start = erlang:now(),
    F = spawn(fun() -> worker(Me, Count) end),
    spawn(fun() -> worker(Me, F, Count) end),
    wait_for_done(2),
    timer:now_diff(erlang:now(), Start).

wait_for_done(0) ->
    ok;
wait_for_done(Count) ->
    receive
        done ->
            wait_for_done(Count - 1)
    end.

worker(Owner, 0) ->
    Owner ! done;
worker(Owner, Count) ->
    receive
        {From, ping} ->
            From ! pong,
            worker(Owner, Count - 1)
    end.

worker(Owner, _Target, 0) ->
    Owner ! done;
worker(Owner, Target, Count) ->
    Target ! {self(), ping},
    receive
        pong ->
            worker(Owner, Target, Count - 1)
    end.

%%EOF
