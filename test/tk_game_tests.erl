-module(tk_game_tests).

-include_lib("eunit/include/eunit.hrl").

foo_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun initial_score/1,
        fun fifteen_love/1,
        fun love_fifteen/1
    ]}.

setup() ->
    process_flag(trap_exit, true),
    {ok, Pid} = tk_game:start_link(),
    Pid.

cleanup(Pid) ->
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).

server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.

initial_score(Pid) ->
    fun() ->
        ?assertEqual(#{p1=>love,p2=>love}, gen_server:call(Pid, get_score))
    end.

fifteen_love(Pid) ->
    fun() ->
        ?assertEqual(#{p1=>15,p2=>love}, gen_server:call(Pid, {won_point, p1}))
    end.

love_fifteen(Pid) ->
    fun() ->
        ?assertEqual(#{p1=>love,p2=>15}, gen_server:call(Pid, {won_point, p2}))
    end.
