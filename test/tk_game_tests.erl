-module(tk_game_tests).

-include_lib("eunit/include/eunit.hrl").

tk_game_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun initial_score/1,
        fun fifteen_love/1,
        fun love_fifteen/1,
        fun fifteen_all/1,
        fun thirty_fifteen/1,
        fun thirty_all/1,
        fun forty_thirty/1,
        fun deuce/1,
        fun p1_wins/1,
        fun p2_wins/1,
        fun advantage_p1/1,
        fun advantage_p2/1,
        fun advantage_p1_back_to_deuce/1
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
        ?assertEqual(#{p1=>15,p2=>love}, won_point(Pid, p1))
    end.

love_fifteen(Pid) ->
    fun() ->
        ?assertEqual(#{p1=>love,p2=>15}, won_point(Pid, p2))
    end.

fifteen_all(Pid) ->
    fun() ->
        won_point(Pid, p1),
        ?assertEqual(#{p1=>15,p2=>15}, won_point(Pid, p2))
    end.

thirty_fifteen(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>30,p2=>15}, won_point(Pid, p1))
    end.

thirty_all(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>30,p2=>30}, won_point(Pid, p1))
    end.

forty_thirty(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p1),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>40,p2=>30}, won_point(Pid, p1))
    end.

deuce(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p1),
        won_point(Pid, p1),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>deuce,p2=>deuce}, won_point(Pid, p2))
    end.

p1_wins(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p1),
        won_point(Pid, p1),
        ?assertEqual({game_over, p1}, won_point(Pid, p1))
    end.

p2_wins(Pid) ->
    fun() ->
        won_point(Pid, p2),
        won_point(Pid, p2),
        won_point(Pid, p2),
        ?assertEqual({game_over, p2}, won_point(Pid, p2))
    end.

advantage_p1(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p1),
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>advantage,p2=>40}, won_point(Pid, p1))
    end.

advantage_p2(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p1),
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p2),
        ?assertEqual(#{p1=>40,p2=>advantage}, won_point(Pid, p2))
    end.

advantage_p1_back_to_deuce(Pid) ->
    fun() ->
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p1),
        won_point(Pid, p1),
        won_point(Pid, p2),
        won_point(Pid, p2),
        won_point(Pid, p1),
        ?assertEqual(#{p1=>deuce,p2=>deuce}, won_point(Pid, p2))
    end.

won_point(Pid, Player) ->
    gen_server:call(Pid, {won_point, Player}).
