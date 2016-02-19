-module(tk_game).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #{p1=>love,p2=>love}}.

handle_call(get_score, _From, State) ->
    {reply, State, State};

handle_call({won_point, p1}, _From, State = #{p1:=40, p2:=P2Score}) when P2Score =/= advantage ->
    {stop, normal, {game_over, p1}, State};

handle_call({won_point, p1}, _From, State = #{p1:=advantage}) ->
    {stop, normal, {game_over, p1}, State};

handle_call({won_point, p1}, _From, State = #{p1:=P1Score,p2:=P2Score}) ->
    {NewP1Score, NewP2Score} = new_score(P1Score, P2Score),
    NewState = State#{p1:=NewP1Score, p2:=NewP2Score},
    {reply, NewState, NewState};

handle_call({won_point, p2}, _From, State = #{p1:=P1Score, p2:=40}) when P1Score =/= advantage ->
    {stop, normal, {game_over, p2}, State};

handle_call({won_point, p2}, _From, State = #{p2:=advantage}) ->
    {stop, normal, {game_over, p2}, State};

handle_call({won_point, p2}, _From, State = #{p1:=P1Score, p2:=P2Score}) ->
    {NewP2Score, NewP1Score} = new_score(P2Score, P1Score),
    NewState = State#{p1:=NewP1Score, p2:=NewP2Score},
    {reply, NewState, NewState};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_score(love, Score) -> {15, Score};
new_score(15, Score) -> {30, Score};
new_score(30, 40) -> {deuce, deuce};
new_score(30, Score) -> {40, Score};
new_score(deuce, deuce) -> {advantage, 40};
new_score(40, advantage) -> {deuce, deuce}.
