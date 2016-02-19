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

handle_call({won_point, p1}, _From, State = #{p1:=CurrentScore}) ->
    NewState = State#{p1:=new_score(CurrentScore)},
    {reply, NewState, NewState};

handle_call({won_point, p2}, _From, State = #{p2:=CurrentScore}) ->
    NewState = State#{p2:=new_score(CurrentScore)},
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

new_score(love) -> 15.
