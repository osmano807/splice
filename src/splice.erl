%%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 sts=4 et
%%%
%%% Copyright 2008-2011 Steve Vinoski. All Rights Reserved.
%%% Copyright 2010-2011 Tuncer Ayaz. All Rights Reserved.
%%% Copyright 2012 Joaquim Pedro França Simão. All Rights Reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% Based on original code from yaws
%%%
%%% File    : yaws_sendfile.erl
%%% Author  : Claes Wikstrom, klacke@hyber.org
%%%           Steve Vinoski, vinoski@ieee.org
%%% Description : Conditional OS dependent call to splice
%%%
%%% Renamed to sendfile and modified: Tuncer Ayaz in May 2010
%%% Renamed to splice and modified: Joaquim Pedro in Jan 2012

-module(splice).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, send/4, enabled/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

send(In, Out, Offset, Count) when is_integer(Count)->
    do_send(In, Out, Offset, Count).

-ifdef(HAVE_SPLICE).
enabled() ->
    true.
-else.
enabled() ->
    false.
-endif.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% Will be defined for Linux, FreeBSD, DragonflyBSD, Solaris and Mac OS X
-ifdef(HAVE_SPLICE).

-record(state, {
          port,               % driver port
          caller_tbl          % table mapping socket descriptors to callers
         }).

init([]) ->
    process_flag(trap_exit, true),
    Shlib = "splice_drv",
    Dir = code:priv_dir(?MODULE),
    case erl_ddll:load_driver(Dir, Shlib) of
        ok -> 
            ok;
        {error, already_loaded} -> 
            ok;
        _ -> 
            exit({error, "could not load driver " ++ Shlib})
    end,
    Port = open_port({spawn, Shlib}, [binary]),
    CallerTable = ets:new(splice_drv, []),
    {ok, #state{port = Port, caller_tbl = CallerTable}}.

handle_call({send, InFd, Msg}, From, State) ->
    true = erlang:port_command(State#state.port, Msg),
    true = ets:insert(State#state.caller_tbl, {InFd, From}),
    {noreply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({_, {data, <<Cnt:64, InFd:32, _OutFd:32, Res:8, Err/binary>>}}, State) ->
    Reply = case Res of
                1 ->
                    {ok, Cnt};
                0 ->
                    {error,
                     list_to_atom(
                       lists:takewhile(fun(El) -> El =/= 0 end,
                                       binary_to_list(Err)))}
            end,
    CallerTable = State#state.caller_tbl,
    [{InFd, From}] = ets:lookup(CallerTable, InFd),
    gen_server:reply(From, Reply),
    ets:delete(CallerTable, InFd),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port, caller_tbl = CallerTable}) ->
    erlang:port_close(Port),
    receive {'EXIT', Port, _Reason} -> ok
    after 0 -> ok
    end,
    ets:delete(CallerTable),
    ok.

-else.

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
-endif.

handle_cast(stop, State) ->
    {stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-ifdef(HAVE_SPLICE).
do_send(_In, _Out, _Offset, Count) when Count =< 0 ->
    {ok, 0};
do_send(In, Out, Offset, Count) ->
    {ok, InFd} = prim_inet:getfd(In),
    {ok, OutFd} =  prim_inet:getfd(Out),
    Call = list_to_binary([<<Offset:64, Count:64, InFd:32, OutFd:32>>, <<0:8>>]),
    case gen_server:call(?SERVER, {send, InFd, Call}, infinity) of
        {error, eoverflow} ->
            compat_send(In, Out, Count, Count);
        Else ->
            Else
    end.
-else.
do_send(_In, _Out, _Offset, Count) when Count =< 0 ->
    {ok, 0};
do_send(In, Out, _Offset, undefined) ->
    compat_send(In, Out, undefined, undefined);
do_send(In, Out, Offset, Count) when is_integer(Offset) andalso is_integer(Count) ->
    compat_send(In, Out, Count - Offset, Count).
-endif.

compat_send(In, Out, Remaining, Count) ->
    case gen_tcp:recv(In, 0) of
        {ok, Data} ->
            Size = iolist_size(Data),
            {Ended, NewRemaining} = case {Remaining, Size} of
                {_, 0} ->
                    {true, Remaining};
                {undefined, _} ->
                    {false, Remaining};
                _ when is_integer(Remaining) ->
                    if
                        Remaining - Size > 0 ->
                            {false, Remaining - Size};
                        Remaining - Size == 0 ->
                            {true, Remaining - Size};
                        true ->
                            {true, Remaining - Size}
                    end
            end,
            case Ended of 
                true ->
                    case gen_tcp:send(Out, Data) of
                        ok ->
                            {ok, Count};
                        {error, _} = Err2 ->
                            Err2
                    end;
                false ->
                    case gen_tcp:send(Out, Data) of
                        ok ->
                            compat_send(In, Out, NewRemaining, Count);
                        {error, _} = Err3 ->
                            Err3
                    end
            end;
        {error, _} = Err1 ->
            Err1
    end.

