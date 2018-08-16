%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    wire mcxxxx and other components
%%% @end
%%% Created : 14 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(wire).
-export([connect/2, disconnect/2, read/1, write/2]).
-export([wait/1]).
-export([next/0]).

%% debug
-export([get_dst_list/1, get_pin/1, set_pin/2, clear_pin/1]).

%% Pin is p0,p1..  x0,x1,..
connect(Src,Dst) ->
    case is_xpin(Src) =:= is_xpin(Dst) of %% compatible pins
	true ->
	    case pin_owner(Src) =:= pin_owner(Dst) of
		true -> {error, connect_to_self};
		false ->
		    List = get_dst_list(Src),
		    set_dst_list(Src,[Dst|(List--[Dst])]),
		    ok
	    end;
	false ->
	    {error,bad_pin_types}
    end.

disconnect(Src,Dst) ->
    List = get_dst_list(Src),
    set_dst_list(Src,(List--[Dst])),
    ok.

write(Src,Value) ->
    case is_xpin(Src) of
	true ->
	    case get_read_list(Src) of
		[] ->
		    block_write(Src);
		Rs ->
		    %% select a random reader
		    D = lists:nth(rand:uniform(length(Rs)), Rs),
		    set_xpin(D,Value),
		    ok
	    end;
	false ->
	    Ds = get_dst_list(Src),
	    %% fixme: check this
	    lists:foreach(fun(D) -> set_pin(D,Value) end, Ds),
	    ok
    end.

block_write(Pin) -> %% fixme
    put({block_write,Pin}, true),
    block.

read(Pin) ->
    case is_xpin(Pin) of
	true ->
	    case get_pin(Pin) of
		undefined ->
		    block_read(Pin);
		Value ->
		    clear_pin(Pin), 
		    Value
	    end;
	false ->
	    case get_pin(Pin) of
		undefined -> 0;
		Value -> Value
	    end
    end.

block_read(Pin) ->
    put({block_read,Pin}, true),
    block.

wait(Pin) ->
    case is_xpin(Pin) of
	true ->
	    case get_pin(Pin) of
		undefined ->
		    block_read(Pin);
		_ ->
		    ok
	    end;
	false ->
	    ok
    end.

set_pin(Pin, Value) ->
    put({pin,Pin}, Value).

set_xpin(Pin,Value) ->
    R = put({pin,Pin}, Value),
    case get({block_read,Pin}) of
	true ->
	    put({block_read,Pin},false),
	    schedule(Pin),
	    R;
	false ->
	    R
    end.

next() ->
    case get(xqueue) of
	undefined -> empty;
	[] -> empty;
	[ID|Q] -> put(xqueue,Q),ID
    end.

schedule({ID,{x,_}}) ->
    Ls = case get(xqueue) of
	     undefined -> [];
	     Q when is_list(Q) -> Q
	 end,
    case lists:member(ID, Ls) of
	true -> ok;
	false -> put(xqueue,Ls++[ID]), ok
    end.

get_pin(Pin) ->
    get({pin,Pin}).

clear_pin(Pin) ->
    erase({pin,Pin}).

%% check if ready for read
is_reading(Pin) ->
    case get({block_read,Pin}) of
	true -> true;
	_ -> false
    end.

get_read_list(P) ->
    lists:filter(fun is_reading/1, get_dst_list(P)).

get_dst_list(P) ->
    case get({dst,P}) of
	undefined -> [];
	L when is_list(L) -> L
    end.

set_dst_list(P,[]) ->
    erase({dst,P});
set_dst_list(P,L) when is_list(L) ->
    put({dst,P},L).

is_xpin({_,{x,_}}) -> true;
is_xpin(_) -> false.

pin_owner({ID,_}) -> ID.

    
