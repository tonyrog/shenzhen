%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    wire mcxxxx and other components
%%% @end
%%% Created : 14 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(wire).
-export([connect/2, disconnect/2, read/1, write/2]).
-export([wait/1]).

%% Pin is p0,p1..  x0,x1,..
connect(Src,Dst) ->
    List = get_dst_list(Src),
    set_dst_list(Src,[Dst|(List--[Dst])]),
    ok.

disconnect(Src,Dst) ->
    List = get_dst_list(Src),
    set_dst_list(Src,(List--[Dst])),
    ok.

write(Src,Value) ->
    lists:foreach(
      fun(Pin) ->
	      set_pin_value(Pin, Value)
      end, get_dst_list(Src)).

read(Pin) ->
    case get_pin_value(Pin) of
	undefined -> error;
	Value -> Value
    end.

wait(_Pin) -> 1.

set_pin_value(Pin, Value) ->
    put({pin,Pin}, Value).

get_pin_value(Pin) ->
    get({pin,Pin}).

get_dst_list(P) ->
    case get({dst,P}) of
	undefined -> [];
	L when is_list(L) -> L
    end.

set_dst_list(P,[]) ->
    erase({dst,P});
set_dst_list(P,L) when is_list(L) ->
    put({dst,P},L).
