%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    SHENZHEN IO, MCxxxx generic stuff
%%% @end
%%% Created : 14 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(mcxxxx).

-export([compile/2]).
-export([parse/1]).

compile(XXXX,Is) ->
    compile_(XXXX,Is,[]).

compile_(XXXX,[{'+',Op}|Is],Acc) ->
    case is_valid_op(XXXX,Op) of
	ok -> compile_(XXXX,Is,[{1,Op}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[{'-',Op}|Is],Acc) ->
    case is_valid_op(XXXX,Op) of
	ok -> compile_(XXXX,Is,[{-1,Op}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[Op|Is],Acc) ->
    case is_valid_op(XXXX,Op) of
	ok -> compile_(XXXX,Is,[{0,Op}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[],Acc) ->
    L = length(Acc),
    {_,N} = XXXX:lrange(),
    if L > N ->
	    {error,program_too_large};
       true ->
	    {ok,lists:reverse(Acc)}
    end.

is_valid_op(XXXX,Op) when is_atom(Op) ->
    case lists:member(Op, XXXX:instructions()) of
	true -> ok;
	false -> {error,{unknown_op, Op}}
    end;
is_valid_op(XXXX,{Op,Arg}) when is_atom(Op) ->
    case lists:keyfind(Op,1,XXXX:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec} -> is_valid_arg(XXXX,Spec,Arg)
    end;
is_valid_op(XXXX,{Op,Arg1,Arg2}) when is_atom(Op) ->
    case lists:keyfind(Op,1,XXXX:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec1,Spec2} ->
	    case is_valid_arg(XXXX,Spec1,Arg1) of
		ok -> is_valid_arg(XXXX,Spec2,Arg2);
		Error -> Error
	    end
    end.

is_valid_arg(XXXX,[r],Arg) ->
    case lists:member(Arg, XXXX:registers()++XXXX:pins()++XXXX:ports()) of
	true -> ok;
	false -> {error,{badarg,Arg}}
    end;
is_valid_arg(XXXX,[p],Arg) ->
    case lists:member(Arg, XXXX:ports()) of
	true -> ok;
	false -> {error,{badarg,Arg}}
    end;
is_valid_arg(XXXX,[r,i],Arg) ->
    if is_integer(Arg), Arg >= -999, Arg =< 999 -> ok;
       true -> case lists:member(Arg, XXXX:registers()++
				     XXXX:pins()++XXXX:ports()) of
		   true -> ok;
		   false -> {error,{badarg,Arg}}
	       end
    end;
is_valid_arg(XXXX,[l],Arg) ->
    {L0,L1} = XXXX:lrange(),
    if Arg >= L0, Arg =< L1 -> ok;
       true -> {error,{label_overflow,Arg}}
    end.

%% parse text code into internal form 
parse(Text) ->
    Rows = string:tokens(Text),
    Rows1 = [string:trim(R) || R <- Rows],
    ok.

    
    
    
