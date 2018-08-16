%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    SHENZHEN IO, MCxxxx generic stuff
%%% @end
%%% Created : 14 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(mcxxxx).

-export([compile/2]).
-export([parse/1]).
-export([load/1]).

compile(XXXX,Is) ->
    Ls = enumerate_labels(Is),
    compile_(XXXX,Is,Ls,[]).

compile_(XXXX,[{label,_}|Is],Ls,Acc) ->
    compile_(XXXX,Is,Ls,Acc);
compile_(XXXX,[{'+',Op}|Is],Ls,Acc) ->
    case compile_op(XXXX,Op,Ls) of
	{ok,Op1} -> compile_(XXXX,Is,Ls,[{1,Op1}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[{'-',Op}|Is],Ls,Acc) ->
    case compile_op(XXXX,Op,Ls) of
	{ok,Op1} -> compile_(XXXX,Is,Ls,[{-1,Op1}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[Op|Is],Ls,Acc) ->
    case compile_op(XXXX,Op,Ls) of
	{ok,Op1} -> compile_(XXXX,Is,Ls,[{0,Op1}|Acc]);
	Error -> Error
    end;
compile_(XXXX,[],_Ls,Acc) ->
    L = length(Acc),
    {_,N} = XXXX:lrange(),
    if L > N ->
	    {error,program_too_large};
       true ->
	    {ok,lists:reverse(Acc)}
    end.

%% calculate label addresses
enumerate_labels(Is) ->
    enumerate_labels(Is,1,[]).
    
enumerate_labels([{label,L}|Is],I,Labels) ->
    enumerate_labels(Is, I, [{L,I}|Labels]);
enumerate_labels([_|Is],I,Labels) ->
    enumerate_labels(Is,I+1,Labels);
enumerate_labels([],_I,Labels) ->
    Labels.

compile_op(XXXX,Op,_Ls) when is_atom(Op) ->
    case lists:member(Op, XXXX:instructions()) of
	true -> {ok,Op};
	false -> {error,{unknown_op, Op}}
    end;
compile_op(XXXX,{Op,Arg},Ls) when is_atom(Op) ->
    case lists:keyfind(Op,1,XXXX:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec} ->
	    case compile_arg(XXXX,Spec,Arg,Ls) of
		{ok,A} -> {ok,{Op,A}};
		Error -> Error
	    end
    end;
compile_op(XXXX,{Op,Arg1,Arg2},Ls) when is_atom(Op) ->
    case lists:keyfind(Op,1,XXXX:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec1,Spec2} ->
	    case compile_arg(XXXX,Spec1,Arg1,Ls) of
		{ok,A1} ->
		    case compile_arg(XXXX,Spec2,Arg2,Ls) of
			{ok,A2} -> {ok,{Op,A1,A2}};
			Error -> Error
		    end;
		Error -> Error
	    end
    end;
compile_op(_XXXX,Op,_Ls) ->
    {error,{unknown_op,Op}}.


compile_arg(XXXX,r,Arg,_Ls) ->
    case lists:member(Arg, XXXX:registers()++XXXX:pins()++XXXX:ports()) of
	true  -> {ok,Arg};
	false -> {error,{badarg,Arg}}
    end;
compile_arg(XXXX,p,Arg,_Ls) ->
    case lists:member(Arg, XXXX:ports()) of
	true -> {ok,Arg};
	false -> {error,{badarg,Arg}}
    end;
compile_arg(_XXXX,i,Arg,_Ls) ->
    if is_integer(Arg), Arg >= -999, Arg =< 999 -> 
	    {ok,Arg};
       true -> {error,{badarg,Arg}}
    end;
compile_arg(XXXX,l,Arg,_Ls) when is_integer(Arg) ->
    {L0,L1} = XXXX:lrange(),
    if Arg >= L0, Arg =< L1 -> {ok,Arg};
       true -> {error,{label_overflow,Arg}}
    end;
compile_arg(XXXX,l,Arg,Ls) when is_list(Arg) ->
    case lists:keyfind(Arg, 1, Ls) of
	false -> {error,{label_not_found,Arg}};
	{_,L} ->
	    {L0,L1} = XXXX:lrange(),
	    if L >= L0, L =< L1 -> {ok,L};
	       true -> {error,{label_overflow,L}}
	    end
    end;
compile_arg(XXXX,[T|Ts],Arg,Ls) ->
    case compile_arg(XXXX,T,Arg,Ls) of
	{ok,A} -> {ok,A};
	_Error -> compile_arg(XXXX,Ts,Arg,Ls)
    end;
compile_arg(_XXXX,[],Arg,_Ls) ->
    {error,{badarg,Arg}}.

%% load and parse
load(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    parse(binary_to_list(Bin));
	Error ->
	    Error
    end.

%% parse text code into internal form 
parse(Text) ->
    parse_lines(string:tokens(Text, "\n"),1,[]).

parse_lines([Text|Ls], Ln, Acc) ->
    case scan_line(Text, Ln) of
	{ok,[]} -> %% empty line
	    parse_lines(Ls,Ln+1,Acc);
	{ok,Ts} ->
	    case parse_labels(Ts, Acc) of
		{Acc1,[]} ->
		    parse_lines(Ls,Ln+1,Acc1);
		{Acc1,Ts1} ->
		    case parse_line(Ts1, Ln) of
			{ok,Instr} ->
			    parse_lines(Ls,Ln+1,[Instr|Acc1]);
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end;
parse_lines([],_Ln,Acc) ->
    {ok,lists:reverse(Acc)}.

parse_line(['+',Op|As],Ln) when is_atom(Op) ->
    case parse_args(As,Ln) of
	{ok,As1} ->
	    {ok, {'+', list_to_tuple([Op|As1])}};
	Error -> Error
    end;
parse_line(['-',Op|As],Ln) when is_atom(Op) ->
    case parse_args(As,Ln) of
	{ok,As1} ->
	    {ok, {'-', list_to_tuple([Op|As1])}};
	Error -> Error
    end;
parse_line([Op|As],Ln) when is_atom(Op) ->
    case parse_args(As,Ln) of
	{ok,As1} ->
	    {ok, list_to_tuple([Op|As1])};
	Error -> Error
    end;
parse_line(As,Ln) ->
    {error, {parse_error,Ln,As}}.

parse_labels([L={label,_}|Ts], Acc) ->
    parse_labels(Ts, [L|Acc]);
parse_labels(Ts,Acc) ->
    {Acc,Ts}.


parse_args(As,Ln) -> parse_args(As,Ln,[]).

parse_args([{register,R}|As], Ln, Acc) ->
    parse_args(As, Ln, [R|Acc]);
parse_args([{integer,I}|As], Ln, Acc) ->
    parse_args(As, Ln, [I|Acc]);
parse_args([{string,L}|As], Ln, Acc) ->
    parse_args(As, Ln, [L|Acc]);
parse_args([],_Ln,Acc) ->
    {ok,lists:reverse(Acc)};
parse_args(_,_Ln,_Acc) ->
    {error,badarg}.

scan_line(Text, Ln) ->
    Text1 = strip_comment(Text),
    scan_parts(string:tokens(Text1, " \t"),Ln,[]).

scan_parts([Part|Ps],Ln,Acc) ->
    case scan_integer(Part) of
	false ->
	    case scan_instruction(Part) of
		false ->
		    case scan_register(Part) of
			false ->
			    case scan_label(Part) of
				false -> scan_parts(Ps,Ln,[{string,Part}|Acc]);
				L -> scan_parts(Ps,Ln,[{label,L}|Acc])
			    end;
			R -> scan_parts(Ps,Ln,[{register,R}|Acc])
		    end;
		[Cnd,Op] -> scan_parts(Ps,Ln,[Op,Cnd|Acc]);
		Op -> scan_parts(Ps,Ln,[Op|Acc])
	    end;
	Int -> scan_parts(Ps,Ln,[{integer,Int}|Acc])
    end;
scan_parts([],_Ln,Acc) ->
    Acc1 = lists:reverse(Acc),
    {ok,Acc1}.

scan_integer(Text) ->
    try list_to_integer(Text) of
	Int -> Int
    catch
	error:_ -> false
    end.

scan_register("acc") -> acc;
scan_register("dat") -> dat;
scan_register("null") -> null;
scan_register([$x,I]) when I>=$0, I=<$9 -> {x,I-$0};
scan_register([$p,I]) when I>=$0, I=<$9 -> {p,I-$0};
scan_register(_) -> false.
    
scan_label(Text) ->
    case lists:reverse(Text) of
	[$:|RText] -> lists:reverse(RText);
	_ -> false
    end.

scan_instruction([$+|Text]) ->
    case scan_instruction(Text) of
	false -> false;
	Instr -> ['+',Instr]
    end;
scan_instruction([$-|Text]) ->
    case scan_instruction(Text) of
	false -> false;
	Instr -> ['-',Instr]
    end;
scan_instruction("nop") -> nop;
scan_instruction("mov") -> mov;
scan_instruction("jmp") -> jmp;
scan_instruction("slp") -> slp;
scan_instruction("slx") -> slx;
scan_instruction("add") -> add;
scan_instruction("sub") -> sub;
scan_instruction("mul") -> mul;
scan_instruction("not") -> 'not';
scan_instruction("dgt") -> dgt;
scan_instruction("dst") -> dst;
scan_instruction("teq") -> teq;
scan_instruction("tgt") -> tgt;
scan_instruction("tlt") -> tlt;
scan_instruction("tcp") -> tcp;
scan_instruction(_) -> false.

strip_comment(Text) ->
    case string:chr(Text, $#) of
	0 -> Text;
	I -> string:substr(Text,1,I-1)
    end.
