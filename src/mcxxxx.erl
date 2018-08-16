%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    SHENZHEN IO, MCxxxx generic stuff
%%% @end
%%% Created : 14 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(mcxxxx).

-export([compile/1]).
-export([parse/1]).
-export([load/1]).
-export([scan/1]).


compile(Code) ->
    compile(Code, [], [], []).

compile([{{macro,Name,Args},Macro}|Code],Progs,Macros,Acc) ->
    compile(Code, Progs, [{Name,Args,Macro}|Macros], Acc);
compile([{prog,Type,ID}|Code],Progs,Macros,Acc) ->
    compile(Code, [{Type,ID}|Progs],Macros,Acc);
compile([{code,Prog}|Code],Progs,Macros,Acc) ->
    Type = prog_type(Progs, undefined),
    case compile_prog(Type,Prog,Macros) of
	{ok,Compiled} ->
	    compile(Code, [], Macros, [{Progs,Compiled}|Acc]);
	Error ->
	    Error
    end;
compile([{directive,D}|Code],Progs,Macros,Acc) ->
    compile(Code, Progs, Macros, [{directive,D}|Acc]);
compile([],_Progs,_Macros,Acc) ->
    {ok, lists:reverse(Acc)}.

prog_type([{mc4000,_}|_Ts], _) -> mc4000;
prog_type([{mc6000,_}|_Ts], mc4000) -> mc4000;
prog_type([{mc6000,_}|Ts], undefined) -> prog_type(Ts, mc6000);
prog_type([{mc6000,_}|Ts], mc6000) -> prog_type(Ts, mc6000);
prog_type([], undefined) -> mc4000;
prog_type([], Type) -> Type.

compile_prog(Type,Prog, Macros) ->
    case expand_macros(Prog, Macros, []) of
	{ok,Prog1} ->
	    Ls = enumerate_labels(Prog1),
	    compile_(Type,Prog1,Ls,[]);
	Error ->
	    Error
    end.

compile_(Type,[{label,_}|Is],Ls,Acc) ->
    compile_(Type,Is,Ls,Acc);
compile_(Type,[{'+',Op}|Is],Ls,Acc) ->
    case compile_op(Type,Op,Ls) of
	{ok,Op1} -> compile_(Type,Is,Ls,[{1,Op1}|Acc]);
	Error -> Error
    end;
compile_(Type,[{'-',Op}|Is],Ls,Acc) ->
    case compile_op(Type,Op,Ls) of
	{ok,Op1} -> compile_(Type,Is,Ls,[{-1,Op1}|Acc]);
	Error -> Error
    end;
compile_(Type,[Op|Is],Ls,Acc) ->
    case compile_op(Type,Op,Ls) of
	{ok,Op1} -> compile_(Type,Is,Ls,[{0,Op1}|Acc]);
	Error -> Error
    end;
compile_(Type,[],_Ls,Acc) ->
    L = length(Acc),
    {_,N} = Type:lrange(),
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

expand_macros([{{call,Name},Args}|Code],Ms,Acc) ->
    case lists:keyfind(Name, 1, Ms) of
	false ->
	    {error,{macro_not_defined,Name}};
	{_,Formal,Body} ->
	    case length(Formal) =:= length(Args) of
		false ->
		    {error,{macro_arg_mismatch,Name}};
		true ->
		    Bound = lists:zip(Formal,Args),
		    Body1 = set_bindings(Body, Bound),
		    Body2 = prefix_labels(Body1, erlang:unique_integer()),
		    expand_macros(Body2++Code, Ms, Acc)
	    end
    end;
expand_macros([A|Code], Ms, Acc) ->
    expand_macros(Code, Ms, [A|Acc]);
expand_macros([], _Ms, Acc) ->
    {ok, lists:reverse(Acc)}.

prefix_labels([{label,L}|Code], Prefix) ->
    [{label,{Prefix,L}} | prefix_labels(Code, Prefix)];
prefix_labels([{'+',{jmp,[L]}}|Code], Prefix) ->
    [{'+',{jmp,[{Prefix,L}]}}  | prefix_labels(Code, Prefix)];
prefix_labels([{'-',{jmp,[L]}}|Code], Prefix) ->
    [{'-',{jmp,[{Prefix,L}]}}  | prefix_labels(Code, Prefix)];
prefix_labels([{jmp,[L]}|Code], Prefix) ->
    [{jmp,[{Prefix,L}]}  | prefix_labels(Code, Prefix)];
prefix_labels([A|Code], Prefix) ->
    [A | prefix_labels(Code, Prefix)];
prefix_labels([], _Prefix) ->
    [].

set_bindings([L={label,_}|Code], Bound) ->
    [L|set_bindings(Code, Bound)];
set_bindings([{'+',{Op,Args}}|Code], Bound) ->
    [{'+',{Op,set_args(Args, Bound)}}|set_bindings(Code, Bound)];
set_bindings([{'-',{Op,Args}}|Code], Bound) ->
    [{'-',{Op,set_args(Args, Bound)}}|set_bindings(Code, Bound)];
set_bindings([{Op,Args}|Code], Bound) ->
    [{Op,set_args(Args, Bound)}|set_bindings(Code, Bound)];
set_bindings([], _Bound) ->
    [].

set_args([V|Args], Bound) when is_atom(V) ->
    case lists:keyfind(V, 1, Bound) of
	false -> [V|set_args(Args,Bound)];
	{_,Val} ->[Val | set_args(Args,Bound)]
    end;
set_args([A|Args], Bound) ->
    [A|set_args(Args, Bound)];
set_args([], _Bound) ->
    [].


compile_op(Type,{Op,[]},_Ls) when is_atom(Op) ->
    case lists:member(Op, Type:instructions()) of
	true -> {ok,Op};
	false -> {error,{unknown_op, Op}}
    end;
compile_op(Type,{Op,[Arg]},Ls) when is_atom(Op) ->
    case lists:keyfind(Op,1,Type:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec} ->
	    case compile_arg(Type,Spec,Arg,Ls) of
		{ok,A} -> {ok,{Op,A}};
		Error -> Error
	    end
    end;
compile_op(Type,{Op,[Arg1,Arg2]},Ls) when is_atom(Op) ->
    case lists:keyfind(Op,1,Type:instructions()) of
	false -> {error,{unknown_op,Op}};
	{Op,Spec1,Spec2} ->
	    case compile_arg(Type,Spec1,Arg1,Ls) of
		{ok,A1} ->
		    case compile_arg(Type,Spec2,Arg2,Ls) of
			{ok,A2} -> {ok,{Op,A1,A2}};
			Error -> Error
		    end;
		Error -> Error
	    end
    end;
compile_op(_Type,Op,_Ls) ->
    {error,{unknown_op,Op}}.


compile_arg(Type,r,Arg,_Ls) ->
    case lists:member(Arg, Type:registers()++Type:pins()++Type:ports()) of
	true  -> {ok,Arg};
	false -> {error,{badarg,Arg}}
    end;
compile_arg(Type,p,Arg,_Ls) ->
    case lists:member(Arg, Type:ports()) of
	true -> {ok,Arg};
	false -> {error,{badarg,Arg}}
    end;
compile_arg(_Type,i,Arg,_Ls) ->
    if is_integer(Arg), Arg >= -999, Arg =< 999 -> 
	    {ok,Arg};
       true -> {error,{badarg,Arg}}
    end;
compile_arg(Type,l,Arg,_Ls) when is_integer(Arg) ->
    {L0,L1} = Type:lrange(),
    if Arg >= L0, Arg =< L1 -> {ok,Arg};
       true -> {error,{label_overflow,Arg}}
    end;
compile_arg(Type,l,Arg,Ls) ->
    case lists:keyfind(Arg, 1, Ls) of
	false -> {error,{label_not_found,Arg}};
	{_,L} ->
	    {L0,L1} = Type:lrange(),
	    if L >= L0, L =< L1 -> {ok,L};
	       true -> {error,{label_overflow,L}}
	    end
    end;
compile_arg(Type,[T|Ts],Arg,Ls) ->
    case compile_arg(Type,T,Arg,Ls) of
	{ok,A} -> {ok,A};
	{error,{badarg,_}} -> compile_arg(Type,Ts,Arg,Ls);
	Error -> Error
    end;
compile_arg(_Type,[],Arg,_Ls) ->
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
    case scan(Text) of
	{ok,Ts} -> parse_file(Ts,[]);
	Error -> Error
    end.

parse_file([{'[',_},{atom,_,Type},T,{']',_}|Ts], Acc) ->
    ID = erl_parse:normalise(T),
    parse_file(Ts, [{prog,Type,ID}|Acc]);
parse_file([{'[',_},{atom,_,connect},T1,P1,T2,P2,{']',_}|Ts], Acc) ->
    ID1 = erl_parse:normalise(T1),
    PIN1 = pin(P1),
    ID2 = erl_parse:normalise(T2),
    PIN2 = pin(P2),
    parse_file(Ts, [{directive,{connect,ID1,PIN1,ID2,PIN2}}|Acc]);
parse_file([{atom,_,code},{'{',_}|Ts], Acc) ->
    parse_code(code, '', Ts, [], Acc);
parse_file([{atom,_,macro},{var,_,Name},{'(',_}|Ts], Acc) ->
    parse_macro(Name, Ts, [], Acc);
parse_file([{atom,_,macro},{atom,_,Name},{'(',_}|Ts], Acc) ->
    parse_macro(Name, Ts, [], Acc);
parse_file([], Acc) ->
    {ok, lists:reverse(Acc)}.

parse_macro(Name, [{var,_,V}|Ts], Args, Acc) ->
    parse_macro(Name, Ts, [V|Args], Acc);
parse_macro(Name, [{',',_}|Ts], Args, Acc) ->
    parse_macro(Name, Ts, Args, Acc);
parse_macro(Name, [{')',_},{'{',_}|Ts], Args, Acc) ->
    parse_code({macro,Name,lists:reverse(Args)}, '', Ts, [], Acc);
parse_macro(Name, _, _Args, _Acc) ->
    {error, {macro_variable_expected,Name}}.

parse_code(What, _Pref, [{atom,Ln,Name},{':',Ln}|Ts], Code, Acc) ->
    parse_code(What, '', Ts, [{label,Name}|Code], Acc);
parse_code(What, _Pref, [{'+',_}|Ts], Code, Acc) ->
    parse_code(What, '+', Ts, Code, Acc);
parse_code(What, _Pref, [{'-',_}|Ts], Code, Acc) ->
    parse_code(What, '-', Ts, Code, Acc);

parse_code(What, Pref, [{atom,Ln,Name},{'(',Ln}|Ts], Code, Acc) ->
    {Args,Ts2} = parse_args(Ln,Ts,[]),
    parse_code(What, '', Ts2, [prefix(Pref,{{call,Name},Args})|Code], Acc);
parse_code(What, Pref, [{var,Ln,Name},{'(',Ln}|Ts], Code, Acc) ->
    {Args,Ts2} = parse_args(Ln,Ts,[]),
    parse_code(What, '', Ts2, [prefix(Pref,{{call,Name},Args})|Code], Acc);
parse_code(What, Pref, [{atom,Ln,Op}|Ts], Code, Acc) ->
    {Args,Ts1} = parse_args(Ln,Ts,[]),
    parse_code(What, '', Ts1, [prefix(Pref,{Op,Args})|Code], Acc);
parse_code(What, _Pref, [{'}',_}|Ts], Code, Acc) ->
    parse_file(Ts, [{What, lists:reverse(Code)}|Acc]);
parse_code(What, _Pref, [], Code, Acc) ->
    parse_file([], [{What, lists:reverse(Code)}|Acc]).

prefix('+', Op) -> {'+', Op};
prefix('-', Op) -> {'-', Op};
prefix('', Op) -> Op.

parse_args(Ln, [{atom,Ln,acc}|Ts], Acc) -> parse_args(Ln, Ts, [acc|Acc]);
parse_args(Ln, [{atom,Ln,dat}|Ts], Acc) -> parse_args(Ln, Ts, [dat|Acc]);
parse_args(Ln, [{atom,Ln,null}|Ts], Acc) -> parse_args(Ln, Ts, [null|Acc]);
parse_args(Ln, [{atom,Ln,x0}|Ts], Acc) -> parse_args(Ln, Ts, [{x,0}|Acc]);
parse_args(Ln, [{atom,Ln,x1}|Ts], Acc) -> parse_args(Ln, Ts, [{x,1}|Acc]);
parse_args(Ln, [{atom,Ln,x2}|Ts], Acc) -> parse_args(Ln, Ts, [{x,2}|Acc]);
parse_args(Ln, [{atom,Ln,x3}|Ts], Acc) -> parse_args(Ln, Ts, [{x,3}|Acc]);
parse_args(Ln, [{atom,Ln,p0}|Ts], Acc) -> parse_args(Ln, Ts, [{p,0}|Acc]);
parse_args(Ln, [{atom,Ln,p1}|Ts], Acc) -> parse_args(Ln, Ts, [{p,1}|Acc]);
parse_args(Ln, [{atom,Ln,A}|Ts], Acc) -> parse_args(Ln, Ts, [A|Acc]);
parse_args(Ln, [{var,Ln,A}|Ts], Acc) -> parse_args(Ln, Ts, [A|Acc]);
parse_args(Ln, [{'-',_},{integer,Ln,X}|Ts], Acc) -> parse_args(Ln,Ts,[-X|Acc]);
parse_args(Ln, [{integer,Ln,X}|Ts], Acc) -> parse_args(Ln,Ts,[X|Acc]);
parse_args(Ln, [{',',Ln}|Ts], Acc) -> parse_args(Ln, Ts, Acc);
parse_args(Ln, [{')',Ln}|Ts], Acc) -> {lists:reverse(Acc), Ts};
parse_args(Ln, [{';',Ln}|Ts], Acc) -> {lists:reverse(Acc), Ts};
parse_args(Ln, Ts=[{_,Ln1}|_], Acc) 
  when Ln =/= Ln1 -> {lists:reverse(Acc), Ts};
parse_args(Ln, Ts=[{_,Ln1,_}|_], Acc) 
  when Ln =/= Ln1 -> {lists:reverse(Acc), Ts};
parse_args(_Ln, Ts=[{'}',_}|_], Acc) -> {lists:reverse(Acc), Ts};
parse_args(_Ln, [], Acc) -> {lists:reverse(Acc), []}.

pin({atom,_,p0}) -> {p,0};
pin({atom,_,p1}) -> {p,1};
pin({atom,_,x0}) -> {x,0};
pin({atom,_,x1}) -> {x,1};
pin({atom,_,x2}) -> {x,2};
pin({atom,_,x3}) -> {x,3}.

scan(Text) ->
    Text1 = remove_comments(Text),
    case erl_scan:string(Text1) of
	{ok,Ts,_Ln} -> {ok,Ts};
	Error = {error,_} -> Error
    end.

%% remove # comments
remove_comments(Text) ->
    string:join(re:split(Text, "#.*\n",[{return,list}]), "\n").
