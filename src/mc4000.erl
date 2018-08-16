%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    SHENZHEN IO, MC4000
%%% @end
%%% Created : 13 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(mc4000).

-export([new/2]).
-export([exec/1]).
-export([instructions/0,registers/0, pins/0, ports/0, irange/0, lrange/0]).

-define(EMPTY, {undefined,undefined}).
-define(JMP1,  {0,{jmp,1}}).

-record(mc,
	{
	 id,
	 state = init :: init | run | slp,
	 cycle = 0,
	 acc = 0 :: -999..999,
	 pc = 1 :: 1..9,
	 cnd = 0,
	 rd = ?EMPTY,  %% cached read
	 prog = {?JMP1,?JMP1,?JMP1,
		 ?JMP1,?JMP1,?JMP1,
		 ?JMP1,?JMP1,?JMP1}
	}).

registers() -> [acc,null].
pins() -> [{p,0}, {p,1}].
ports() -> [{x,0}, {x,1}].
irange() -> {-999,999}.
lrange() -> {1,9}.

instructions() ->
    [nop,
     {mov,[r,i],[r]}, 
     {jmp,[l]}, 
     {slp,[r,i]},
     {slx,[p]},
     {add,[r,i]}, 
     {sub,[r,i]}, 
     {mul,[r,i]}, 
     'not', 
     {dgt,[r,i]}, 
     {dst,[r,i],[r,i]},
     {teq,[r,i],[r,i]}, 
     {tgt,[r,i],[r,i]},
     {tlt,[r,i],[r,i]},
     {tcp,[r,i],[r,i]}].

%% create a machine instance 
new(Id,Is) when is_list(Is) ->
    case mcxxxx:compile(?MODULE,Is) of
	{ok,Is1} -> 
	    Len = length(Is1),
	    Pad = lists:duplicate(9-Len, ?JMP1),
	    #mc { id=Id, prog = list_to_tuple(Is1++Pad) };
	{error,Reason} ->
	    error(Reason)
    end.

exec(M) ->
    exec(element(1,M#mc.prog),1,0,M#mc { state=run }).

exec({C,Op},Pc,Cnd,M) when C =:= 0; C =:= Cnd ->
    io:format("mc4000:~w:exec: ~w ~p\n", [Pc,C,Op]),
    exec_(Op,Pc,Cnd,M);
exec(_,Pc,Cnd,M) ->
    next(Pc+1,Cnd,M).

exec_(nop,Pc,Cnd,M) ->
    next(Pc+1,Cnd,M);
exec_({mov,Src,Dst},Pc,Cnd,M) ->
    case rd(Src,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{S,M1} ->
	    case wr(S,Dst,M1) of
		block -> block(Pc,Cnd,M1);
		M2 -> nextc(Pc+1,Cnd,M2)
	    end
    end;
exec_({jmp,L},_Pc,Cnd,M) ->
    next(L,Cnd,M);
exec_({slp,Arg},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{S,M2} -> sleep(Pc,Cnd,M2,S)
    end;
exec_({slx,Arg},Pc,Cnd,M) ->
    case wire:wait({M#mc.id,Arg}) of
	block -> block(Pc,Cnd,M);
	ok -> next(Pc+1,Cnd,M)
    end;
exec_({add,Arg},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{S,M1} -> nextc(Pc+1,Cnd,M1#mc{acc=int(M1#mc.acc + S)})
    end;
exec_({sub,Arg},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{S,M1} -> nextc(Pc+1,Cnd,M1#mc{acc=int(M1#mc.acc - S)})
    end;
exec_({mul,Arg},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{S,M1} -> nextc(Pc+1,Cnd,M1#mc{acc=int(M1#mc.acc * S)})
    end;
exec_('not',Pc,Cnd,M) ->
    Acc1 = if M#mc.acc =:= 0 -> 100; true -> 0 end,
    next(Pc+1,Cnd,M#mc{acc=Acc1});
exec_({dgt,Arg},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{Pos,M1} ->
	    A = digit(Pos,M1#mc.acc),
	    nextc(Pc+1,Cnd,M1#mc{acc=A})
    end;
exec_({dst,Arg,Dgt},Pc,Cnd,M) ->
    case rd(Arg,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{Pos,M1} ->
	    case rd(Dgt,2,M1) of
		{block,M2} -> block(Pc,Cnd,M2);
		{D0,M2} ->
		    D = D0 rem 10,
		    P10 = pow10(Pos),
		    A0 = digit(Pos,M2#mc.acc)*P10,
		    A = if M2#mc.acc < 0 ->
				(M2#mc.acc + A0) - D*P10;
			   true ->
				(M2#mc.acc - A0) + D*P10
			end,
		    nextc(Pc+1,Cnd,M2#mc{acc=A})
	    end
    end;
exec_({teq,Src1,Src2},Pc,Cnd,M) ->
    case rd(Src1,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{A,M1} ->
	    case rd(Src2,2,M1) of
		{block,M2} -> block(Pc,Cnd,M2);
		{B,M2} ->
		    if A =:= B -> nextc(Pc+1,1,M2);
		       true -> nextc(Pc+1,-1,M2)
		    end
	    end
    end;
exec_({tgt,Src1,Src2},Pc,Cnd,M) ->
    case rd(Src1,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{A,M1} ->
	    case rd(Src2,2,M1) of
		{block,M2} -> block(Pc,Cnd,M2);
		{B,M2} ->
		    if A > B -> nextc(Pc+1,1,M2);
		       true -> nextc(Pc+1,-1,M2)
		    end
	    end
    end;
exec_({tlt,Src1,Src2},Pc,Cnd,M) ->
    case rd(Src1,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{A,M1} ->
	    case rd(Src2,2,M1) of
		{block,M2} -> block(Pc,Cnd,M2);
		{B,M2} ->
		    if A < B -> nextc(Pc+1,1,M2);
		       true -> nextc(Pc+1,-1,M2)
		    end
	    end
    end;
exec_({tcp,Src1,Src2},Pc,Cnd,M) ->
    case rd(Src1,1,M) of
	{block,M1} -> block(Pc,Cnd,M1);
	{A,M1} ->
	    case rd(Src2,2,M1) of
		{block,M2} -> block(Pc,Cnd,M2);
		{B,M2} ->
		    if A > B -> nextc(Pc+1,1,M2);
		       A < B -> nextc(Pc+1,-1,M2);
		       true -> nextc(Pc+1,0,M2)
		    end
	    end
    end.

nextc(Pc,Cnd,M) -> %% clear rd cache (commit)
    next(Pc,Cnd,M#mc { rd=?EMPTY }).

next(Pc,Cnd,M) when Pc > tuple_size(M#mc.prog) ->
    exec(element(1,M#mc.prog),1,Cnd,M);
next(Pc,Cnd,M) ->
    exec(element(Pc,M#mc.prog),Pc,Cnd,M).

block(Pc,Cnd,M) ->
    M#mc { state=slp, pc=Pc, cnd=Cnd, cycle=M#mc.cycle+1 }.

sleep(Pc,Cnd,M,Cycles) ->
    M#mc { state=slp, pc=Pc+1, cnd=Cnd, rd=?EMPTY, cycle=M#mc.cycle+Cycles }.

rd(acc,_Q,M)  -> {M#mc.acc,M};
rd(null,_Q,M) -> {0,M};
rd(I,_Q,M) when is_integer(I) -> {I,M};
rd(P={p,_},_Q,M) -> {wire:read({M#mc.id,P}),M};
rd(X={x,_},Q,M) -> xrd({M#mc.id,X},Q,M).

xrd(XPin,Q,M) ->
    case element(Q,M#mc.rd) of
	undefined ->
	    case wire:read(XPin) of
		block -> {block,M};
		V -> {V,M#mc { rd=setelement(Q,M#mc.rd,V) }}
	    end;
	V -> {V,M}
    end.

wr(S,acc,M) -> M#mc { acc=S };
wr(_S,null,M) -> M;
wr(S,P={p,_},M) -> wire:write({M#mc.id,P},S), M;
wr(S,X={x,_},M) -> xwr(S,{M#mc.id,X},M);
wr(_S,_,M) -> M.

xwr(S,XPin,M) ->
    case wire:write(XPin,S) of
	block -> block;
	ok -> M
    end.

digit(0,Value) -> abs(Value) rem 10;
digit(1,Value) -> (abs(Value) div 10) rem 10;
digit(2,Value) -> (abs(Value) div 100) rem 10;
digit(_,_Value) -> 0.

pow10(0) -> 1;
pow10(1) -> 10;
pow10(2) -> 100;
pow10(_) -> 0.

%% check overflow/underflow
int(I) when I < -999 -> -999;
int(I) when I > 999 -> 999;
int(I) -> I.
