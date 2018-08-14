%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    SHENZHEN IO, MC6000
%%% @end
%%% Created : 13 Aug 2018 by Tony Rogvall <tony@rogvall.se>

-module(mc6000).

-export([new/2]).
-export([exec/1]).
-export([instructions/0, registers/0, pins/0, ports/0, irange/0, lrange/0]).

-record(mc,
	{
	 id,
	 state = init :: init | run | hlt | slp,
	 cycle = 0,
	 acc = 0 :: -999..999,
	 dat = 0 :: -999..999,
	 pc = 1 :: 1..14,
	 cnd = 0,
	 prog = {{0,hlt},{0,hlt},{0,hlt},{0,hlt},
		 {0,hlt},{0,hlt},{0,hlt},{0,hlt},
		 {0,hlt},{0,hlt},{0,hlt},{0,hlt},
		 {0,hlt},{0,hlt}}
	}).

registers() -> [acc,dat,null].
pins() -> [p0, p1].
ports() -> [x0, x1, x2, x3].
irange() -> {-999,999}.
lrange() -> {1,14}.
    
instructions() ->
    [nop, hlt,
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
	    Len = length(Is),
	    Pad = lists:duplicate(14-Len, {0,hlt}),
	    #mc { id=Id, prog = list_to_tuple(Is1++Pad) };
	{error,Reason} ->
	    error(Reason)
    end.

exec(M) ->
    exec(element(1,M#mc.prog),1,0,M#mc { state=run }).

exec({C,Op},Pc,Cnd,M) when C =:= 0; C =:= Cnd ->
    io:format("mc6000:~w:exec: ~w ~p\n", [Pc,C,Op]),
    exec_(Op,Pc,Cnd,M);
exec(_,Pc,Cnd,M) ->
    next(Pc+1,Cnd,M).

exec_(nop,Pc,Cnd,M) ->
    next(Pc+1,Cnd,M);
exec_(hlt,Pc,Cnd,M) ->
    M#mc { pc=Pc, cnd=Cnd };
exec_({mov,Src,Dst},Pc,Cnd,M) ->
    S = rd(Src,M),
    M1 = wr(S,Dst,M),
    next(Pc+1,Cnd,M1);
exec_({jmp,L},_Pc,Cnd,M) ->
    next(L,Cnd,M);
exec_({slp,Arg},Pc,Cnd,M) ->
    M1 = sleep(rd(Arg,M),M),
    next(Pc+1,Cnd,M1);
exec_({slx,Arg},Pc,Cnd,M) ->
    M1 = wait(Arg,M),
    next(Pc+1,Cnd,M1);
exec_({add,Arg},Pc,Cnd,M) ->
    next(Pc+1,Cnd,M#mc{acc=int(M#mc.acc + rd(Arg,M))});
exec_({sub,Arg},Pc,Cnd,M) ->
    next(Pc+1,Cnd,M#mc{acc=int(M#mc.acc - rd(Arg,M))});
exec_({mul,Arg},Pc,Cnd,M) ->
    next(Pc+1,Cnd,M#mc{acc=int(M#mc.acc * rd(Arg,M))});
exec_('not',Pc,Cnd,M) ->
    Acc1 = if M#mc.acc =:= 0 -> 100; true -> 0 end,
    next(Pc+1,Cnd,M#mc{acc=Acc1});
exec_({dgt,Arg},Pc,Cnd,M) ->
    %% 1. what about negative Acc? 
    %% 2. What if Pos is < 0 or > 2?
    Pos = rd(Arg,M),
    A = digit(Pos,M#mc.acc),
    next(Pc+1,Cnd,M#mc{acc=A});
exec_({dst,Arg,Dgt},Pc,Cnd,M) ->
    %% 1. What if Dgt < 0 or Dgt > 9 ?
    D = rd(Dgt,M) rem 10,
    Pos = rd(Arg,M),
    P10 = pow10(Pos),
    A0 = digit(Pos,M#mc.acc)*P10,
    A = if M#mc.acc < 0 ->
		(M#mc.acc + A0) - D*P10;
	   true ->
		(M#mc.acc - A0) + D*P10
	end,
    next(Pc+1,Cnd,M#mc{acc=A});
exec_({teq,Src1,Src2},Pc,_Cnd,M) ->
    A = rd(Src1,M),
    B = rd(Src2,M),
    if A =:= B -> next(Pc+1,1,M);
       true -> next(Pc+1,-1,M)
    end;
exec_({tgt,Src1,Src2},Pc,_Cnd,M) ->
    A = rd(Src1,M),
    B = rd(Src2,M),
    if A > B -> next(Pc+1,1,M);
       true -> next(Pc+1,-1,M)
    end;
exec_({tlt,Src1,Src2},Pc,_Cnd,M) ->
    A = rd(Src1,M),
    B = rd(Src2,M),
    if A < B -> next(Pc+1,1,M);
       true -> next(Pc+1,-1,M)
    end;
exec_({tcp,Src1,Src2},Pc,_Cnd,M) ->
    A = rd(Src1,M),
    B = rd(Src2,M),
    if A > B -> next(Pc+1,1,M);
       A < B -> next(Pc+1,-1,M);
       true -> next(Pc+1,0,M)
    end.

next(Pc,Cnd,M) when Pc > tuple_size(M#mc.prog) ->
    exec(element(1,M#mc.prog),1,Cnd,M);
next(Pc,Cnd,M) ->
    exec(element(Pc,M#mc.prog),Pc,Cnd,M).

sleep(Cycles,M) ->
    M#mc { cycle = M#mc.cycle + Cycles }.

wait(Port,M) -> 
    Cycle = wire:wait({M#mc.id,Port}),
    M#mc { cycle = M#mc.cycle + Cycle }.

    
rd(acc,M) -> M#mc.acc;
rd(dat,M) -> M#mc.dat;
rd(null,_M) -> 0;
rd(I,_M) when is_integer(I) -> I;
rd(p0,M) -> wire:read({M#mc.id,p0});
rd(p1,M) -> wire:read({M#mc.id,p1});
rd(x0,M) -> wire:read({M#mc.id,x0});
rd(x1,M) -> wire:read({M#mc.id,x1});
rd(x2,M) -> wire:read({M#mc.id,x2});
rd(x3,M) -> wire:read({M#mc.id,x3}).

wr(S,acc,M) -> M#mc { acc=S };
wr(S,dat,M) -> M#mc { dat=S };
wr(_S,null,M) -> M;
wr(S,p0,M) -> wire:write({M#mc.id,p0},S);
wr(S,p1,M) -> wire:write({M#mc.id,p1},S);
wr(S,x0,M) -> wire:write({M#mc.id,x0},S);
wr(S,x1,M) -> wire:write({M#mc.id,x1},S);
wr(S,x2,M) -> wire:write({M#mc.id,x2},S);
wr(S,x3,M) -> wire:write({M#mc.id,x3},S);
wr(_S,_,M) -> M.

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
