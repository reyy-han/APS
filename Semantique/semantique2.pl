% executer : swipl -> [sematique2].


exeProg(P,Mem,O,ok):- evalProg(P,Mem,O).
exeProg(_,_,_,ko).

% ---------------------------------------------PROGRAMMES---------------------------------------------%
% Programmes %
% |- p ~> (sigma,w)
evalProg(prog(BK),Mem,O):- evalBlock([],[],[],BK,Mem,O),!.

%-----------------------------------------------------------------------------------------------------%



% -----------------------------------------------BLOCKS-----------------------------------------------%
% Blocs %
% p,sigma,w |- bk ~> (sigma',w')
evalBlock(Env,Mem,O,BK,MemI,OI):- evalCmds(Env,Mem,O,BK,MemI,OI),!.
%-----------------------------------------------------------------------------------------------------%




% -----------------------------------------SUITE-DE-COMMANDES-----------------------------------------%
% Suites de commandes %
% p,sigma,w |- cs ~> (sigma',w')

% (END)
evalCmds(_,Mem,O,[],Mem,O).
% (STATS)
evalCmds(Env,Mem,O,[Stat|CS],MemII,OII):- evalStat(Env,Mem,O,Stat,MemI,OI),
    evalCmds(Env,MemI,OI,CS,MemII,OII).
% (DECS)
evalCmds(Env,Mem,O,[Def|CS],MemII,OI):- evalDef(Env,Mem,Def,EI,MemI),
    evalCmds(EI,MemI,O,CS,MemII,OI).
%-----------------------------------------------------------------------------------------------------%
%



% --------------------------------------------INSTRUCTIONS--------------------------------------------%
% setlist permet de changer la valeur d'une adresse (attention elle
% change la valeur de chaque occurence de l'adresse X)
% par ex: setlist([(x,any),(b,2)],(x,1),R) retourne R = [(x,1),(b,2)]
% et setlist([(x,any),(x,42),(b,2)],(x,1),R) retourne R = [(x,1),(x,1),(b,2)]
setlist([],_,[]).
setlist([(X,_)|T],(X,N),[(X,N)|R]):- setlist(T,(X,N),R).
setlist([E|T],(X,N),[E|R]):- E\=(X,N),setlist(T,(X,N),R).
%-----------------------------------------------------------------------------------------------------%
%
% Instructions %
% p,sigma,w |- s ~> (sigma',w')
% (ECHO)
evalStat(Env,Mem,O,echo(E),MemI,[R|O]):- evalExpr(Env,Mem,E,R,MemI).
% (SET)
evalStat(Env,Mem,O,set(LV,E),MemII,O):-
    evalExpr(Env,Mem,E,V,MemI),
    evalLvalue(Env,MemI,LV,A,MemIIbis),
    setlist(MemIIbis,(A,V),MemII).

% (STAT_IF1)
evalStat(Env,Mem,O,stat_if(E,BK1,_),MemII,OI):- evalExpr(Env,Mem,E,1,MemI), evalBlock(Env,MemI,O,BK1,MemII,OI).
% (STAT_IF0)
evalStat(Env,Mem,O,stat_if(E,_,BK2),MemII,OI):- evalExpr(Env,Mem,E,0,MemI), evalBlock(Env,MemI,O,BK2,MemII,OI).
% (WHILE)
evalStat(Env,Mem,O,while(E,_),MemI,O):- evalExpr(Env,Mem,E,0,MemI).
evalStat(Env,Mem,O,while(E,BK),MemIII,OII):- evalExpr(Env,Mem,E,1,MemI),
    evalBlock(Env,MemI,O,BK,MemII,OI),evalStat(Env,MemII,OI,while(E,BK),MemIII,OII).
% (CALL)
evalStat(Env,Mem,O,call(id(X),ES),MemII,OI):-
    contient(X,inP(BK,XS,EnvI),Env),
    calcule_valeurs_expar(Env,Mem,ES,VS,MemI),
    att_val(XS,VS,Dico),
    append(Dico,EnvI,EnvII),
    evalBlock(EnvII,MemI,O,BK,MemII,OI).
% (CALLR)
evalStat(Env,Mem,O,call(id(X),ES),MemII,OI):-
    contient(X,inPR(BK,X,XS,EnvI),Env),
    calcule_valeurs_expar(Env,Mem,ES,VS,MemI),
    att_val(XS,VS,Dico),
    append(Dico,EnvI,EnvII),
    evalBlock([(X,inPR(BK,X,XS,EnvI))|EnvII],MemI,O,BK,MemII,OI).

%-----------------------------------------------------------------------------------------------------%




% --------------------------------------------DEFINITIONS---------------------------------------------%
% Pour verifier que a n'est pas deja une adresse definie dans la memoire Mem
%notin(A,Mem):- \+ member(A,Mem).
notin(0,[]).
notin(A,[(X,_)|_]):- A is X + 1.

getX([],[]).
getX([(var_id(XI),_)|LS],[id(XI)|R]):- getX(LS,R).
getX([(PI,_)|LS],[PI|R]):- getX(LS,R).
%
% Definitions %
% p,sigma |- d ~> (p',sigma')
%
% (CONST)
evalDef(Env,Mem,const(id(X),_,E),[(X,V)|Env],MemI):- evalExpr(Env,Mem,E,V,MemI).
% (FUN)
evalDef(Env,Mem,fun(id(F),_,LA,E),[(F,inF(E,VS,Env))|Env],Mem):- listArg(LA,VS).
% (FUNREC)
evalDef(Env,Mem,funrec(id(F),_,LA,E),[(F,inFR(E,F,VS,Env))|Env],Mem):- listArg(LA,VS).
% (VAR)
evalDef(Env,Mem,var(id(X),_),[(X,inA(A))|Env],[(A,any)|Mem]):- notin(A,Mem).
% (PROC)
evalDef(Env,Mem,proc(id(P),Args,B),[(P,inP(B,AS,Env))|Env],Mem):- getX(Args,AS).
% (PROCREC)
evalDef(Env,Mem,procrec(id(P),Args,B),[(P,inPR(B,P,AS,Env))|Env],Mem):- getX(Args,AS).

%-----------------------------------------------------------------------------------------------------%



% -----------------------------------------PARAMETRES-D-APPEL-----------------------------------------%
% (REF)
evalExpar(Env,Mem,adr(E),inA(A),Mem):-contient(E,inA(A),Env).
% (VAL)
evalExpar(Env,Mem,E,V,MemI):-evalExpr(Env,Mem,E,V,MemI).

%-----------------------------------------------------------------------------------------------------%


% -----------------------------------------------LVALUE-----------------------------------------------%
% (LID)
evalLvalue(Env,Mem,id(X),A,Mem):- contient(X,inA(A),Env).
% (LNTH1)
evalLvalue(Env,Mem,lvnth(id(X),E),AI,MemI):- contient(X,inB(A,_),Env), evalExpr(Env,Mem,E,I,MemI), AI is A + I.
% (LNTH2)
evalLvalue(Env,Mem,lvnth(LV,E),A,MemII):- evalLvalue(Env,Mem,LV,A1,MemI),
    contient(A1,inB(A2,_),MemI), evalExpr(Env,MemI,E,I,MemII), A is A2+I.
%-----------------------------------------------------------------------------------------------------%





%---------------------------------------------EXPRESSIONS---------------------------------------------%
% Fonctions Auxiliaires :
% -----------------------
% contient : Regarde si Env contient le couple (E,V), utile pour
% chercher dans le contexte.
contient(X,V,[(X,V)|_]).
contient(X,V,[(_,_)|Env]):- contient(X,V,Env),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recupere le valeur d'indice X dans le tableau (liste) donn'e en
% parametre.
getI(X,[(X,V)|_],V).
getI(X,[(_,_)|Env],R):- getI(X,Env,R),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Permet de creer un tableau de taille N et d'adresse de depart A.
tableau(0,_,[]).
tableau(N,A,[(X,any)|XS]):- N>0,X is (A + N -1), S is N-1, tableau(S,A,XS).
allocn(Mem,N,A,MemI):- notin(A,Mem), tableau(N,A,T), append(T,Mem,MemI).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% listArg : prend une liste de tuples tq [(x,1),(d,2)] et retourne la
% liste [x, d].
listArg([],[]).
listArg([(V,_)|LS],[V|R]):- listArg(LS,R).

% calcule_valeurs : calcule et stocke dans une liste les valeurs des
% expressions qu'on lui donne en paramtre
%(par exemple: calcule_valeurs([(x,1),(d,2)],[num(4),id(x)],R). R = [4, 1].)
calcule_valeurs(_,Mem,[],[],Mem).
calcule_valeurs(Env,Mem,[Expr|ListExprs],[Valeur|ValeursExprs],MemII):-
    evalExpr(Env,Mem,Expr,Valeur,MemI),
    calcule_valeurs(Env,MemI,ListExprs,ValeursExprs,MemII).


calcule_valeurs_expar(_,Mem,[],[],Mem).
calcule_valeurs_expar(Env,Mem,[Expar|ListExpars],[Valeur|ValeursExpars],MemII):-
    evalExpar(Env,Mem,Expar,Valeur,MemI),
    calcule_valeurs_expar(Env,MemI,ListExpars,ValeursExpars,MemII).

% att_val : prend une liste de cls (attributs) et une liste de valeurs
% et retourne un dictionnaire qui lie les cls et les valeurs
% correspondantes (attention : on suppose que les deux listes sont de la
% meme taille)
att_val([],[],[]).
att_val([id(A)|Attrs],[V|Valeurs],[(A,V)|Dico]):- att_val(Attrs,Valeurs,Dico).
%-----------------------------------------------------------------------------------------------------%

% Expressions %
% p, sigma |- e ~> v
%
% (TRUE)
evalExpr(_,Mem,id(true),1,Mem):-!.
% (FALSE)
evalExpr(_,Mem,id(false),0,Mem):-!.
% (NUM)
evalExpr(_,Mem,num(N),N,Mem):- integer(N).
% (ID1)
evalExpr(Env,Mem,id(X),V,Mem) :- contient(X,inA(A),Env), contient(A,V,Mem).
% (ID2)
evalExpr(Env,Mem, id(X),V,Mem) :- contient(X, V, Env), V\=inA(_).


% Fonctions primitives %
% (PRIM1)
evalExpr(Env,Mem,app(id(not),[E]),0,MemI):- E = id(_), evalExpr(Env,Mem,E,1,MemI),!.
evalExpr(Env,Mem,app(id(not),[E]),1,MemI):- E = id(_), evalExpr(Env,Mem,E,0,MemI),!.
% (PRIM2)
evalExpr(Env,Mem,app(id(add),[E1,E2]),R,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII),R is R1+R2.
evalExpr(Env,Mem,app(id(mul),[E1,E2]),R,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII),R is R1*R2.
evalExpr(Env,Mem,app(id(div),[E1,E2]),R,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII),R is R1/R2.
evalExpr(Env,Mem,app(id(sub),[E1,E2]),R,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII),R is R1-R2.
evalExpr(Env,Mem,app(id(eq),[E1,E2]),1,MemII):- evalExpr(Env,Mem,E1,R,MemI),
    evalExpr(Env,MemI,E2,R,MemII),!.
evalExpr(Env,Mem,app(id(eq),[E1,E2]),0,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII), R1\=R2.
evalExpr(Env,Mem,app(id(lt),[E1,E2]),1,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII),R1 < R2,!.
evalExpr(Env,Mem,app(id(lt),[E1,E2]),0,MemII):- evalExpr(Env,Mem,E1,R1,MemI),
    evalExpr(Env,MemI,E2,R2,MemII), R1 >= R2.
% -------------------- %

% (APP)
evalExpr(Env,Mem,app(Func,Exprs),V,MemIII):-
    evalExpr(Env,Mem,Func,inF(ExpFunc,Attrs,EnvFunc),MemI),
    calcule_valeurs(Env,MemI,Exprs,Valeurs,MemII),
    att_val(Attrs,Valeurs,D),
    append(D,EnvFunc,NewEnv),
    evalExpr(NewEnv,MemII,ExpFunc,V,MemIII).
% (APPR)
evalExpr(Env,Mem,app(Func,Exprs),V,MemIII):-
    evalExpr(Env,Mem,Func,inFR(ExpFunc,X,Attrs,EnvFunc),MemI),
    calcule_valeurs(Env,MemI,Exprs,Valeurs,MemII),
    att_val(Attrs,Valeurs,D),
    append(D,EnvFunc,NewEnv),
    evalExpr([(X,inFR(ExpFunc,X,Attrs,EnvFunc))|NewEnv],MemII,ExpFunc,V,MemIII).

% (IF)
evalExpr(Env,Mem,if(E1,E2,_),R,MemII):- evalExpr(Env,Mem,E1,1,MemI),evalExpr(Env,MemI,E2,R,MemII),!.
evalExpr(Env,Mem,if(E1,_,E3),R,MemII):- evalExpr(Env,Mem,E1,0,MemI),evalExpr(Env,MemI,E3,R,MemII).
% (ABS)
evalExpr(Env,Mem,lambda(VTS,E),inF(E,VS,Env),Mem):- listArg(VTS,VS).
% (AND)
evalExpr(Env,Mem,and(E1,_),0,MemI):- evalExpr(Env,Mem,E1,0,MemI).
evalExpr(Env,Mem,and(E1,E2),R,MemII):- evalExpr(Env,Mem,E1,1,MemI),
    evalExpr(Env,MemI,E2,R,MemII),!.
% (OR)
evalExpr(Env,Mem,or(E1,_),1,MemI):- evalExpr(Env,Mem,E1,1,MemI).
evalExpr(Env,Mem,or(E1,E2),R,MemII):- evalExpr(Env,Mem,E1,0,MemI),evalExpr(Env,MemI,E2,R,MemII).

% (ALLOC)
evalExpr(Env,Mem,alloc(E),inB(A,N),MemII):- evalExpr(Env,Mem,E,N,MemI), N>0,
    allocn(MemI,N,A,MemII).
% (LEN)
evalExpr(Env,Mem,len(E),N,MemI):- evalExpr(Env,Mem,E,inB(_,N),MemI).
% (NTH)
evalExpr(Env,Mem,nth(E1,E2),R,MemII):-
    evalExpr(Env,Mem,E1,inB(A,_),MemI),
    evalExpr(Env,MemI,E2,I,MemII),
    AI is A+I, getI(AI,MemII,R).
% (VSET)
evalExpr(Env,Mem,vset(E1,E2,E3),inB(A,N),MemIII):-
    evalExpr(Env,Mem,E1,inB(A,N),MemI),
    evalExpr(Env,MemI,E2,I,MemII),
    evalExpr(Env,MemII,E3,V,MemIIbis),
    AI is A+I, setlist(MemIIbis,(AI,V),MemIII).
%-----------------------------------------------------------------------------------------------------

exitCode(ok):- halt(0).
exitCode(ko):- halt(1).


% Lancer le main permet de donner manuellement un programme (attention 
% bien ecrire le programme, cad de type: prog([...]))
main :-
    read(Prog),
    print(Prog),nl,
    exeProg(Prog,Mem,O,Res),
    write("|> test semantique : "),
    print(Res),nl,    
    write("|> Memoire: "),
    print(Mem),nl,
    write("|> Resultat: "),
    print(O),nl,
    exitCode(Res).
