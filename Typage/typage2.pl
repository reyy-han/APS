% executer : swipl -> [typage2].
% le contexte doit avoir initctx([(true,bool),(false,bool),...])

%[cs]:void__ Prog * {void}
%cs:void___ G * Cmds * {void}
%s:void___ G * Stat * {void}

% Initialise le Contexte G
initctx([(true, bool), (false, bool), (not,arc([bool],bool)), (and, arc([bool,bool],bool)),
	(or, arc([bool,bool],bool)), (eq, arc([int,int],bool)), (lt, arc([int,int],bool))
	, (add, arc([int,int],int)), (sub, arc([int,int],int)), (mul, arc([int,int],int))
	, (div, arc([int,int],int))]).

tyProg(P,T,ok):- testProg(P,T).
tyProg(_,_,ko).

% Fonction de test pour verifier les typages
testProg(P,T):-  initctx(G), typeProg(G,P,T).
testBlock(B,T):-  initctx(G), typeBlock(G,B,T).
testCmds(C,T):- initctx(G), typeCmds(G,C,T).
testInstr(S,T):- initctx(G), typeInstr(G,S,T).
testDef(D,NG):- initctx(G), typeDef(G,D,NG).
testExpr(E,T):- initctx(G), typeExpr(G,E,T).

% Regarde si G contient le couple (E,T), utile pour chercher dans le contexte.
contient(E,T,G):- member((E,T),G),!.

% Depuis une liste d'exprs, on verifie le typage de chaque element.
listExpr(_,[],[]).
listExpr(G,[L|LS],[T1|T2]):- typeExpr(G,L,T1), listExpr(G,LS,T2).

% version pour les parametres d'appel
listExpar(_,[],[]).
listExpar(G,[L|LS],[T1|T2]):- typeExpar(G,L,T1), listExpar(G,LS,T2).


% ajout de variables dans le contexte G -> retourne R le nouveau contexte
appendArg([],G,G).
appendArg([(id(V),T)|LS],G,[(V,T)|R]):- appendArg(LS,G,R).
%---------------------------------------------------------------------------------------------------%


% permet de le typage correct
getA([],[]).
getA([(var_id(X),T)|PS],[(id(X),ref(T))|XS]):- getA(PS,XS).
getA([(P,T)|PS],[(P,T)|XS]):- getA(PS,XS).


% ----------------------------------------------PROGRAMMES-------------------------------------------%
% [cs]:void___PROG * {void}
typeProg(G,prog(Block),void):- typeBlock(G,Block,void),!.
%---------------------------------------------------------------------------------------------------%


% ------------------------------------------------BLOCS---------------------------------------------%
% [cs]:void___PROG * {void}
typeBlock(G,CS,void):- typeCmds(G,CS,void),!.

%---------------------------------------------------------------------------------------------------%



% ------------------------------------------SUITE-DE-COMMANDES---------------------------------------%
% % cs:void___ G * CMDS * {void}

% (DECS) DEF; CMDS
typeCmds(G,[D|CS],void):- typeDef(G,D,NG),typeCmds(NG,CS,void),!.
% STAT% (STATS)
typeCmds(G,[S|CS],void):- typeInstr(G,S,void), typeCmds(G,CS,void),!.
% (END) ?
typeCmds(_,[],void).

%---------------------------------------------------------------------------------------------------%
%



% --------------------------------------------INSTRUCTIONS-------------------------------------------%
% s:void___ G * Stat * {void}
% ECHO EXPR
typeInstr(G,echo(E),void):- typeExpr(G,E,int),!.


% SET lvalue EXPR
% %lvalue_id() ???
typeInstr(G,set(E1,E2),void):- typeLvalue(G,E1,T), typeExpr(G,E2,T).
% IF EXPR BLOCK BLOCK
typeInstr(G,stat_if(E,B1,B2),void):- typeExpr(G,E,bool),
    typeBlock(G,B1,void), typeBlock(G,B2,void).
% WHILE EXPR BLOCK
typeInstr(G,while(E,B),void):- typeExpr(G,E,bool), typeBlock(G,B,void).
% CALL x EXPRSP
typeInstr(G,call(id(X),ES),void):- listExpar(G,ES,TS), contient(X,arc(TS,void),G),!.

%---------------------------------------------------------------------------------------------------%


% --------------------------------------------DEFINITIONS-------------------------------------------%
% ----------------Fonctions--AUX----------------%
%liste des types%

getTypes([],[]).
getTypes([(_,T)|LA],[T|LT]):- getTypes(LA,LT).
%
aux_funrec(G,X,TF,LA,E,LT):- appendArg(LA,G,NG), getTypes(LA,LT), appendArg([(X,arc(LT,TF))],NG,NG2), typeExpr(NG2,E,TF).

%---------------------------------------------------------------------------------------------------%
% d:nv ctx___ G * Def * G

% CONST ident TYPE EXPR
typeDef(G,const(id(X),T,E),NG):- typeExpr(G,E,T), appendArg([(id(X),T)],G,NG),!.
% FUN ident TYPE [ARGS] EXPR
typeDef(G,fun(id(F),TF,LA,E),NG):- typeExpr(G,lambda(LA,E),TF), getTypes(LA,TS),appendArg([(id(F),arc(TS,TF))],G,NG),!.

% FUN REC ident TYPE [ARGS] EXPR
typeDef(G,funrec(id(F),TF,LA,E),NG):- aux_funrec(G,id(F),TF,LA,E,TS), appendArg([(id(F),arc(TS,TF))],G,NG),!.

% VAR ident TYPE
typeDef(G,var(id(X),int),[(X,ref(int))|G]).
typeDef(G,var(id(X),bool),[(X,ref(bool))|G]).

% PROC x [ARGSP] BLOCK
typeDef(G,proc(id(P),Args,B),NG):- getA(Args,TS), appendArg(TS,G,Gbis),
    typeBlock(Gbis,B,void), getTypes(TS,TTS), appendArg([(id(P),arc(TTS,void))],G,NG),!.

% PROC REC x [ARGSP] BLOCK
typeDef(G,procrec(id(P),Args,B),NG):- getA(Args,TS),appendArg(TS,G,Gbis),getTypes(TS,TTS),
	appendArg([(id(P),arc(TTS,void))],Gbis,Gbis2),
	typeBlock(Gbis2,B,void),appendArg([(id(P),arc(TTS,void))],G,NG),!.

% LVALUE
% (LVAR)
typeLvalue(G,id(X),T):- contient(X,ref(T),G).
% (LNTH)
typeLvalue(G,lvnth(E1,E2),T):-typeExpr(G,nth(E1,E2),T).




%---------------------------------------------------------------------------------------------------%
%




% ---------------------------------------------EXPRESSIONS-------------------------------------------%
% PARAMETRES D'APPEL
% (REF)
typeExpar(G,adr(X),ref(T)):- contient(X,ref(T),G).
% (VAL)
typeExpar(G,E,T):- typeExpr(G,E,T).
%---------------------------------------------------------------------------------------------------%

% % e:t___ G * Expr *Type

% ( NUM )
typeExpr(_,num(N),int):- integer(N).
% ( IDR )
typeExpr(G,id(X),T):- contient(X,ref(T),G),!.
% ( IDV )
typeExpr(G,id(X),T):- contient(X,T,G).
% ( if EXPR EXPR EXPR )
typeExpr(G,if(E1,E2,E3),T):- typeExpr(G,E1,bool), typeExpr(G,E2,T), typeExpr(G,E3,T),!.
% ( EXPR EXPRS )
typeExpr(G,app(Op,L),TR):- typeExpr(G,Op,arc(T,TR)), listExpr(G,L,T),!.
% ( [ARGS] EXPR )
typeExpr(G,lambda(VS,E),T):- appendArg(VS,G,NG), typeExpr(NG,E,T),!.
% (ALLOC)
typeExpr(G,alloc(E),vec(_)):- typeExpr(G,E,int).
% (LEN)
typeExpr(G,len(E),int):- typeExpr(G,E,vec(_)).
% (NTH)
typeExpr(G,nth(E1,E2),T):- typeExpr(G,E1,vec(T)), typeExpr(G,E2,int).
% (VSET)
typeExpr(G,vset(E1,E2,E3),vec(T)):- typeExpr(G,E1,vec(T)),
    typeExpr(G,E2,int), typeExpr(G,E3,T).
%

%---------------------------------------------------------------------------------------------------%

exitCode(ok):- halt(0).
exitCode(ko):- halt(1).

main :-
    %write("Ecrit un programme: "),nl,
    read(Prog),
    print(Prog),nl,
    tyProg(Prog,Type,R),
    write("|> test typage : "),
    print(R),nl,
    write("|> Type: "),
    print(Type),nl,
    exitCode(R).
