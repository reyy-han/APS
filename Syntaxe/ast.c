/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe C                                                  == */
/* == Fichier: ast.c                                                       == */
/* ==  Arbre de syntaxe abstraite                                          == */
/* ========================================================================== */
#include <stdbool.h> 
#include <stdlib.h>
#include <stdio.h>
#include "ast.h"


/* Expressions */
Expr newASTNum(int v) {
  Expr r = mallocExpr;
  r->tag = ASTNum;
  r->content.num = v;
  return r;
}

Expr newASTId(char* v) {
  Expr r = mallocExpr;
  r->tag = ASTId;
  r->content.id = v;
  return r;
}

Expr newASTApp(Expr e, Exprs es) {
  Expr r = mallocExpr;
  r->tag = ASTApp;
  r->content.app.fun = e;
  r->content.app.args = es;
  return r;
}

Expr newASTIf(Expr cond, Expr then, Expr el) {
	Expr r = mallocExpr;
	r->tag = ASTIf;
	r->content.si.cond = cond;
	r->content.si.then = then;
	r->content.si.el = el;
	return r;
}

Expr newASTAnd(Expr e1, Expr e2) {
	Expr r = mallocExpr;
	r->tag = ASTAnd;
	r->content.binaire.e1 = e1;
	r->content.binaire.e2 = e2;
	return r;
}

Expr newASTOr(Expr e1, Expr e2) {
	Expr r = mallocExpr;
	r->tag = ASTOr;
	r->content.binaire.e1 = e1;
	r->content.binaire.e2 = e2;
	return r;
}

Expr newASTLambda(Args as, Expr ea) {
  Expr r = mallocExpr; 
  r->tag = ASTLambda; 
  r->content.lambda.as = as; 
  r->content.lambda.ea = ea; 
  return r;
}

Expr newASTAlloc(Expr expr) {
  Expr r = mallocExpr; 
  r->tag = ASTAlloc; 
  r->content.alloc.expr = expr;
  return r;
}

Expr newASTLen(Expr expr) {
  Expr r = mallocExpr; 
  r->tag = ASTLen; 
  r->content.len.expr = expr; 
  return r;
}

Expr newASTNth(Expr e1, Expr e2) {
  Expr r = mallocExpr; 
  r->tag = ASTNth; 
  r->content.nth.expr = e1; 
  r->content.nth.indice = e2; 
  return r;
}

Expr newASTVset(Expr e1, Expr e2, Expr e3) {
  Expr r = mallocExpr; 
  r->tag = ASTVset; 
  r->content.vset.vs1 = e1;
  r->content.vset.indice = e2; 
  r->content.vset.vs3 = e3; 
  return r;
}

Exprs addExpr(Expr e, Exprs es) {
  Exprs r = mallocExprs;
  r->head = e;
  r->tail = es;
  return r;
}

/* Parametres d'appels  */

Exprp newASTExprP(Expr e) {
  Exprp ep = mallocExprp;
  ep -> tag = ASTExprP;
  ep -> content.expr.e = e; 
  return ep;
}
Exprp newASTIdP(char* idp) {
  Exprp ep = mallocExprp;
  ep -> tag = ASTIdP;
  ep -> content.idp = idp; 
  return ep;  
}

Exprsp addExprp(Exprp ep, Exprsp esp) {
  Exprsp rp = mallocExprsp;
  rp -> head = ep;
  rp -> tail = esp;
  return rp;
}

/* Statements */
Stat newASTEcho(Expr e) {
  Stat r = mallocStat;
  r->tag = ASTecho;
  r->content.echo.expr = e;
  return r;
}

Stat newASTSet(Lvalue lvalue, Expr e) {
  Stat r = mallocStat;
  r->tag = ASTSet;
  r->content.set.lvalue = lvalue;
  r->content.set.expr = e;
  return r;
}

Stat newASTStatIf(Expr e, Block b1, Block b2){
  Stat r = mallocStat;
  r->tag = ASTStatIf;
  r->content._if.expr = e;
  r->content._if.b1 = b1;
  r->content._if.b2 = b2;
  return r;
}

Stat newASTWhile(Expr e, Block b) {
  Stat r = mallocStat;
  r->tag = ASTWhile;
  r->content.boucle_while.expr = e;
  r->content.boucle_while.block = b;
  return r;
}

Stat newASTCall(char* ident, Exprsp es) {
  Stat r = mallocStat;
  r->tag = ASTCall;
  r->content.call.ident = ident;
  r->content.call.exprsp = es;
  return r;
}

/* Tableaux */

Lvalue newASTlvalue(char* ident) {
  Lvalue r = mallocLvalue;
  r->tag = ASTlvalue;
  r->content.id = ident;
  return r;
}

Lvalue newASTtab(Lvalue lvalue, Expr expr) {
  Lvalue r = mallocLvalue;
  r->tag = ASTtab;
  r->content.tableau.lvalue = lvalue;
  r->content.tableau.expr = expr;
  return r;
}


/* Arguments de procedures */

Argp newASTVarId(char* ident, Type t) {
  Argp r = mallocArgp;
  r -> tag = ASTVarId;
  r -> idp = ident;
  r -> tp = t;
  return r;
} 


Argp newASTIdentPointer(char* ident, Type t) {
  Argp r = mallocArgp;
  r -> tag = ASTIdentP;
  r -> idp = ident;
  r -> tp = t;
  return r;
} 

Argsp addArgp(Argp ap, Argsp asp) {
  Argsp r = mallocArgsp;
  r -> head = ap;
  r -> tail = asp;
  return r;
}

/* Arguments de fonctions */ 
Arg newASTIdent(char* ident, Type type) {
  Arg r = mallocArg;
  r->tag = ASTIdent;
  r->id = ident;
  r->t = type;
  return r;
}

Args addArg(Arg arg, Args args) {
  Args r = mallocArgs;
  r->head = arg;
  r->tail = args;
  return r;
}


/* Types et Stype */
SType newASTBool(bool b) {
	SType r = mallocSType;
	r->tag = ASTBool;
	r->content.b = b;
	return r;
}

SType newASTInt(int n) {
	SType r = mallocSType;
	r->tag = ASTInt;
	r->content.entier = n;
	return r;
}

SType newASTVec(SType stype) {
	SType r = mallocSType;
	r->tag = ASTVec;
	r->content.stype = stype;
	return r;
}

Type newASTTypeFun(Types types,Type type) {
	Type r = mallocType;
	r->tag = ASTTypeFun;
	r->content.function.types = types;
	r->content.function.type = type;
	return r;
}

Types addType(Type t, Types ts) {
	Types r = mallocTypes;
	r->head = t; 
	r->tail = ts; 
	return r; 
}


Type newASTSType(SType stype) {
	Type r = mallocType;
	r->tag = ASTSType;
	r->content.stype = stype;
	return r;
}


/* Definitions */
Def newASTConst(char* id, Type t, Expr e) {
  Def r = mallocDef; 
  r->tag = ASTConst;
  r->ident = id;
  r->content.constant.t = t;	
  r->content.constant.e = e;
  return r; 
} 

Def newASTFun(char* ident, Type type, Args args, Expr e) {
	Def r = mallocDef;
  r->tag = ASTFun;
  r->ident = ident;
  r->content.fun.t = type;
  r->content.fun.args = args;
  r->content.fun.e = e;
  return r;  
}

Def newASTFunRec(char* ident, Type type, Args args, Expr e) {
  Def r = mallocDef;
  r->tag = ASTFunRec;
  r->ident = ident;
  r->content.funrec.t = type;
  r->content.funrec.args = args;
  r->content.funrec.e = e;
  return r;  
}

Def newASTVar(char* ident, SType stype) {
  Def r = mallocDef;
  r->tag = ASTVar;
  r->ident = ident;
  r->content.var.t = stype;
  return r;  
}

Def newASTProc(char* ident, Argsp argsp, Block b) {
  Def r = mallocDef;
  r->tag = ASTProc;
  r->ident = ident;
  r->content.proc.argsp = argsp;
  r->content.proc.b = b;
  return r;  
}

Def newASTProcRec(char* ident, Argsp argsp, Block b) {
  Def r = mallocDef;
  r->tag = ASTProcRec;
  r->ident = ident;
  r->content.procrec.argsp = argsp;
  r->content.procrec.b = b;
  return r;  
}



/* Suite de commandes */
Cmd newStat(Stat s) {
  Cmd r = mallocCmd;
  r->tag = ASTstat;
  r->content.stat = s;
  return r;
}

Cmds addStat(Stat s, Cmds cs) {
  Cmds r = mallocCmds;
  r->head = newStat(s);
  r->tail = cs;
  return r;
}

Cmd newDef(Def d) {
  Cmd r = mallocCmd;
  r->tag = ASTDef;
  r->content.def = d;
  return r;
} 

Cmds addDef(Def d, Cmds cs) {
  Cmds r = mallocCmds;
  r->head = newDef(d);
  r->tail = cs;
  return r;
}


