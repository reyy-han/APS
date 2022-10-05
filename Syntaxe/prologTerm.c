/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe C                                                  == */
/* == Fichier: prologTerm.c                                                == */
/* == Génération du terme prolog                                           == */
/* ========================================================================== */

#include <stdio.h>
#include "ast.h"
#include "prologTerm.h"

void printExprs(Exprs);
void printExprsp(Exprsp);
void printTypes(Types ts);
void printType(Type t);
void printSType(SType st);
void printArgs(Args as);
void printArgsp(Argsp ap);
void printBlock(Block b);
void printLvalue(Lvalue l);



/* -------------------------- Expressions et suites d'expressions -------------------------- */
void printExpr(Expr e) {
  switch(e->tag) {
    case ASTNum : printf("num(%d)",getNum(e)); break;
    case ASTId : printf("id(%s)",getId(e)); break;
    case ASTApp : {
      printf("app(");
      printExpr(getFun(e));
      printf(",[");
      printExprs(getArgs(e));
      printf("])");
      break;
    }
    case ASTIf : {
      printf("if(");
      printExpr(getCond(e));
      printf(",");
      printExpr(getThen(e));
      printf(",");
      printExpr(getElse(e));
      printf(")");
      break;
    }
    case ASTAnd : {
      printf("and(");
      printExpr(getExpr1(e));
      printf(",");
      printExpr(getExpr2(e));
      printf(")");
      break;
    }
    case ASTOr : {
      printf("or(");
      printExpr(getExpr1(e));
      printf(",");
      printExpr(getExpr2(e));
      printf(")");
      break;
    }
    case ASTLambda : {
      printf("lambda([");
      printArgs(getArgsLambda(e)); 
      printf("],");
      printExpr(getExprArgs(e));
      printf(")"); 
      break;	
    }
    case ASTAlloc : {
      printf("alloc(");
      printExpr(getAlloc(e));
      printf(")");
      break;
    }
    case ASTLen : {
      printf("len(");
      printExpr(getLen(e));
      printf(")");
      break;
    }
    case ASTNth : {
      printf("nth(");
      printExpr(getNthExpr(e));
      printf(",");
      printExpr(getNthIndice(e));
      printf(")");
      break;
    }
    case ASTVset : {
      printf("vset(");
      printExpr(getVsetExpr1(e));
      printf(",");
      printExpr(getVsetIndice(e));
      printf(",");
      printExpr(getVsetExpr3(e));
      printf(")");
      break;
    }
  }
}

void printExprs(Exprs es) {
  if (es) {
    while (es->tail) {
      printExpr(es->head);
      printf(",");
      es = es->tail;
    };
    printExpr(es->head);
  }
}

/* -------------------------- Parametres d'appel  -------------------------- */
void printExprp(Exprp ep) {
  switch(ep->tag) {
    case ASTExprP : printExpr(getExprP(ep)); break;
    case ASTIdP : printf("adr(%s)",getIdP(ep)); break;	
  }
}

void printExprsp(Exprsp esp) {
  if (esp) {
    while (esp->tail) {
      printExprp(esp->head);
      printf(",");
      esp = esp->tail;
    };
    printExprp(esp->head);
  }
}


/* -------------------------- Statements -------------------------- */
void printStat(Stat s) {
  switch(s->tag) {
    case ASTecho : {
      printf("echo(");
      printExpr(getEchoExpr(s));
      printf(")");
      break;	
    }
    case ASTSet : {
      printf("set(");
      printLvalue(getLvalueSet(s));
      printf(",");
      printExpr(getSetExpr(s));
      printf(")");
      break;	
    }
    case ASTStatIf : {
      printf("stat_if(");
      printExpr(getIfExpr(s));
      printf(",");
      printBlock(getIfBlock1(s));
      printf(",");
      printBlock(getIfBlock2(s));
      printf(")");
      break;	
    }
    case ASTWhile : {
      printf("while(");
      printExpr(getWhileExpr(s));
      printf(",");
      printBlock(getWhileBlock(s));
      printf(")");
      break;	
    }
    case ASTCall : {
      printf("call(");
      printf("id(%s)",getIdentCall(s));
      printf(",[");
      printExprsp(getCallExprsp(s));
      printf("])");
      break;	
    }
  }
}

/* -------------------------- Tableaux -------------------------- */
void printLvalue(Lvalue l) {
  switch(l->tag) {
    case ASTlvalue: printf("id(%s)",getLvalueID(l)); break;
    case ASTtab: {
      printf("lvnth(");
      printLvalue(getLvalue(l));
      printf(",");
      printExpr(getLvalueExpr(l));
      printf(")");
      break;
    }
  }
}
/* -------------------------- Definitions -------------------------- */
void printDef(Def d) {
  switch(d->tag) {
    case ASTConst : {
      printf("const(");
      printf("id(%s)",getDefId(d));
      printf(",");
      printType(getDefType(d));
      printf(",");
      printExpr(getDefExpr(d));
      printf(")");
      break;
    }
    case ASTFun : {
      printf("fun(");
      printf("id(%s)",getDefId(d));
      printf(",");
      printType(getFunType(d));
      printf(",[");
      printArgs(getFunArgs(d));
      printf("],");
      printExpr(getFunExpr(d));
      printf(")");
      break;
    }
    case ASTFunRec : {
      printf("funrec(");
      printf("id(%s)",getDefId(d));
      printf(",");
      printType(getFunType(d));
      printf(",[");
      printArgs(getFunArgs(d));
      printf("],");
      printExpr(getFunExpr(d));
      printf(")");
      break;
    }
    case ASTVar : {
      printf("var(");
      printf("id(%s)",getDefId(d));
      printf(",");
      printSType(getVarType(d));
      printf(")");
      break;
    }
    case ASTProc : {
      printf("proc(");
      printf("id(%s)",getDefId(d));
      printf(",[");
      printArgsp(getProcArgsp(d));
      printf("],");
      printBlock(getProcBlock(d));
      printf(")");
      break;
    }
    case ASTProcRec : {
      printf("procrec(");
      printf("id(%s)",getDefId(d));
      printf(",[");
      printArgsp(getProcRecArgsp(d));
      printf("],");
      printBlock(getProcRecBlock(d));
      printf(")");
      break;
    }
  }
}


/* -------------------------- Suite de commandes -------------------------- */
void printCmd(Cmd c) {
  switch(c->tag) {
    case ASTstat : printStat(getStat(c)); break;
    case ASTDef : printDef(getDef(c)); break;
  }
}
  
void printCmds(Cmds cs) {
  printf("[");
  while (cs->tail) {
    printCmd(cs->head);
    printf(",");
    cs = cs->tail;
  }
  printCmd(cs->head);
  printf("]");
}

/* -------------------------- Blocks -------------------------- */
void printBlock(Block b) {
  printCmds(b);
}

/* -------------------------- Types -------------------------- */
void printType(Type t) {
  switch(t->tag) {
    case ASTSType : printSType(getSType(t)); break;
    case ASTTypeFun : {
      printf("arc(");
      printTypes(getTypesFun(t));
      printf(",");
      printType(getTypeFun(t));
      printf(")");
      break;
    }
  }
}


void printTypes(Types ts) {
  if (ts) {
    printf("[");
    while (ts->tail) {
      printType(ts->head);
      printf(",");
      ts = ts->tail;
    };
    printType(ts->head);
    printf("]");
  }
}

/* -------------------------- STypes -------------------------- */
void printSType(SType st) {
  switch(st->tag) {
    case ASTBool : printf("bool"); break;
    case ASTInt : printf("int"); break;
    case ASTVec : {
      printf("vec(");
      printSType(getVec(st));
      printf(")");
      break;
    }  
  }
}


/* -------------------------- Arguments de fonctions -------------------------- */
void printArg(Arg a) {
  printf("(");
  printf("id(%s)",getIdent(a));
  printf(",");
  printType(getType(a));
  printf(")");
}

void printArgs(Args as) {
  if (as) {
    while (as->tail) {
      printArg(as->head);
      printf(",");
      as = as->tail;
    };
    printArg(as->head);
  }
}

/* -------------------------- Arguments de procedures -------------------------- */
void printArgp(Argp ap) {
  switch(ap->tag) {
    case ASTVarId : {
      printf("(");
      printf("var_id(%s)",getIdentP(ap));
      printf(",");
      printType(getTypeP(ap));
      printf(")"); break;	
    }
    case ASTIdentP : {
      printf("(");
      printf("id(%s)",getIdentP(ap));
      printf(",");
      printType(getTypeP(ap));
      printf(")"); break;	
    }
  }	
}

void printArgsp(Argsp aps) {
  if (aps) {
    while (aps->tail) {
      printArgp(aps->head);
      printf(",");
      aps = aps->tail;
    };
    printArgp(aps->head);
  }
}


/* -------------------------- Programme -------------------------- */
void printProg(Prog bk) {
  if (bk) {
    printf("prog(");
    printBlock(bk);
    printf(")");
  }
}
