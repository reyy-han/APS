/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe C                                                  == */
/* == Fichier: ast.h                                                       == */
/* ==  Arbre de syntaxe abstraite                                          == */
/* ========================================================================== */

/* -------------------------- Expressions et suites d'expressions -------------------------- */
#include <stdbool.h> 

typedef struct _arg *Arg;
typedef struct _args *Args;

typedef struct _argp *Argp;
typedef struct _argsp *Argsp;

typedef struct _expr *Expr;
typedef struct _exprs *Exprs;

typedef struct _exprp *Exprp;
typedef struct _exprsp *Exprsp;
/* Block */
typedef struct _cmds *Block;
/* Lvalue */
typedef struct _lvalue *Lvalue;

enum expr_tag {
  ASTNum, ASTId, ASTApp, ASTIf, ASTAnd, ASTOr, ASTLambda, ASTAlloc, ASTNth, ASTLen, ASTVset
};

struct _expr {
  enum expr_tag tag;
  union {
    int num;
    char* id;
    struct {
      Expr fun;
      Exprs args;
    } app;
    struct {
      Expr cond;
      Expr then;
      Expr el;
    } si;
    struct {
      Expr e1;
      Expr e2;
    } binaire;
    struct {
      Args as; 
      Expr ea; 
    } lambda;
    struct {
      Expr expr;
    } alloc;
    struct {
      Expr expr;
    } len;
    struct {
      Expr expr;
      Expr indice;
    } nth;
    struct { 
      Expr vs1;
      Expr indice;
      Expr vs3;
    } vset;
  } content;
};

#define mallocExpr malloc(sizeof(struct _expr))

Expr newASTNum(int n);
Expr newASTId(char* x);
Expr newASTApp(Expr e, Exprs es);
Expr newASTIf(Expr cond, Expr then, Expr el);
Expr newASTAnd(Expr e1, Expr e2);
Expr newASTOr(Expr e1, Expr e2);
Expr newASTLambda(Args as, Expr ea);

#define getNum(e) e->content.num
#define getId(e) e->content.id

#define getFun(e) e->content.app.fun
#define getArgs(e) e->content.app.args

#define getCond(e) e->content.si.cond
#define getThen(e) e->content.si.then
#define getElse(e) e->content.si.el

#define getExpr1(e) e->content.binaire.e1
#define getExpr2(e) e->content.binaire.e2

#define getArgsLambda(e) e->content.lambda.as
#define getExprArgs(e) e->content.lambda.ea


Expr newASTAlloc(Expr expr);
Expr newASTLen(Expr expr);
Expr newASTNth(Expr e1, Expr e2);
Expr newASTVset(Expr e1, Expr e2, Expr e3);

#define getAlloc(e) e->content.alloc.expr
#define getLen(e) e->content.len.expr
#define getNthIndice(e) e->content.nth.indice
#define getNthExpr(e) e->content.nth.expr
#define getVsetExpr1(e) e->content.vset.vs1
#define getVsetIndice(e) e->content.vset.indice
#define getVsetExpr3(e) e->content.vset.vs3

struct _exprs {
  Expr head;
  Exprs tail;
};

#define mallocExprs malloc(sizeof(struct _exprs))

Exprs addExpr(Expr e, Exprs es);

/* -------------------------- Parametres d'appel -------------------------- */

enum exprp_tag {
  ASTExprP, ASTIdP
};

struct _exprp {
  enum exprp_tag tag;
  union {
    char* idp;
    struct {
      Expr e;
    } expr;
  } content;
};


#define mallocExprp malloc(sizeof(struct _exprp))
Exprp newASTExprP(Expr e);
Exprp newASTIdP(char* x);
#define getIdP(e) e->content.idp
#define getExprP(ep) ep->content.expr.e

struct _exprsp {
  Exprp head;
  Exprsp tail;
};
#define mallocExprsp malloc(sizeof(struct _exprsp))
Exprsp addExprp(Exprp ep, Exprsp esp);




/* -------------------------- Instructions -------------------------- */
typedef struct _stat *Stat;

enum stat_tag {
  ASTecho, ASTSet, ASTStatIf, ASTWhile, ASTCall
};

struct _stat {
  enum stat_tag tag;
  union {
    struct {
      Expr expr;
    } echo;
    struct {
      Lvalue lvalue;
      Expr expr;   
    } set;
    struct {
      Expr expr;   
      Block b1;
      Block b2;
    } _if;
    struct {
      Expr expr;   
      Block block;
    } boucle_while;
    struct {
      char* ident;
      Exprsp exprsp;
    } call;
  } content;
};

#define mallocStat malloc(sizeof(struct _stat))

Stat newASTEcho(Expr e);
Stat newASTSet(Lvalue lvalue, Expr e);
Stat newASTStatIf(Expr e, Block b1, Block b2);
Stat newASTWhile(Expr e, Block b);
Stat newASTCall(char* ident, Exprsp es);

#define getEchoExpr(x) x->content.echo.expr
#define getIfExpr(x) x->content._if.expr
#define getIfBlock1(x) x->content._if.b1
#define getIfBlock2(x) x->content._if.b2
#define getWhileExpr(x) x->content.boucle_while.expr
#define getWhileBlock(x) x->content.boucle_while.block
#define getIdentCall(x) x->content.call.ident
#define getCallExprsp(x) x->content.call.exprsp
#define getLvalueSet(x) x->content.set.lvalue
#define getSetExpr(x) x->content.set.expr

/* -------------------------- Tableaux -------------------------- */

enum lvalue_tag {
  ASTlvalue, ASTtab
};

struct _lvalue {
  enum lvalue_tag tag;
  union {
    char* id;
    struct {
      Lvalue lvalue;
      Expr expr;
    } tableau;
  } content;
};
#define mallocLvalue malloc(sizeof(struct _lvalue))

Lvalue newASTlvalue(char* ident);
Lvalue newASTtab(Lvalue lvalue, Expr expr);

#define getLvalue(l) l->content.tableau.lvalue
#define getLvalueExpr(l) l->content.tableau.expr
#define getLvalueID(l) l->content.id

/* -------------------------- STypes -------------------------- */

typedef struct _stype *SType;


enum stype_tag {
    ASTBool, ASTInt, ASTVec
};
struct _stype {
    enum stype_tag tag;
    union {
      bool b;
      int entier;
      SType stype;
  } content;
};

#define mallocSType malloc(sizeof(struct _stype))

SType newASTBool(bool b);
SType newASTInt(int n);
SType newASTVec(SType st);

#define getBool(st) st->content.b
#define getInt(st) st->content.entier
#define getVec(st) st->content.stype


/* -------------------------- Types -------------------------- */
typedef struct _type *Type;
typedef struct _types *Types;

enum type_tag {
  ASTSType, ASTTypeFun
};

struct _type {
  enum type_tag tag;
  union {
    SType stype;
    struct{
      Types types;
      Type type;
    } function;
  } content;
};

#define mallocType malloc(sizeof(struct _type))
Type newASTSType(SType stype);
Type newASTTypeFun(Types types,Type type);

struct _types {
  Type head;
  Types tail;
};

#define mallocTypes malloc(sizeof(struct _types))

Types addType(Type t, Types ts);

#define getSType(t) t->content.stype
#define getTypesFun(t) t->content.function.types
#define getTypeFun(t) t->content.function.type

/* -------------------------- Arguments des fonctions -------------------------- */

enum arg_tag {
  ASTIdent
};

struct _arg {
  enum arg_tag tag;
  char* id; 
   Type t; 
};

#define mallocArg malloc(sizeof(struct _arg))

Arg newASTIdent(char* ident, Type t); 

struct _args {
  Arg head; 
  Args tail; 
};

#define mallocArgs malloc(sizeof(struct _args))

Args addArg(Arg a, Args as);

#define getIdent(a) a->id
#define getType(a) a->t

/* -------------------------- Arguments des procedures --------------------------*/

enum argp_tag {
  ASTIdentP, ASTVarId
};

struct _argp {
  enum argp_tag tag;
  char* idp; 
  Type tp; 
};

#define mallocArgp malloc(sizeof(struct _argp))
Argp newASTVarId(char* ident, Type t);
Argp newASTIdentPointer(char* ident, Type t); 

struct _argsp {
  Argp head; 
  Argsp tail; 
};

#define mallocArgsp malloc(sizeof(struct _argsp))
Argsp addArgp(Argp a, Argsp as);

#define getIdentP(a) a->idp
#define getTypeP(a) a->tp


/* -------------------------- Definitions -------------------------- */
typedef struct _def *Def;

enum def_tag {
	ASTConst, ASTFun, ASTFunRec, ASTVar, ASTProc, ASTProcRec
};

struct _def {
  enum def_tag tag; 
  char* ident;
  union {
    struct {
      Type t; 
      Expr e; 
    } constant;
    struct {
      Type t;
      Args args; 
      Expr e; 
    } fun;
    struct {
      Type t;
      Args args; 
      Expr e; 
    } funrec; 
    struct {
      SType t;
    } var;
    struct {
      Argsp argsp; 
      Block b; 
    } proc;
    struct {
      Argsp argsp; 
      Block b;
    } procrec;
  } content; 
};

#define mallocDef malloc(sizeof(struct _def))


Def newASTConst(char* id, Type t, Expr e);
Def newASTFun(char* ident, Type type, Args args, Expr e);
Def newASTFunRec(char* ident, Type type, Args args, Expr e);

Def newASTVar(char* ident, SType stype);
Def newASTProc(char* ident, Argsp argsp, Block b);
Def newASTProcRec(char* ident, Argsp argsp, Block b);

#define getDefId(d) d->ident

#define getDefType(d) d->content.constant.t
#define getDefExpr(d) d->content.constant.e

#define getFunType(d) d->content.fun.t
#define getFunArgs(d) d->content.fun.args
#define getFunExpr(d) d->content.fun.e

#define getFunRecType(d) d->content.funrec.t
#define getFunRecArgs(d) d->content.funrec.args
#define getFunRecExpr(d) d->content.funrec.e

#define getVarType(d) d->content.var.t

#define getProcArgsp(d) d->content.proc.argsp
#define getProcBlock(d) d->content.proc.b

#define getProcRecArgsp(d) d->content.procrec.argsp
#define getProcRecBlock(d) d->content.procrec.b


/* -------------------------- Commandes et suites de commandes -------------------------- */
typedef struct _cmd *Cmd;

enum cmd_tag {
  ASTstat, ASTDef
};

struct _cmd {
  enum cmd_tag tag;
  union {
    Stat stat;
    Def def;
  } content;
};
  
#define mallocCmd malloc(sizeof(struct _cmd))

Cmd newStat(Stat s);

#define getStat(c) c->content.stat
#define getDef(c) c->content.def

typedef struct _cmds *Cmds;

struct _cmds {
  Cmd head;
  Cmds tail;
};

#define mallocCmds malloc(sizeof(struct _cmds))

Cmds addStat(Stat s, Cmds cs);
Cmds addDef(Def d, Cmds cs);


/* -------------------------- Programme -------------------------- */
typedef struct _cmds *Prog;







