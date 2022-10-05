/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et semantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe C                                                  == */
/* == Fichier: parser.y                                                    == */
/* == Grammaire et generation terme Prolog                                 == */
/* == Nota: prend son entree sur stdin                                     == */
/* ========================================================================== */

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>

#include "ast.h"
#include "prologTerm.h"

int yylex (void);
int yyerror (char *);

Block theBlock;
Cmds theProg;
 
%}

%token<num>  	NUM
%token<str>	IDENT
%token<bl>	BOOL 
%token<num>	INT
%token		VEC
%token       	ECHO CONST FUN REC
%token       	LPAR RPAR
%token       	LBRA RBRA
%token 	 	SEMICOL COL DPOINT ARC STAR
%token 	 	IF 
%token 	 	STATIF 
%token 	 	VARIABLE ADR 
%token 		VAR SET PROC CALL WHILE
%token 		AND OR 
%token 		ALLOC LEN NTH VSET

%union {
  int num;
  char* str;
  Expr expr;
  Exprs exprs;
  Exprp exprp;
  Exprsp exprsp;
  Stat stat;
  Lvalue lvalue;
  Cmds cmds;
  Cmds prog;
  Def def;
  Type type;
  Types types;
  SType stype;
  bool bl; 
  int ent;
  Arg arg;
  Args args;
  Argp argp;
  Argsp argsp;
}

%type<expr> expr
%type<exprs> exprs
%type<exprp> exprp
%type<exprsp> exprsp
%type<stat> stat
%type<lvalue> lvalue
%type<cmds> cmds
%type<cmds> block
%type<prog> prog
%type<type> type
%type<types> types
%type<stype> stype
%type<def> def
%type<arg> arg
%type<args> args
%type<argp> argp
%type<argsp> argsp


%start prog
%%

prog: block   { theBlock = $1;}
;

block: LBRA cmds RBRA { theProg = $2; $$ = $2; }
;

cmds:
  stat  		{ $$ = addStat($1,NULL); }
| def SEMICOL cmds	{ $$ = addDef($1,$3);  }
| stat SEMICOL cmds	{ $$ = addStat($1,$3); }
;

stat:
  ECHO expr 			{ $$ = newASTEcho($2); }
| SET lvalue expr 		{ $$ = newASTSet($2,$3); }
| STATIF expr block block  	{ $$ = newASTStatIf($2,$3,$4); }
| WHILE expr block 		{ $$ = newASTWhile($2,$3); }
| CALL IDENT exprsp 		{ $$ = newASTCall($2,$3); }
; 

lvalue:
  IDENT				{ $$ = newASTlvalue($1); }
| LPAR NTH lvalue expr RPAR	{ $$ = newASTtab($3,$4); }
;

def:
  CONST IDENT type expr				{ $$ = newASTConst($2,$3,$4); }
| FUN IDENT type LBRA args RBRA expr 		{ $$ = newASTFun($2,$3,$5,$7); }
| FUN REC IDENT type LBRA args RBRA expr 	{ $$ = newASTFunRec($3,$4,$6,$8); }
| VAR IDENT stype 				{ $$ = newASTVar($2,$3); }
| PROC IDENT LBRA argsp RBRA block 		{ $$ = newASTProc($2,$4,$6); }
| PROC REC IDENT LBRA argsp RBRA block 		{ $$ = newASTProcRec($3,$5,$7); }
;	

type:
  stype				{ $$ = newASTSType($1); }
| LPAR types ARC type RPAR 	{ $$ = newASTTypeFun($2,$4); }
;

types: 
  type			{ $$ = addType($1,NULL); }
| type STAR types 	{ $$ = addType($1,$3); }
;

stype:
  BOOL 				{ $$ = newASTBool($1); }
| INT				{ $$ = newASTInt($1); }
| LPAR VEC stype RPAR		{ $$ = newASTVec($3); }
;

expr:
  NUM				{ $$ = newASTNum($1); }
| IDENT                         { $$ = newASTId($1); }
| LPAR IF expr expr expr RPAR	{ $$ = newASTIf($3,$4,$5); }
| LPAR AND expr expr RPAR	{ $$ = newASTAnd($3,$4); }
| LPAR OR expr expr RPAR	{ $$ = newASTOr($3,$4); }
| LPAR expr exprs RPAR          { $$ = newASTApp($2,$3); }
| LBRA args RBRA expr           { $$ = newASTLambda($2,$4); }
| LPAR ALLOC expr RPAR		{ $$ = newASTAlloc($3); }
| LPAR LEN expr RPAR		{ $$ = newASTLen($3); }
| LPAR NTH expr expr RPAR	{ $$ = newASTNth($3,$4); }
| LPAR VSET expr expr expr RPAR { $$ = newASTVset($3,$4,$5); }
;

exprs:
  expr       { $$ = addExpr($1,NULL); }
| expr exprs { $$ = addExpr($1,$2); }
; 

exprp:
  expr			{ $$ = newASTExprP($1); }
| LPAR ADR IDENT RPAR	{ $$ = newASTIdP($3); }
;

exprsp:
  exprp		{ $$ = addExprp($1,NULL); }
| exprp exprsp	{ $$ = addExprp($1,$2); }
;

arg: IDENT DPOINT type  { $$ = newASTIdent($1,$3); }
;

args:
  arg 		{ $$ = addArg($1,NULL); }
| arg COL args 	{ $$ = addArg($1,$3); }
;

argp:
  IDENT DPOINT type		{ $$ = newASTIdentPointer($1,$3); }
| VARIABLE IDENT DPOINT type 	{ $$ = newASTVarId($2,$4); }
;

argsp:
  argp			{ $$ = addArgp($1,NULL); }
| argp COL argsp	{ $$ = addArgp($1,$3); }
;
%%

int yyerror(char *s) {
  printf("error: %s\n",s);
  return 1;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: ./prologTerm <fichier.aps>\n");
    return 1;
  }
  FILE* infile = freopen(argv[1], "r", stdin);
  yyparse();
  fclose(infile);
  printProg(theProg);
  printf(".\n");
  return 0;
}

