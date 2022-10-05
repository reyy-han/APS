/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe C                                                  == */
/* == Fichier: lexer.lex                                                   == */
/* == Lexique                                                              == */
/* ========================================================================== */

%{

#include <stdlib.h>

#include "ast.h"  
#include "y.tab.h"


%}

nls "\n"|"\r"|"\r\n"
nums "-"?[0-9]+
bl [01]
idents [a-zA-Z][a-zA-Z0-9]*
%%

[ \t\n\r]  { /* on ignore */ }
"\r\n"     { /* on ignore aussi */ }


"["	{ return(LBRA); }
"]"	{ return(RBRA); }
"("	{ return(LPAR); }
")"	{ return(RPAR); }
";"	{ return(SEMICOL); }
":"	{ return(DPOINT); }
","	{ return(COL); }
"*"	{ return(STAR); }
"->"	{ return(ARC); }

"CONST"	{ return(CONST); }
"FUN"	{ return(FUN); }
"REC"	{ return(REC); }
"ECHO"  { return(ECHO); }
"if"	{ return(IF); }
"IF"	{ return(STATIF); }
"bool"	{ return(BOOL); }
"int"	{ return(INT); }
"and"	{ return(AND); }
"or"	{ return(OR); }
"var"	{ return(VARIABLE); }
"adr"	{ return(ADR); }
"vec"	{ return(VEC); }
"alloc" { return(ALLOC); }
"len"	{ return(LEN); }
"nth"	{ return(NTH); }
"vset"	{ return(VSET); }

"WHILE"    	{ return(WHILE); }
"CALL"    	{ return(CALL); }
"PROC"    	{ return(PROC); }
"SET"    	{ return(SET); }
"VAR"    	{ return(VAR); }


{nums}    {
            yylval.num=atoi(yytext);
            return(NUM);
          }


{idents}  {
            yylval.str=strdup(yytext);
            return(IDENT);
          }

