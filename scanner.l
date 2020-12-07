%{
/*
 * scanner.l
 *
 * lex input file for pascal scanner
 *
 */

#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "parser.h"

int fileno(FILE *);

#define YY_USER_ACTION \
    yylloc.first_line = line_no; \
    yylloc.first_column = col_no; \
    col_no += yyleng;

#define MAX_LINE_LENG      256
#define LIST       strcat(buffer, yytext)
#define LIST_FLUSH do{ if(opt_list) printf("%s", buffer); *buffer = 0; }while(0)
#define LOG(TYPE) \
    do{ LIST; \
        if(opt_token) \
        fprintf(stderr, "token(type:%-10s) on line %4d, %3d : %s\n", \
            #TYPE, line_no, col_no - yyleng + 1, yytext); \
    } while(0)

#ifdef __cplusplus
extern "C" int yylex(void);
#endif

int opt_list = 1, opt_token = 0;
int line_no = 1, col_no = 0;
char buffer[MAX_LINE_LENG];

%}

%option nounput
%option noinput

A [aA]
B [bB]
C [cC]
D [dD]
E [eE]
F [fF]
G [gG]
H [hH]
I [iI]
J [jJ]
K [kK]
L [lL]
M [mM]
N [nN]
O [oO]
P [pP]
Q [qQ]
R [rR]
S [sS]
T [tT]
U [uU]
V [vV]
W [wW]
X [xX]
Y [yY]
Z [zZ]
BLANKS [ \t]*

DIGIT [0-9]

%x COMMENT

%%
                             /* v could do something */
{P}{R}{O}{G}{R}{A}{M}       {LOG(KEYWORD); return(PROGRAM);    }
{V}{A}{R}                   {LOG(KEYWORD); return(VAR);        }
{A}{R}{R}{A}{Y}             {LOG(KEYWORD); return(ARRAY);      }
{O}{F}                      {LOG(KEYWORD); return(OF);         }
{I}{N}{T}{E}{G}{E}{R}       {LOG(KEYWORD); return(INTEGER);    }
{R}{E}{A}{L}                {LOG(KEYWORD); return(REAL);       }
{S}{T}{R}{I}{N}{G}          {LOG(KEYWORD); return(STRING);     }
{F}{U}{N}{C}{T}{I}{O}{N}    {LOG(KEYWORD); return(FUNCTION);   }
{P}{R}{O}{C}{E}{D}{U}{R}{E} {LOG(KEYWORD); return(PROCEDURE);  }
{B}{E}{G}{I}{N}             {LOG(KEYWORD); return(PBEGIN);     }
{E}{N}{D}                   {LOG(KEYWORD); return(END);        }
{I}{F}                      {LOG(KEYWORD); return(IF);         }
{T}{H}{E}{N}                {LOG(KEYWORD); return(THEN);       }
{E}{L}{S}{E}                {LOG(KEYWORD); return(ELSE);       }
{W}{H}{I}{L}{E}             {LOG(KEYWORD); return(WHILE);      }
{D}{O}                      {LOG(KEYWORD); return(DO);         }
{N}{O}{T}                   {LOG(KEYWORD); return(NOT);        }
{A}{N}{D}                   {LOG(KEYWORD); return(AND);        }
{O}{R}                      {LOG(KEYWORD); return(OR);         }

"("                         {LOG(KEYWORD); return(LPAREN);     }
")"                         {LOG(KEYWORD); return(RPAREN);     }
";"                         {LOG(KEYWORD); return(SEMICOLON);  }
"."                         {LOG(KEYWORD); return(DOT);        }
","                         {LOG(KEYWORD); return(COMMA);      }
":"                         {LOG(KEYWORD); return(COLON);      }
"["                         {LOG(KEYWORD); return(LBRACE);     }
"]"                         {LOG(KEYWORD); return(RBRACE);     }
".."                        {LOG(KEYWORD); return(DOTDOT);     }
":="                        {LOG(KEYWORD); return(ASSIGNMENT); }
"+"                         {LOG(KEYWORD); return(ADDOP);      }
"-"                         {LOG(KEYWORD); return(SUBOP);      }
"*"                         {LOG(KEYWORD); return(MULOP);      }
"/"                         {LOG(KEYWORD); return(DIVOP);      }
">"                         {LOG(KEYWORD); return(GTOP);       }
"<"                         {LOG(KEYWORD); return(LTOP);       }
"="                         {LOG(KEYWORD); return(EQOP);       }
">="                        {LOG(KEYWORD); return(GETOP);      }
"<="                        {LOG(KEYWORD); return(LETOP);      }
"!="                        {LOG(KEYWORD); return(NEQOP);      }


[a-z]([a-zA-Z0-9_])*[a-zA-Z0-9]|[a-z]       {LOG(IDENTIFIER); yylval.text = strdup(yytext); return IDENTIFIER;}

{DIGIT}+                                    {LOG(NUMBER); yylval.val  = atoi(yytext); return INTEGERNUM;}// integer
{DIGIT}+"."{DIGIT}+                         {LOG(NUMBER); yylval.dval = atof(yytext); return REALNUMBER;} // real
{DIGIT}+{E}[+-]?{DIGIT}+                    {LOG(NUMBER); yylval.dval = atof(yytext); return SCIENTIFIC;} // scientific
{DIGIT}+"."{DIGIT}+{E}[+-]?{DIGIT}+         {LOG(NUMBER); yylval.dval = atof(yytext); return SCIENTIFIC;} // scientific

\"([^\n\"]|\\\")*\" {
  LOG(STRING);
  col_no = col_no - yyleng;
  for(size_t c = 0; c < strlen(yytext); c++){
    if (yytext[c] == '\n'){
      line_no++, col_no = 0;
    }
    else col_no++;
  }
  yylval.text = strdup(yytext);
  return(LITERALSTR);
}

"//"[^\n]* {
  LIST;
  if(opt_token) fprintf(stderr, "[INFO ] line %4d:%3d comment string\n", line_no, col_no-(int)yyleng+1);
}

"/*" {
  LIST;
  if(opt_token) fprintf(stderr, "[INFO ] line %4d:%3d comment string start\n", line_no, col_no-(int)yyleng+1);
  BEGIN(COMMENT);
}

<COMMENT>"*/" {
  LIST;
  BEGIN(INITIAL);
  if(opt_token) fprintf(stderr, "[INFO ] line %4d:%3d comment string end\n", line_no, col_no-(int)yyleng+1);
}

<COMMENT>\n {
  LIST;
  LIST_FLUSH;
  line_no++, col_no = 0;
}

<COMMENT>. { LIST; }

\n {
  LIST;
  LIST_FLUSH;
  line_no++, col_no = 0;
}

[ \t\f\r]         LIST;

#[ \t]*{P}{R}{A}{G}{M}{A}[ \t]+{L}{I}{S}{T}[ \t]+{O}{N}    { LIST; opt_list = 1; }
#[ \t]*{P}{R}{A}{G}{M}{A}[ \t]+{L}{I}{S}{T}[ \t]+{O}{F}{F} { LIST; opt_list = 0; }

#[ \t]*{P}{R}{A}{G}{M}{A}[ \t]+{T}{O}{K}{E}{N}[ \t]+{O}{N}    { LIST; opt_token = 1; }
#[ \t]*{P}{R}{A}{G}{M}{A}[ \t]+{T}{O}{K}{E}{N}[ \t]+{O}{F}{F} { LIST; opt_token = 0; }

.  { LIST; fprintf(stderr, "[ERROR] line %4d:%3d lexical analyzer error %s\n", line_no, col_no-(int)yyleng+1, yytext); }
