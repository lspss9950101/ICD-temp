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
#define LOG(TYPE) do{ LIST; if(opt_token) fprintf(stderr, "token(type:%-10s) on line %4d, %3d : %s\n", #TYPE, line_no,col_no - yyleng + 1, yytext); } while(0)
#define LOG_COMMENT do{ LIST; } while(0)
#define LOG_COMMENT_START do{LIST; } while(0)
#define LOG_COMMENT_END do{ LIST;	} while(0)

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

%x MULTI_LINE_COMMENT

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
"/"							            {LOG(KEYWORD); return(DIVOP);      }
">"                         {LOG(KEYWORD); return(GTOP);       }
"<"                         {LOG(KEYWORD); return(LTOP);       }
"="                         {LOG(KEYWORD); return(EQOP);       }
">="                        {LOG(KEYWORD); return(GETOP);      }
"<="                        {LOG(KEYWORD); return(LETOP);      }
"!="                        {LOG(KEYWORD); return(NEQOP);      }

  /* define identifier here */
([a-z][A-Za-z0-9_]*[A-Za-z0-9]|[a-z])	{
  LOG(IDENTIFIER);
  yylval.text = strdup(yytext);
  return(IDENTIFIER);
}

  /* define INTEGERNUM, REALNUMBER, SCIENTIFIC here */
([0-9]+)([.][0-9]+)?{E}[+-]?[0-9]+ {
  LOG(NUMBER);
  yylval.dval = atof(yytext);
  return(SCIENTIFIC);
}
[0-9]*						{
  LOG(NUMBER);
  yylval.val = atoi(yytext);
  return(INTEGERNUM);
}
[0-9]+[.][0-9]+				{
  LOG(NUMBER);
  yylval.dval = atof(yytext);
  return(REALNUMBER);
}

  /* define single/multiple line comment here */
"//".*						{LOG_COMMENT;	}
"//".*\n					{LOG_COMMENT; LIST_FLUSH; line_no++, col_no=0;	}
"/*"						{LOG_COMMENT_START; BEGIN(MULTI_LINE_COMMENT);	}
<MULTI_LINE_COMMENT>.		{LIST; LIST_FLUSH;	}
<MULTI_LINE_COMMENT>\n		{LIST; LIST_FLUSH; line_no++, col_no = 0;	}
<MULTI_LINE_COMMENT>"*/"	{LOG_COMMENT_END; BEGIN(INITIAL);	}
<MULTI_LINE_COMMENT>"*/"\n	{LOG_COMMENT_END; LIST_FLUSH; BEGIN(INITIAL); line_no++, col_no=0;	}

  /* define string constant (LITERALSTR) here */
\"[^"]*\"					{
  LOG(STRING);
  yylval.text = strdup(yytext);
  return(LITERALSTR);
}

  /* define pragma here */
\#[ ]*"pragma"[ ]+"list"[ ]+"on"	{opt_list = 1; LIST; LIST_FLUSH;	}
\#[ ]*"pragma"[ ]+"list"[ ]+"off"	{opt_list = 0; LIST; LIST_FLUSH;	}
\#[ ]*"pragma"[ ]+"token"[ ]+"on" {opt_token = 1; LIST; LIST_FLUSH;     }
\#[ ]*"pragma"[ ]+"token"[ ]+"off" {opt_token = 0; LIST; LIST_FLUSH;     }

[ \t\f\r]    LIST;

\n {
  LIST;
  LIST_FLUSH;
  line_no++, col_no = 0;
} 

.  { LIST; fprintf(stderr, "[ERROR] line %4d:%3d lexical analyzer error %s\n", line_no, col_no-(int)yyleng+1, yytext); }

%%

int yywrap() { fclose(yyin), exit(0); }
