%{
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "loc.h"
#include "ast.h"
#include "error.h"

#define YYLTYPE LocType

#define MAX_LINE_LENG      256
extern int line_no, col_no, opt_list;
extern char buffer[MAX_LINE_LENG];
extern FILE *yyin;        /* declared by lex */
extern char *yytext;      /* declared by lex */
extern int yyleng;

extern
#ifdef __cplusplus
"C"
#endif
int yylex(void);
static void yyerror(const char *msg);
extern int yylex_destroy(void);

%}

%locations

%token PROGRAM VAR ARRAY OF INTEGER REAL STRING FUNCTION PROCEDURE PBEGIN END IF THEN ELSE WHILE DO NOT AND OR

%token LPAREN RPAREN SEMICOLON DOT COMMA COLON LBRACE RBRACE DOTDOT ASSIGNMENT ADDOP SUBOP MULOP DIVOP LTOP GTOP EQOP GETOP LETOP NEQOP

%token IDENTIFIER REALNUMBER INTEGERNUM SCIENTIFIC LITERALSTR

%union {
  int val;
  char* text;
  double dval;
  Node node;
}

%type <node> prog

%%
prog : PROGRAM IDENTIFIER LPAREN identifier_list RPAREN SEMICOLON
     declarations
     subprogram_declarations
     compound_statement
     DOT
     ;

identifier_list : IDENTIFIER
                | IDENTIFIER COMMA identifier_list
                ;

declarations : VAR identifier_list COLON type SEMICOLON declarations
             |
             ;

type : standard_type
     | ARRAY LBRACE num DOTDOT num RBRACE OF type
     ;

standard_type : INTEGER
              | REAL
              | STRING
              ;

subprogram_declarations : subprogram_declaration SEMICOLON subprogram_declarations
                        | 
                        ;

subprogram_declaration : subprogram_head
                       declarations
                       subprogram_declarations
                       compound_statement
                       ;

subprogram_head : FUNCTION IDENTIFIER arguments COLON standard_type SEMICOLON
                | PROCEDURE IDENTIFIER arguments SEMICOLON
                ;

arguments : LPAREN parameter_list RPAREN 
          | 
          ;

parameter_list : optional_var identifier_list COLON type
               | optional_var identifier_list COLON type SEMICOLON parameter_list
               ;

optional_var : VAR
             |
             ;

compound_statement : PBEGIN statement_list END

statement_list : statement
               | statement SEMICOLON statement_list
               ;

statement : variable ASSIGNMENT expression
          | procedure_statement
          | compound_statement
          | IF expression THEN statement ELSE statement
          | WHILE expression DO statement
          | 
          ;

variable : IDENTIFIER tail

tail : LBRACE expression RBRACE tail
     | 
     ;

procedure_statement : IDENTIFIER
                    | IDENTIFIER LPAREN expression_list RPAREN
                    ;

expression_list : expression
                | expression COMMA expression_list
                ;

expression : boolexpression
           | boolexpression AND boolexpression
           | boolexpression OR boolexpression
           ;


boolexpression : simple_expression
               | simple_expression relop simple_expression
               ;

simple_expression : term
                  | simple_expression addop term
                  ;

term : factor
     | term mulop factor
     ;

factor : variable
       | IDENTIFIER LPAREN expression_list RPAREN
       | num
       | LITERALSTR
       | LPAREN expression RPAREN
       | NOT factor
       | SUBOP factor
       ;

addop : ADDOP
      | SUBOP
      ;

mulop : MULOP
      | DIVOP
      ;

relop : LTOP
      | GTOP
      | EQOP 
      | LETOP
      | GETOP
      | NEQOP
      ;

num : INTEGERNUM
    | REALNUMBER
    | SCIENTIFIC
    ;

%%

void yyerror(const char *msg) {
    fprintf(stderr,
            "[ERROR] line %4d:%3d %s, Unmatched token: %s\n",
            line_no, col_no-(int)yyleng+1, buffer, yytext);
}

int main(int argc, const char *argv[]) {

    if(argc > 2)
        fprintf( stderr, "Usage: ./parser [filename]\n" ), exit(0);

    FILE *fp = argc == 1 ? stdin : fopen(argv[1], "r");

    if(fp == NULL)
        fprintf( stderr, "Open file error\n" ), exit(-1);

    yyin = fp;
    yyparse();
    return 0;
}