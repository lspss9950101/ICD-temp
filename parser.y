%{
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "loc.h"
#include "ast.h"
#include <sym_tab.h>
#include <string.h>
#include <types.h>
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

Node *root = NULL;

%}

%locations

%token PROGRAM VAR ARRAY OF INTEGER REAL STRING FUNCTION PROCEDURE PBEGIN END IF THEN ELSE WHILE DO NOT AND OR

%token LPAREN RPAREN SEMICOLON DOT COMMA COLON LBRACE RBRACE DOTDOT ASSIGNMENT ADDOP SUBOP MULOP DIVOP LTOP GTOP EQOP GETOP LETOP NEQOP

%token IDENTIFIER REALNUMBER INTEGERNUM SCIENTIFIC LITERALSTR

%union {
  int val;
  char* text;
  double dval;

  Node *node;
  VarType *var_type;
  OpType op_type;
}

%type <node> prog
%type <node> identifier_list
%type <node> declarations
%type <var_type> type
%type <var_type> standard_type
%type <node> subprogram_declarations
%type <node> subprogram_declaration
%type <node> subprogram_head
%type <node> arguments
%type <node> parameter_list
%type <node> compound_statement
%type <node> statement_list
%type <node> statement
%type <node> variable
%type <node> tail
%type <node> procedure_statement
%type <node> expression_list
%type <node> expression
%type <node> boolexpression
%type <node> simple_expression
%type <node> term
%type <node> factor
%type <op_type> addop
%type <op_type> mulop
%type <op_type> relop
%type <node> num

%%
prog : PROGRAM IDENTIFIER LPAREN identifier_list RPAREN SEMICOLON
     declarations
     subprogram_declarations
     compound_statement
     DOT { 
        root = $$ = malloc(sizeof(Node));
        $$->nt = ProgNode;
        $$->loc = concat(@1, @10);
        $$->prog_node_attr.id = $<text>2;
        $$->prog_node_attr.type = malloc(sizeof(VarType));
        $$->prog_node_attr.type->type = VAR_FUNCTION;
        $$->prog_node_attr.type->ret_type = NONE;
        $$->prog_node_attr.type->args = $<node>4;

        $$->prog_node_attr.declarations = $<node>7;
        $$->prog_node_attr.subprogram_declarations = $<node>8;
        $$->prog_node_attr.compound_statement = $<node>9;

        YYACCEPT;
     };

identifier_list : IDENTIFIER {
        $$ = malloc(sizeof(Node));
        $$->nt = DeclarationNode;
        $$->loc = @1;
        $$->declaration_node_attr.id = $<text>1;
        $$->declaration_node_attr.next = NULL;
    }
    | IDENTIFIER COMMA identifier_list {
        $$ = malloc(sizeof(Node));
        $$->nt = DeclarationNode;
        $$->loc = @1;
        $$->declaration_node_attr.id = $<text>1;
        $$->declaration_node_attr.next = $<node>3;
    };

declarations : VAR identifier_list COLON type SEMICOLON declarations {
        $$ = $<node>2;
        Node *last;
        for(Node *cur = $$; cur != NULL; cur = cur->declaration_node_attr.next) {
            cur->declaration_node_attr.type = $<var_type>4;
            if(cur->declaration_node_attr.next == NULL) last = cur;
        }
        last->declaration_node_attr.next = $<node>6;
    }
    | { $$ = NULL; };

type : standard_type {
        $$ = $<var_type>1;
    }
    | ARRAY LBRACE num DOTDOT num RBRACE OF type {
        $$ = malloc(sizeof(VarType));
        $$->loc = concat(@1, $<var_type>8->loc);
        $$->type = VAR_ARRAY;
        $$->of_type = $<var_type>8;
        $$->begin = $<node>3;
        $$->end = $<node>5;
    };

standard_type : INTEGER {
        $$ = malloc(sizeof(VarType));
        $$->loc = @1;
        $$->type = VAR_INT;
    }
    | REAL {
        $$ = malloc(sizeof(VarType));
        $$->loc = @1;
        $$->type = VAR_REAL;
    }
    | STRING {
        $$ = malloc(sizeof(VarType));
        $$->loc = @1;
        $$->type = VAR_TEXT;
    };

subprogram_declarations : subprogram_declaration SEMICOLON subprogram_declarations {
        $$ = malloc(sizeof(Node));
        $$->nt = SubprogramDeclarationNode;
        $$->loc = $<node>1->loc;
        $$->subprogram_declaration_node_attr.subprogram = $<node>1;
        $$->subprogram_declaration_node_attr.next = $<node>3;
    }
    | { $$ = NULL; };

subprogram_declaration : subprogram_head
                       declarations
                       subprogram_declarations
                       compound_statement {
        $$ = $<node>1;
        $$->loc = concat($$->loc, $<node>4->loc);
        $$->prog_node_attr.declarations = $<node>2;
        $$->prog_node_attr.subprogram_declarations = $<node>3;
        $$->prog_node_attr.compound_statement = $<node>4;
    };

subprogram_head : FUNCTION IDENTIFIER arguments COLON standard_type SEMICOLON {
        $$ = malloc(sizeof(Node));
        $$->nt = SubprogramDeclarationNode;
        $$->loc = concat(@1, @6);
        $$->prog_node_attr.id = $<text>2;
        
        $$->prog_node_attr.type = malloc(sizeof(VarType));
        $$->prog_node_attr.type->type = VAR_FUNCTION;
        $$->prog_node_attr.type->ret_type = $<var_type>5;
        $$->prog_node_attr.type->args = $<node>3;
    }
    | PROCEDURE IDENTIFIER arguments SEMICOLON {
        $$ = malloc(sizeof(Node));
        $$->nt = SubprogramDeclarationNode;
        $$->loc = concat(@2, @4);
        $$->prog_node_attr.id = $<text>2;

        $$->prog_node_attr.type = malloc(sizeof(VarType));
        $$->prog_node_attr.type->type = VAR_FUNCTION;
        $$->prog_node_attr.type->ret_type = NULL;
        $$->prog_node_attr.type->args = $<node>3;
    };

arguments : LPAREN parameter_list RPAREN {
        $$ = $<node>2;
    }
    | {$$ = NULL;};

parameter_list : optional_var identifier_list COLON type {
        $$ = $<node>2;
        for(Node *cur = $$; cur != NULL; cur = cur->declaration_node_attr.next) {
            cur->declaration_node_attr.type = $<var_type>4;
        }
    }
    | optional_var identifier_list COLON type SEMICOLON parameter_list {
        $$ = $<node>2;
        Node *last;
        for(Node *cur = $$; cur != NULL; cur = cur->declaration_node_attr.next) {
            cur->declaration_node_attr.type = $<var_type>4;
            if(cur->declaration_node_attr.next == NULL) last = cur;
        }
        last->declaration_node_attr.next = $<node>6;
    };

optional_var : VAR
    | ;

compound_statement : PBEGIN statement_list END {
    $$ = malloc(sizeof(Node));
    $$->nt = CompoundStatementListNode;
    $$->loc = concat(@1, @3);
    $$->compound_stmt_node_attr.stmts = $<node>2;
    $$->compound_stmt_node_attr.next = NULL;
}

statement_list : statement {
        $$ = $<node>1;
    }
    | statement SEMICOLON statement_list {
        $$ = $<node>1;
        $$->compound_stmt_node_attr.next = $<node>3;
    };

statement : variable ASSIGNMENT expression {
        Node *expr= malloc(sizeof(Node));
        createBinaryExpr(OP_ASSIGN, expr, $<node>1, $<node>3);
        expr->loc = concat($<node>1->loc, $<node>3->loc);

        $$ = malloc(sizeof(Node));
        $$->nt = CompoundStatementListNode;
        $$->loc = expr->loc;
        $$->compound_stmt_node_attr.stmts = expr;
        $$->compound_stmt_node_attr.next = NULL;
    }
    | procedure_statement {
        $$ = malloc(sizeof(Node));
        $$->nt = CompoundStatementListNode;
        $$->loc = $<node>1->loc;
        $$->compound_stmt_node_attr.stmts = $<node>1;
        $$->compound_stmt_node_attr.next = NULL;
    }
    | compound_statement {
        $$ = $<node>1;
    }
    | IF expression THEN statement ELSE statement {
        Node *expr = malloc(sizeof(Node));
        expr->nt = IfNode;
        expr->if_node_attr.condition = $<node>2;
        expr->if_node_attr.statement = $<node>4;
        expr->if_node_attr.else_statement = $<node>6;

        if($<node>6 == NULL) expr->loc = concat(@1, @5);
        else expr->loc = concat(@1, $<node>6->loc);

        $$ = malloc(sizeof(Node));
        $$->nt = CompoundStatementListNode;
        $$->loc = expr->loc;
        $$->compound_stmt_node_attr.stmts = expr;
        $$->compound_stmt_node_attr.next = NULL;
    }
    | WHILE expression DO statement {
        Node *expr = malloc(sizeof(Node));
        expr->nt = WhileNode;
        expr->while_node_attr.condition = $<node>2;
        expr->while_node_attr.statement = $<node>4;
        
        if($<node>4 == NULL) expr->loc = concat(@1, @3);
        else expr->loc = concat(@1, $<node>4->loc);

        $$ = malloc(sizeof(Node));
        $$->nt = CompoundStatementListNode;
        $$->loc = expr->loc;
        $$->compound_stmt_node_attr.stmts = expr;
        $$->compound_stmt_node_attr.next = NULL;
    }
    | { $$ = NULL; };

variable : IDENTIFIER tail {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Ref;
        $$->expression.ref.id = $<text>1;
        $$->expression.next = NULL;

        if($<node>2 != NULL) {
            $$ = createRefStrip($$, $<node>2);
            $$->loc = concat(@1, $<node>2->loc);
        } 
    };

tail : LBRACE expression RBRACE tail {
        $$ = $<node>2;
        $$->expression.next = $<node>4;
    }
    | { $$ = NULL; };

procedure_statement : IDENTIFIER {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Func;
        $$->expression.func.id = $<text>1;
        $$->expression.func.args = NULL;
        $$->expression.next = NULL;
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = concat(@1, @4);
        $$->expression.type = Func;
        $$->expression.func.id = $<text>1;
        $$->expression.func.args = $<node>3;
        $$->expression.next = NULL;
    };

expression_list : expression {
        $$ = $<node>1;
    }
    | expression COMMA expression_list {
        $$ = $<node>1;
        $$->expression.next = $<node>3;
    };

expression : boolexpression {
        $$ = $<node>1;
    }
    | boolexpression AND boolexpression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr(OP_AND, $$, $<node>1, $<node>3);
        $$->loc = @2;
    }
    | boolexpression OR boolexpression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr(OP_OR, $$, $<node>1, $<node>3);
        $$->loc = @2;
    };


boolexpression : simple_expression {
        $$ = $<node>1;
    }
    | simple_expression relop simple_expression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = @2;
    };

simple_expression : term {
        $$ = $<node>1;
    }
    | simple_expression addop term {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = @2;
    };

term : factor {
        $$ = $<node>1;
    }
    | term mulop factor {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = @2;
    };

factor : variable {
        $$ = $<node>1;
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Func;
        $$->expression.func.id = $<text>1;
        $$->expression.func.args = $<node>3;
        $$->expression.next = NULL;
    }
    | num {
        $$ = $<node>1;
    }
    | LITERALSTR {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.primary.var_type = VAR_TEXT;
        $$->expression.primary.text = $<text>1;
        $$->expression.next = NULL;
    }
    | LPAREN expression RPAREN {
        $$ = $<node>2;
        $$->loc = concat(@1, @3);
    }
    | NOT factor {
        $$ = malloc(sizeof(Node));
        createUnaryExpr(OP_NOT, $$, $<node>2);
        $$->loc = @1;
    }
    | SUBOP factor {
        $$ = malloc(sizeof(Node));
        createUnaryExpr(OP_NEG, $$, $<node>2);
        $$->loc = @1;
    };

addop : ADDOP { $$ = OP_ADD; }
    | SUBOP { $$ = OP_SUB; };

mulop : MULOP { $$ = OP_MUL; }
    | DIVOP { $$ = OP_DIV; };

relop : LTOP { $$ = OP_LT; }
    | GTOP { $$ = OP_GT; }
    | EQOP { $$ = OP_EQ; }
    | LETOP { $$ = OP_LET; }
    | GETOP { $$ = OP_GET; }
    | NEQOP { $$ = OP_NEQ; };

num : INTEGERNUM {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.primary.var_type = VAR_INT;
        $$->expression.primary.val = $<val>1;
        $$->expression.next = NULL;
    }
    | REALNUMBER {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.primary.var_type = VAR_REAL;
        $$->expression.primary.dval = $<dval>1;
        $$->expression.next = NULL;
    }
    | SCIENTIFIC {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.primary.var_type = VAR_REAL;
        $$->expression.primary.dval = $<dval>1;
        $$->expression.next = NULL;
    };

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
    
    traverseAST(root);
    //printAST(root, 0);
    return 0;
}