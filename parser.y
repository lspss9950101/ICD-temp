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
  IDList *id_list;
  VarDescription *var_description;
  VarType *var_type;
  OpType op_type;
  Range range;
  StmtList *stmt_list;
}

%type <node> prog
%type <id_list> identifier_list
%type <node> declarations
%type <var_type> type
%type <var_type> standard_type
%type <node> subprogram_declarations
%type <node> subprogram_declaration
%type <node> subprogram_head
%type <node> arguments
%type <node> parameter_list
%type <node> compound_statement
%type <stmt_list> statement_list
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
        $$ = malloc(sizeof(Node));
        $$->nt = ProgNode;
        $$->loc = concat(@1, @10);
        $$->prog_node_attr.id = $<text>2;
        $$->prog_node_attr.ret_type = NULL;

        Node *ids_node = malloc(sizeof(Node));
        ids_node->nt = IDListNode;
        ids_node->loc = $<id_list>4->loc;
        ids_node->id_list = $<id_list>4;

        addChildNode($$, &$$->prog_node_attr.args, ids_node);
        addChildNode($$, &$$->prog_node_attr.declarations, $<node>7);
        addChildNode($$, &$$->prog_node_attr.subprogram_declarations, $<node>8);
        addChildNode($$, &$$->prog_node_attr.compound_statement, $<node>9);

        root = $$;
        printAST(root, 0);
     };

identifier_list : IDENTIFIER {
        $$ = malloc(sizeof(IDList));
        $$->id = $<text>1;
        $$->loc = @1;
        $$->next = NULL;
    }
    | IDENTIFIER COMMA identifier_list {
        $$ = malloc(sizeof(IDList));
        $$->id = $<text>1;
        $$->loc = concat(@1, $<id_list>3->loc);
        $$->next = $<id_list>3;
    };

declarations : VAR identifier_list COLON type SEMICOLON declarations {
        $$ = malloc(sizeof(Node));
        $$->nt = DeclarationNode;
        $$->declaration_node_attr.type = $<var_type>4;
        $$->declaration_node_attr.list = $<id_list>2;
        if($<node>6 == NULL) {
            $$->loc = concat(@1, @5);
        } else {
            $$->loc = concat(@1, $<node>6->loc);
            addChildNode($$, &$$->declaration_node_attr.next, $<node>6);
        }
    }
    | { $$ = NULL; };

type : standard_type {
        $$ = $<var_type>1;
    }
    | ARRAY LBRACE num DOTDOT num RBRACE OF type {
        $$ = malloc(sizeof(VarType));
        $$->loc = concat(@1, $<var_type>8->loc);
        $$->type = VAR_ARRAY;
        $$->of_type = $<var_type>6;
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
        addChildNode($$, &$$->subprogram_declaration_node_attr.subprogram, $<node>1);
        if($<node>3 == NULL) {
            $$->loc = concat($<node>1->loc, @2);
            $$->subprogram_declaration_node_attr.next = NULL;
        } else {
            $$->loc = concat($<node>1->loc, $<node>3->loc);
            addChildNode($$, &$$->subprogram_declaration_node_attr.next, $<node>3);
        }
    }
    | { $$ = NULL; };

subprogram_declaration : subprogram_head
                       declarations
                       subprogram_declarations
                       compound_statement {
        $$ = $<node>1;
        $$->loc = concat($$->loc, $<node>4->loc);
        addChildNode($$, &$$->prog_node_attr.declarations, $<node>2);
        addChildNode($$, &$$->prog_node_attr.subprogram_declarations, $<node>3);
        addChildNode($$, &$$->prog_node_attr.compound_statement, $<node>4);
    };

subprogram_head : FUNCTION IDENTIFIER arguments COLON standard_type SEMICOLON {
        $$ = malloc(sizeof(Node));
        $$->nt = SubprogramDeclarationNode;
        $$->loc = concat(@1, @6);
        $$->prog_node_attr.id = $<text>2;
        $$->prog_node_attr.ret_type = $<var_type>4;
        addChildNode($$, &$$->prog_node_attr.args, $<node>3);
    }
    | PROCEDURE IDENTIFIER arguments SEMICOLON {
        $$ = malloc(sizeof(Node));
        $$->nt = SubprogramDeclarationNode;
        $$->loc = concat(@1, @4);
        $$->prog_node_attr.id = $<text>2;
        $$->prog_node_attr.ret_type = NULL;
        addChildNode($$, &$$->prog_node_attr.args, $<node>3);
    };

arguments : LPAREN parameter_list RPAREN {
        $$ = $<node>2;
        $$->loc = concat(@1, @3);
    }
    | {$$ = NULL;};

parameter_list : optional_var identifier_list COLON type {
        $$ = malloc(sizeof(Node));
        $$->nt = DeclarationNode;
        $$->loc = concat(@1, $<var_type>4->loc);
        $$->declaration_node_attr.type = $<var_type>4;
        $$->declaration_node_attr.list = $<id_list>2;
        $$->declaration_node_attr.next = NULL;
    }
    | optional_var identifier_list COLON type SEMICOLON parameter_list {
        $$ = malloc(sizeof(Node));
        $$->nt = DeclarationNode;
        $$->loc = concat(@1, $<var_type>4->loc);
        $$->declaration_node_attr.type = $<var_type>4;
        $$->declaration_node_attr.list = $<id_list>2;
        addChildNode($$, &$$->declaration_node_attr.next, $<node>6);
    };

optional_var : VAR
             |
             ;

compound_statement : PBEGIN statement_list END {
    $$ = malloc(sizeof(Node));
    $$->nt = CompoundStatementListNode;
    $$->loc = concat(@1, @3);
    $$->compound_stmt_node_attr.stmts = $<stmt_list>2;
}

statement_list : statement {
        if($<node>1 == NULL) {
            $$ = NULL;
        } else {
            $$ = malloc(sizeof(StmtList));
            $$->loc = $<node>1->loc;
            $$->stmt = $<node>1;
            $$->next = NULL;
        }
    }
    | statement SEMICOLON statement_list {
        $$ = malloc(sizeof(StmtList));
        $$->loc = $<node>1->loc;
        $$->stmt = $<node>1;
        $$->next = $<stmt_list>3;
    };

statement : variable ASSIGNMENT expression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr(OP_ASSIGN, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    }
    | procedure_statement {
        $$ = $<node>1;
    }
    | compound_statement {
        $$ = $<node>1;
    }
    | IF expression THEN statement ELSE statement {
        $$ = malloc(sizeof(Node));
        createIfStatement($$, $<node>2, $<node>4, $<node>6);
        $$->loc = concat(@1, $<node>6->loc);
    }
    | WHILE expression DO statement {
        $$ = malloc(sizeof(Node));
        createWhileLoop($$, $<node>2, $<node>4);
        $$->loc = concat(@1, $<node>4->loc);
    }
    | { $$ = NULL; };

variable : IDENTIFIER tail {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Ref;
        $$->expression.var_id = $<text>1;
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
        $$->expression.id = $<text>1;
        $$->expression.args = NULL;
        $$->expression.next = NULL;
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = concat(@1, @4);
        $$->expression.type = Func;
        $$->expression.id = $<text>1;
        $$->expression.args = $<node>3;
        $$->expression.next = NULL;
    };

expression_list : expression {
        $$ = $<node>1;
    }
    | expression COMMA expression_list {
        $$ = $<node>1;
        $$->loc = concat($<node>1->loc, $<node>3->loc);
        $$->expression.next = $<node>3;
    };

expression : boolexpression {
        $$ = $<node>1;
    }
    | boolexpression AND boolexpression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr(OP_AND, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    }
    | boolexpression OR boolexpression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr(OP_OR, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    };


boolexpression : simple_expression {
        $$ = $<node>1;
    }
    | simple_expression relop simple_expression {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    };

simple_expression : term {
        $$ = $<node>1;
    }
    | simple_expression addop term {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    };

term : factor {
        $$ = $<node>1;
    }
    | term mulop factor {
        $$ = malloc(sizeof(Node));
        createBinaryExpr($<op_type>2, $$, $<node>1, $<node>3);
        $$->loc = concat($<node>1->loc, $<node>3->loc);
    };

factor : variable {
        $$ = $<node>1;
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = concat(@1, @4);
        $$->expression.type = Func;
        $$->expression.id = $<text>1;
        $$->expression.args = $<node>3;
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
        $$->expression.var_type = VAR_TEXT;
        $$->expression.text = $<text>1;
        $$->expression.next = NULL;
    }
    | LPAREN expression RPAREN {
        $$ = $<node>2;
        $$->loc = concat(@1, @3);
    }
    | NOT factor {
        $$ = malloc(sizeof(Node));
        createUnaryExpr(OP_NOT, $$, $<node>2);
        $$->loc = concat(@1, $<node>1->loc);
    }
    | SUBOP factor {
        $$ = malloc(sizeof(Node));
        createUnaryExpr(OP_NEG, $$, $<node>2);
        $$->loc = concat(@1, $<node>1->loc);
    };

addop : ADDOP {
        $$ = OP_ADD;
    }
    | SUBOP {
        $$ = OP_SUB;
    };

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
        $$->expression.var_type = VAR_INT;
        $$->expression.val = $<val>1;
        $$->expression.next = NULL;
    }
    | REALNUMBER {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.var_type = VAR_REAL;
        $$->expression.dval = $<dval>1;
        $$->expression.next = NULL;
    }
    | SCIENTIFIC {
        $$ = malloc(sizeof(Node));
        $$->nt = ExprNode;
        $$->loc = @1;
        $$->expression.type = Primary;
        $$->expression.var_type = VAR_REAL;
        $$->expression.dval = $<dval>1;
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
    return 0;
}