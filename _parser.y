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

extern SymEntry *sym_table[128];
extern int sym_entry_count;
extern int scope_level;

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
  Node node;
  IDList id_list;
  VarDescription var_description;
  VarType var_type;
  OpType op_type;
  Range range;
  StmtList stmt_list;
}

%type <node> prog
%type <id_list> identifier_list
%type <node> declarations
%type <var_description> declaration
%type <var_description> variable_description
%type <var_type> type
%type <var_type> standard_type
%type <range> array_indices
%type <node> subprogram_declarations
%type <node> subprogram_header
%type <node> subprogram_declaration
%type <node> arguments
%type <node> parameter_list
%type <node> compound_statement
%type <stmt_list> statement_list
%type <stmt_list> statement
%type <node> variable
%type <node> tail
%type <node> expression
%type <node> boolexpression
%type <node> simple_expression
%type <node> term
%type <node> factor
%type <node> expression_list
%type <node> num
%type <op_type> addop
%type <op_type> mulop
%type <op_type> relop
%type <node> procrdure_statement

%%

prog: PROGRAM IDENTIFIER LPAREN identifier_list RPAREN SEMICOLON
    declarations
    subprogram_declarations
    compound_statement
    progend {
        $$.nt = ProgNode;
        $$.loc = concat(@1, @10);
        root = &$$;
        $$.prog_node_attr.id = $<text>2;

        Node *ids_node = malloc(sizeof(Node));
        ids_node->nt = IDListNode;
        ids_node->loc = $<id_list>4.loc;
        ids_node->id_list = &$<id_list>4;

        addChildNode(&$$, &$$.prog_node_attr.args, ids_node);
        addChildNode(&$$, &$$.prog_node_attr.declarations, &$<node>7);
        addChildNode(&$$, &$$.prog_node_attr.subprogram_declarations, &$<node>8);
        addChildNode(&$$, &$$.prog_node_attr.compound_statement, &$<node>9);

        //printAST(root, 0);
    };

progend: DOT
    | ;

identifier_list: IDENTIFIER {
        $$.id = $<text>1;
        $$.next = NULL;
    }
    | identifier_list COMMA IDENTIFIER {
        $$.id = $<text>3;
        $$.next = &$<id_list>1;
    };

declarations: declarations declaration {
        $$.nt = DeclarationNode;
        $$.loc = concat($<node>1.loc, $<var_description>2.loc);
        $$.declaration_node_attr.type = $<var_description>2.type;
        $$.declaration_node_attr.list = $<var_description>2.list;
        $$.declaration_node_attr.next = &$<node>1;
    }
    | {
        $$.nt = NullNode;
    };

declaration: VAR variable_description SEMICOLON {
        $$ = $<var_description>2;
        $$.loc = concat(@1, @3);
    };

variable_description: identifier_list COLON type {
        $$.loc = concat($<id_list>1.loc, $<var_type>3.loc);
        $$.type = $<var_type>3;
        $$.list = &$<id_list>1;
    };

type: standard_type {
        $$ = $<var_type>1;
    }
    | ARRAY array_indices OF type {
        $$.loc = concat(@1, $<var_type>4.loc);
        $$.type = VAR_ARRAY;
        $$.of_type = &$<var_type>4;
        $$.array_range = $<range>2;
    };

standard_type: INTEGER {
        $$.loc = @1;
        $$.type = VAR_INT;
    }
    | REAL {
        $$.loc = @1;
        $$.type = VAR_REAL;
    }
    | STRING {
        $$.loc = @1;
        $$.type = VAR_TEXT;
    };

array_indices: LBRACE INTEGERNUM DOTDOT INTEGERNUM RBRACE {
        $$.begin = $<val>2;
        $$.end = $<val>4;
    };

subprogram_declarations: subprogram_declarations subprogram_declaration {
        $$.nt = SubprogramDeclarationNode;
        $$.loc = concat($<node>1.loc, $<node>2.loc);
        $$.subprogram_declaration_node_attr.subprogram = &$<node>2.prog_node_attr;
        $$.subprogram_declaration_node_attr.next = &$<node>1;
    }
    | {
        $$.nt = NullNode;
    };

subprogram_header: FUNCTION IDENTIFIER arguments COLON type SEMICOLON {
        $$.nt = ProgNode;
        $$.loc = concat(@1, @6);
        $$.prog_node_attr.id = $<text>2;
        $$.prog_node_attr.ret_type = $<var_type>5;
        $$.prog_node_attr.args = &$<node>3;
    }
    | PROCEDURE IDENTIFIER arguments SEMICOLON {
        $$.nt = ProgNode;
        $$.loc = concat(@1, @4);
        $$.prog_node_attr.id = $<text>2;
        $$.prog_node_attr.ret_type.type = NONE;
    };

subprogram_declaration: subprogram_header
    declarations
    subprogram_declarations
    compound_statement
    SEMICOLON {
        $$.nt = $<node>1.nt;
        $$.loc = concat($<node>1.loc, @5);
        $$.prog_node_attr.id = $<node>1.prog_node_attr.id;
        $$.prog_node_attr.ret_type = $<node>1.prog_node_attr.ret_type;
        $$.prog_node_attr.declarations = &$<node>2;
        $$.prog_node_attr.subprogram_declarations = &$<node>3;
        $$.prog_node_attr.compound_statement = &$<node>4;
    };

arguments: LPAREN parameter_list RPAREN {
        $$ = $<node>2;
    }
    | {
        $$.nt = NullNode;
    };

optional_var: VAR
    | ;

parameter_list: optional_var variable_description {
        createDeclaration(&$$, &$<var_description>2);
        $$.loc = concat(@1, $<var_description>2.loc);
    }
    | optional_var variable_description SEMICOLON parameter_list {
        appendDeclaration(&$$, &$<node>4, &$<var_description>2);
        $$.loc = concat(@1, $<node>4.loc);
    };

compound_statement: PBEGIN
    statement_list
    END {
        $$.nt = $<stmt_list>2.stmt == NULL ? NullNode : CompoundStatementListNode;
        $$.loc = concat(@1, @3);
        $$.compound_stmt_node_attr.stmts = &$<stmt_list>2;
    };

statement_list: statement
    | statement_list SEMICOLON statement {
        appendStmt(&$$, &$<stmt_list>1, &$<stmt_list>3);
        $$.loc = concat($<stmt_list>1.loc, $<stmt_list>3.loc);
    };

statement: variable ASSIGNMENT expression {
        Node *new_node = malloc(sizeof(Node));
        
        createBinaryExpr(OP_ASSIGN, new_node, &$<node>1, &$<node>3);
        new_node->loc = concat($<node>1.loc, $<node>3.loc);

        createStmtList(&$$, new_node);
        $$.loc = new_node->loc;
    }
    | procrdure_statement {
        createStmtList(&$$, &$<node>1);
        $$.loc = $<node>1.loc;
    }
    | compound_statement {
        createStmtList(&$$, &$<node>1);
        $$.loc = $<node>1.loc;
    }
    | IF expression THEN statement ELSE statement {
        Node *new_node = malloc(sizeof(Node));

        createIfStatement(new_node, &$<node>2, &$<node>4, &$<node>6);
        new_node->loc = concat(@1, $<node>6.loc);

        createStmtList(&$$, new_node);
        $$.loc = new_node->loc;
    }
    | WHILE expression DO statement {
        Node *new_node = malloc(sizeof(Node));

        createWhileLoop(new_node, &$<node>2, &$<node>4);
        new_node->loc = concat(@1, $<node>4.loc);

        createStmtList(&$$, new_node);
        $$.loc = new_node->loc;
    }
    | {
        $$.stmt = NULL;
    };

variable: IDENTIFIER tail {
        if($<node>2.nt == NullNode) {
            $$.nt = ExprNode;
            $$.loc = @1;
            $$.expression.type = Ref;
            $$.expression.var_id = $<text>1;
            $$.expression.next = NULL;
        } else {
            $$ = *createRefStrip($<text>1, @1, &$<node>2);
            $$.loc = concat(@1, $<node>2.loc);
        }
    };

tail: LBRACE expression RBRACE tail {
        appendExpr(&$$, &$<node>4, &$<node>2);
        $$.loc = concat(@1, $<node>4.loc);
    }
    | {
        $$.nt = NullNode;
    };

expression: boolexpression {
        $$ = $<node>1;
    }
    | boolexpression AND boolexpression {
        createBinaryExpr(OP_AND, &$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    }
    | boolexpression OR boolexpression {
        createBinaryExpr(OP_OR, &$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    };

boolexpression: simple_expression {
        $$ = $<node>1;
    }
    | simple_expression relop simple_expression {
        createBinaryExpr($<op_type>2, &$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    };

simple_expression: term {
        $$ = $<node>1;
    }
    | simple_expression addop term {
        createBinaryExpr($<op_type>2, &$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    };

term: factor {
        $$ = $<node>1;
    }
    | term mulop factor {
        createBinaryExpr($<op_type>2, &$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    };

factor: IDENTIFIER tail {
        $$.nt = ExprNode;
        $$.loc = concat(@1, $<node>2.loc);
        if($<node>2.nt == NullNode) {
            $$.expression.type = Ref;
            $$.expression.var_id = $<text>1;
            $$.expression.next = NULL;
        } else {
            $$ = *createRefStrip($<text>1, @1, &$<node>2);
            $$.loc = concat(@1, $<node>2.loc);
        }
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$.nt = ExprNode;
        $$.loc = concat(@1, @4);
        $$.expression.type = Func;
        $$.expression.id = $<text>1;
        addChildNode(&$$, &$$.expression.args, &$<node>3);
    }
    | num
    | LITERALSTR {
        $$.nt = ExprNode;
        $$.loc = @1;
        $$.expression.type = Primary;
        $$.expression.text = $<text>1;
        $$.expression.next = NULL;
    }
    | LPAREN expression RPAREN{
        $$ = $<node>2;
    }
    | NOT factor{
        createUnaryExpr(OP_NOT, &$$, &$<node>2);
        $$.loc = concat(@1, $<node>2.loc);
    }
    | SUBOP factor {
        createUnaryExpr(OP_NEG, &$$, &$<node>2);
        $$.loc = concat(@1, $<node>2.loc);
    };

expression_list: expression {
        $$ = $<node>1;
    }
    | expression_list COMMA expression {
        appendExpr(&$$, &$<node>1, &$<node>3);
        $$.loc = concat($<node>1.loc, $<node>3.loc);
    };

num: INTEGERNUM {
        $$.nt = ExprNode;
        $$.loc = @1;
        $$.expression.type = Primary;
        $$.expression.val = $<val>1;
        $$.expression.next = NULL;
    }
    | REALNUMBER {
        $$.nt = ExprNode;
        $$.loc = @1;
        $$.expression.type = Primary;
        $$.expression.val = $<dval>1;
        $$.expression.next = NULL;
    }
    | SCIENTIFIC {
        $$.nt = ExprNode;
        $$.loc = @1;
        $$.expression.type = Primary;
        $$.expression.val = $<dval>1;
        $$.expression.next = NULL;
    };

addop: ADDOP {
        $$= OP_ADD;
    }
    | SUBOP {
        $$ = OP_SUB;
    };

mulop: MULOP {
        $$ = OP_MUL;
    }
    | DIVOP {
        $$ = OP_DIV;
    };

relop: LTOP {
        $$ = OP_LT;
    }
    | GTOP {
        $$ = OP_GT;
    }
    | EQOP {
        $$ = OP_EQ;
    }
    | LETOP {
        $$ = OP_LET;
    }
    | GETOP {
        $$ = OP_GET;
    }
    | NEQOP {
        $$ = OP_NEQ;
    };

procrdure_statement: IDENTIFIER {
        $$.nt = ExprNode;
        $$.loc = @1;
        $$.expression.type = Func;
        $$.expression.id = $<text>1;
        $$.expression.args = NULL;
        $$.expression.next = NULL;
    }
    | IDENTIFIER LPAREN expression_list RPAREN {
        $$.nt = ExprNode;
        $$.loc = concat(@1, @4);
        $$.expression.type = Func;
        $$.expression.id = $<text>1;
        $$.expression.args = &$<node>3;
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

    printf("%d\n", yyparse());

    printAST(root, 0);
    
    return 0;
}
