#ifndef __AST_H__
#define __AST_H__

#include "loc.h"
#include <types.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

typedef enum{
  NullNode,
  ProgNode,
  IDListNode,
  DeclarationNode,
  SubprogramDeclarationNode,
  CompoundStatementListNode,
  ExprNode,
  ExprListNode,
  IfNode,
  WhileNode
} NodeType;

typedef enum {
  Primary,
  Ref,
  Unary,
  Binary,
  Func
} ExprType;

struct Node;

typedef struct IDList {
  LocType loc;
  char *id;
  struct IDList *next;
} IDList;

typedef struct StmtList {
  LocType loc;
  struct Node *stmt;
  struct StmtList *next;
} StmtList;

typedef struct VarType {
  LocType loc;
  VarEnum type;
  struct VarType* of_type;
  struct Node *begin, *end;
} VarType;

typedef struct VarDescription {
  LocType loc;
  VarType *type;
  IDList *list;
} VarDescription;

struct ProgNodeAttr {
  char *id;
  VarType *ret_type;
  struct Node *args, *declarations, *subprogram_declarations, *compound_statement;
};

struct DeclarationNodeAttr {
  VarType *type;
  IDList *list;
  struct Node *next;
};

struct SubprogDeclarationNodeAttr {
  struct Node *subprogram;
  struct Node *next;
};

struct CompoundStmtNodeAttr {
  StmtList *stmts;
};

struct ExprNodeAttr {
  ExprType type;
  union {
    struct {
      VarEnum var_type;
      union {
        int val;
        char *text;
        double dval;
      };
    };
    char *var_id;
    struct {
      OpType unary_op;
      struct Node *oprand;
    };
    struct {
      OpType binary_op;
      struct Node *left, *right;
    };
    struct {
      char *id;
      struct Node *args;
    };
  };
  struct Node *next;
};

struct IfNodeAttr {
  struct Node *condition, *statement, *else_statement;
};

struct WhileNodeAttr {
  struct Node *condition, *statement;
};

typedef struct Node {
  NodeType nt;
  LocType loc;

  struct Node *parent;
  union {
    struct ProgNodeAttr prog_node_attr;
    IDList *id_list;
    struct DeclarationNodeAttr declaration_node_attr;
    struct SubprogDeclarationNodeAttr subprogram_declaration_node_attr;
    struct CompoundStmtNodeAttr compound_stmt_node_attr;
    struct ExprNodeAttr expression;
    struct IfNodeAttr if_node_attr;
    struct WhileNodeAttr while_node_attr;
  };
} Node;

void addChildNode(Node*, Node**, Node*);

void createBinaryExpr(OpType, Node*, Node*, Node*);

void createUnaryExpr(OpType, Node*, Node*);

Node* createRefStrip(Node*, Node*);

void createIfStatement(Node*, Node*, Node*, Node*);

void createWhileLoop(Node*, Node*, Node*);

void createStmtList(StmtList*, Node*);

void createDeclaration(Node*, VarDescription*);

void appendStmt(StmtList*, StmtList*, StmtList*);

void printAST(Node *, int);

void printVarType(VarType*);

#define Obj void*

typedef struct ConsTag{
  Obj car;
  struct ConsTag *cdr;
} *Cons, ConsStr;

#endif
