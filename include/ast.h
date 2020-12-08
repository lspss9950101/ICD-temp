#ifndef __AST_H__
#define __AST_H__

#include "loc.h"
#include <types.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

typedef enum{
  ProgNode,
  SubprogNode,
  ArgumentNode,
  DeclarationNode,
  SubprogramDeclarationNode,
  CompoundStatementListNode,
  ExprNode,
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

typedef struct Node Node;

typedef struct VarType {
  LocType loc;
  VarEnum type;
  union {
    struct {
      struct VarType* of_type;
      struct Node *begin, *end;
    };
    struct {
      struct VarType* ret_type;
      struct Node *args;
    };
  };
} VarType;

struct ProgNodeAttr {
  char *id;
  VarType *type;
  struct Node *declarations, *subprogram_declarations, *compound_statement;
};

struct DeclarationNodeAttr {
  VarType *type;
  char *id;
  struct Node *next;
};

struct SubprogDeclarationNodeAttr {
  struct Node *subprogram;
  struct Node *next;
};

struct CompoundStmtNodeAttr {
  Node *stmts;
  Node *next;
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
    } primary;
    struct {
      char *id;
    } ref;
    struct {
      OpType op;
      struct Node *oprand;
    } unary;
    struct {
      OpType op;
      struct Node *left, *right;
      struct {
        char *id;
        LocType loc;
      } array;
    } binary;
    struct {
      char *id;
      struct Node *args;
    } func;
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

  union {
    struct ProgNodeAttr prog_node_attr;
    struct DeclarationNodeAttr declaration_node_attr;
    struct SubprogDeclarationNodeAttr subprogram_declaration_node_attr;
    struct CompoundStmtNodeAttr compound_stmt_node_attr;
    struct ExprNodeAttr expression;
    struct IfNodeAttr if_node_attr;
    struct WhileNodeAttr while_node_attr;
  };
} Node;

void createBinaryExpr(OpType, Node*, Node*, Node*);

void createUnaryExpr(OpType, Node*, Node*);

Node* createRefStrip(Node*, Node*);

void printAST(Node *, int);

void printVarType(VarType*);

#define Obj void*

typedef struct ConsTag{
  Obj car;
  struct ConsTag *cdr;
} *Cons, ConsStr;

#endif
