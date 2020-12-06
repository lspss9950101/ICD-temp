#ifndef __TYPES_H__
#define __TYPES_H__

typedef struct {
    int begin, end;
} Range;

typedef enum {
    NONE,
    VAR_ARRAY,
    VAR_INT,
    VAR_REAL,
    VAR_TEXT,
    VAR_FUNCTION
} VarEnum;

typedef enum {
    OP_ARRAY_ACCESS,
    OP_NOT,
    OP_AND,
    OP_OR,
    OP_ASSIGN,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_LT,
    OP_GT,
    OP_EQ,
    OP_GET,
    OP_LET,
    OP_NEQ,
    OP_NEG
} OpType;

#endif