#ifndef __SYM_TAB_H__
#define __SYM_TAB_H__

#include <types.h>
#include <ast.h>
#include <info.h>
#include <string.h>

typedef struct SymEntry {
    VarType *type;
    int scope;
    char *id;
    struct SymEntry *next;
    int func_set;
    Node *func_node;
    int redef;
} SymEntry;

int checkTable(SymEntry*, char*, int, int);

int typeCmp(VarType*, VarType*);

VarType* getVarType(SymEntry*, char*);

VarType* getPrimaryType(VarType*);

int setFunc(SymEntry*, char*);

int isFuncSet(SymEntry*, Node*);

int insertVarEntry(SymEntry**, char*, VarType*, int);

int insertFuncEntry(SymEntry**, char*, Node*, VarType*, int);

int deleteEntry(SymEntry**, int);

void getVarName(VarType*, char*);

void printSymTab(SymEntry *);

void getVarName(VarType*, char*);

void traverseAST(Node*);

VarType* _traverseAST(SymEntry**, Node*, int, int);

char* getOpTypeStr(OpType);

#endif