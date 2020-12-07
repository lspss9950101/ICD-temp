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
} SymEntry;

int checkTable(SymEntry*, char*, int);

int arrayTypeCmp(VarType*, VarType*);

VarType* getVarType(SymEntry*, char*);

VarType* getPrimaryType(VarType*);

void setFunc(SymEntry*, char*);

int isFuncSet(SymEntry*, char*);

int insertEntry(SymEntry**, char*, VarType*, int);

int deleteEntry(SymEntry**, int);

void printSymTab(SymEntry *);

void getVarName(VarType*, char*);

void traverseAST(Node*);

VarType* _traverseAST(SymEntry**, Node*, int, int);

char* getOpTypeStr(OpType);

#endif