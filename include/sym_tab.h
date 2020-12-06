#ifndef __SYM_TAB_H__
#define __SYM_TAB_H__

#include <types.h>

typedef struct {
    VarEnum type;
    int scope;
    char *id;
    union {
        Range array_range[8];
        VarEnum func_args[8];
    };
} SymEntry;

SymEntry *sym_table[128];
int sym_entry_count;
int scope_level;

#endif