#include <sym_tab.h>

// 1: found
// 0: not found
int checkTable(SymEntry* tab, char* id, int scope, int check_redef) {
    while(tab != NULL && tab->scope == scope) {
        if(!strcmp(id, tab->id)) {
            if(check_redef) tab->redef = 1;
            return 1;
        }
        tab = tab->next;
    }
    return 0;
}

// 0: equal
// 1: not equal
int typeCmp(VarType* l, VarType* r) {
    while(1) {
        if(l->type == VAR_ARRAY && r->type != VAR_ARRAY) return 1;
        else if(r->type == VAR_ARRAY && l->type != VAR_ARRAY) return 1;
        if(l->type != VAR_ARRAY || r->type != VAR_ARRAY) {
            if(l->type != r->type) return 1;
            return 0;
        }
        if(l->begin->expression.primary.val != r->begin->expression.primary.val ||
            l->end->expression.primary.val != r->end->expression.primary.val) {
            return 1;
        }
        l = l->of_type;
        r = r->of_type;
    }
}

VarType* getVarType(SymEntry* tab, char* id) {
    while(tab != NULL) {
        if(!strcmp(id, tab->id)) return tab->type;
        tab = tab->next;
    }
    return NULL;
}

VarType* getPrimaryType(VarType* type) {
    while(type != NULL && type->type == VAR_FUNCTION) {
        type = type->of_type;
    }
    return type;
}

// 1: redef
// 0: no error
int setFunc(SymEntry* tab, char *id) {
    while(tab != NULL) {
        if(!strcmp(id, tab->id)) {
            if(tab->func_node != NULL && tab->redef) return 1;
            tab->func_set = 1;
            return 0;
        }
        tab = tab->next;
    }
    return 0;
}

int isFuncSet(SymEntry* tab, Node* func_node) {
    while(tab != NULL) {
        if(tab->func_node == func_node) {
            return tab->func_set;
        }
        tab = tab->next;
    }
    return 0;
}

int insertVarEntry(SymEntry** tab, char* id, VarType* type, int scope) {
    if(checkTable(*tab, id, scope, 0)) return 1;
    SHOW_NEWSYM(id);
    SymEntry *old = *tab;
    *tab = malloc(sizeof(SymEntry));
    (*tab)->id = id;
    (*tab)->type = type;
    (*tab)->scope = scope;
    (*tab)->next = old;
    (*tab)->func_set = 0;
    (*tab)->func_node = NULL;
    (*tab)->redef = 0;
    return 0;
}

int insertFuncEntry(SymEntry** tab, char* id, Node *func_node, VarType* type, int scope) {
    if(checkTable(*tab, id, scope, 1)) {
        return 1;
    }
    SHOW_NEWSYM(id);
    SymEntry *old = *tab;
    *tab = malloc(sizeof(SymEntry));
    (*tab)->id = id;
    (*tab)->type = type;
    (*tab)->scope = scope;
    (*tab)->next = old;
    (*tab)->func_set = 0;
    (*tab)->func_node = func_node;
    (*tab)->redef = 0;
    return 0;
}

int deleteEntry(SymEntry** tab, int scope) {
    while(*tab != NULL && (*tab)->scope >= scope) {
        SymEntry *old = *tab;
        *tab = (*tab)->next;
        free(old);
    }
    return 0;
}

void getVarName(VarType* type, char* tgt) {
    if(type == NULL) {
        sprintf(tgt, "void");
        return;
    }
    switch(type->type) {
        case NONE:
            sprintf(tgt, "void");
            break;
        case VAR_ARRAY:
            sprintf(tgt, "[%d~%d]", type->begin->expression.primary.val, type->end->expression.primary.val);
            char tmp[128];
            getVarName(type->of_type, tmp);
            strcat(tmp, tgt);
            strcpy(tgt, tmp);
            break;
        case VAR_INT:
            sprintf(tgt, "int");
            break;
        case VAR_REAL:
            sprintf(tgt, "real");
            break;
        case VAR_TEXT:
            sprintf(tgt, "text");
            break;
        case VAR_FUNCTION:
            getVarName(type->ret_type, tgt);
            if(type->args == NULL) break;
            strcat(tgt, " (");
            for(Node *cur = type->args; cur != NULL; cur = cur->declaration_node_attr.next) {
                char type_tmp[32];
                getVarName(cur->declaration_node_attr.type, type_tmp);
                strcat(tgt, type_tmp);
                if(cur->declaration_node_attr.next != NULL) strcat(tgt, ", ");
            }
            strcat(tgt, ")");
            break;
    }
}

void printSymTab(SymEntry *tab) {
    SHOW_SYMTAB_HEAD();
    for(SymEntry *cur = tab; cur != NULL; cur = cur->next) {
        char tmp[64];
        getVarName(cur->type, tmp);
        printf(SYMTAB_ENTRY_FMT , cur->id, cur->scope, tmp);
    }
    SHOW_SYMTAB_TAIL();
}

void traverseAST(Node* ast) {
    SymEntry *tab = NULL;
    _traverseAST(&tab, ast, 0, 0);
}

// method:
// 0: None
// 1: declaration
// 2: expr
// 3: expr_list
VarType* _traverseAST(SymEntry** tab, Node* node, int scope, int method) {
    if(node == NULL) return NULL;
    switch((int)node->nt) {
        case ProgNode:{
            struct ProgNodeAttr attr = node->prog_node_attr;
            SHOW_NEWSCP();
            if(insertFuncEntry(tab, attr.id, node, NULL, scope)) {
                fprintf(stderr, REDEF_FUN, node->loc.first_line, node->loc.first_column, attr.id);
            }
            _traverseAST(tab, attr.declarations, scope, 1);
            _traverseAST(tab, attr.subprogram_declarations, scope, 1);
            _traverseAST(tab, attr.compound_statement, scope, 2);
            SHOW_CLSSCP();
            printSymTab(*tab);
            return node->prog_node_attr.type->ret_type;
        }
        case ArgumentNode:
        case DeclarationNode:{
            struct DeclarationNodeAttr attr;
            for(struct Node *cur = node; cur != NULL; cur = cur->declaration_node_attr.next) {
                attr = cur->declaration_node_attr;
                if(insertVarEntry(tab, attr.id, attr.type, scope)) {
                    fprintf(stderr, node->nt == ArgumentNode ? REDEF_ARG : REDEF_VAR, cur->loc.first_line, cur->loc.first_column, attr.id);
                }
            }
            return NULL;
        }
        case SubprogramDeclarationNode:{
            struct ProgNodeAttr attr;
            Node *subprog;
            for(struct Node *cur = node; cur != NULL; cur = cur->subprogram_declaration_node_attr.next) {
                subprog = cur->subprogram_declaration_node_attr.subprogram;
                attr = subprog->prog_node_attr;
                
                if(insertFuncEntry(tab, attr.id, subprog, attr.type, scope))
                    fprintf(stderr, REDEF_FUN, cur->loc.first_line, cur->loc.first_column, cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.id);
                
                SHOW_NEWSCP();
                scope++;
                _traverseAST(tab, attr.type->args, scope, 1);
                _traverseAST(tab, attr.declarations, scope, 1);
                _traverseAST(tab, attr.subprogram_declarations, scope, 1);
                _traverseAST(tab, attr.compound_statement, scope, 2);
                SHOW_CLSSCP();
                printSymTab(*tab);
                if(attr.type->ret_type != NULL && !isFuncSet(*tab, subprog))
                    fprintf(stderr, RETURN_VAL, subprog->loc.first_line, subprog->loc.first_column, subprog->prog_node_attr.id);
                deleteEntry(tab, scope);
                scope--;
            }
            return NULL;
        }
        case CompoundStatementListNode:{
            for(Node *cur = node; cur != NULL; cur = cur->compound_stmt_node_attr.next) {
                _traverseAST(tab, cur->compound_stmt_node_attr.stmts, scope, 2);
            }
            return NULL;
        }
        case ExprNode:{
            switch(node->expression.type) {
                case Ref:{
                    VarType *type;
                    if(NULL == (type = getVarType(*tab, node->expression.ref.id))) {
                        fprintf(stderr, UNDEC_VAR, node->loc.first_line, node->loc.first_column, node->expression.ref.id);
                    }
                    else if(type->type == VAR_FUNCTION) {
                        if(type->ret_type != NULL) type = type->ret_type;
                    }
                    return type;
                }
                case Func:{
                    VarType *type;
                    if(NULL == (type = getVarType(*tab, node->expression.func.id))) {
                        fprintf(stderr, UNDEC_FUN, node->loc.first_line, node->loc.first_column, node->expression.func.id);
                        for(Node *cur = node->expression.func.args; cur != NULL; cur = cur->expression.next)
                            _traverseAST(tab, cur, scope, 2);
                    } else {
                        Node *args = node->expression.func.args;
                        Node *args_type = type->args;
                        
                        while(1) {
                            if(args == NULL && args_type != NULL) fprintf(stderr, WRONG_ARGS, node->loc.first_line, node->loc.first_column, node->expression.func.id);
                            else if(args_type == NULL && args != NULL) fprintf(stderr, WRONG_ARGS, node->loc.first_line, node->loc.first_column, node->expression.func.id);
                            if(args == NULL || args_type == NULL) break;
                            VarType *arg_type = _traverseAST(tab, args, scope, 2);

                            if(typeCmp(arg_type, args_type->declaration_node_attr.type)) {
                                fprintf(stderr, WRONG_ARGS, args->loc.first_line, args->loc.first_column, node->expression.func.id);
                                break;
                            }
                            args = args->expression.next;
                            args_type = args_type->declaration_node_attr.next;
                        }
                    }
                    return getPrimaryType(type);
                }
                case Unary:{
                    VarType *type = _traverseAST(tab, node->expression.unary.oprand, scope, 2);
                    if(type->type == VAR_TEXT) {
                        fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.unary.op));
                    }
                    return type;
                }
                case Binary:{
                    VarType *type_left = _traverseAST(tab, node->expression.binary.left, scope, 2);
                    VarType *type_right = _traverseAST(tab, node->expression.binary.right, scope, 2);
                    if(type_left == NULL || type_right == NULL) return NULL;
                    if(node->expression.binary.op == OP_ASSIGN) {
                        if(type_right->type == VAR_FUNCTION) {
                            fprintf(stderr, ASSIG_TYPE, node->loc.first_line, node->loc.first_column);
                            return NULL;
                        }
                        if(node->expression.binary.left->expression.type == Ref) {
                            if(setFunc(*tab, node->expression.binary.left->expression.ref.id)) {
                                fprintf(stderr, ASSIG_TYPE, node->loc.first_line, node->loc.first_column);
                                return NULL;
                            }
                        }
                        if(typeCmp(type_left, type_right))
                            fprintf(stderr, ASSIG_TYPE, node->loc.first_line, node->loc.first_column);
                        return NULL;
                    }
                    else if(node->expression.binary.op == OP_ARRAY_ACCESS) {
                        if(type_left->type != VAR_ARRAY)
                            fprintf(stderr, INDEX_MANY, node->expression.binary.array.loc.first_line, node->expression.binary.array.loc.first_column, node->expression.binary.array.id);
                        if(type_right->type != VAR_INT)
                            fprintf(stderr, INDEX_TYPE, node->expression.binary.right->loc.first_line, node->expression.binary.right->loc.first_column);
                        return type_left->of_type;
                    }
                    else if(node->expression.binary.op == OP_AND || node->expression.binary.op == OP_OR) {
                        if(type_left == NULL || type_right == NULL)
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                        if(type_left->type == VAR_TEXT || type_right->type == VAR_TEXT)
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                        else if(type_left->type != VAR_INT || type_left->type != VAR_INT)
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                        return type_left;
                    }
                    else {
                        if(type_right->type == VAR_FUNCTION || type_left->type == VAR_FUNCTION) {
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                            return NULL;
                        }
                        if(type_left->type == VAR_TEXT || type_right->type == VAR_TEXT) {
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                            return NULL;
                        }
                        if(typeCmp(type_left, type_right)) {
                            fprintf(stderr, ARITH_TYPE, node->loc.first_line, node->loc.first_column, getOpTypeStr(node->expression.binary.op));
                            return NULL;
                        }
                        return type_left;
                    }
                    break;
                }
                case Primary:{
                    VarType *type = malloc(sizeof(VarType));
                    type->type = node->expression.primary.var_type;
                    return type;
                }
                default:
                    break;
            }
            return NULL;
        }
        case IfNode:
            _traverseAST(tab, node->if_node_attr.condition, scope, 2);
            _traverseAST(tab, node->if_node_attr.statement, scope, 2);
            _traverseAST(tab, node->if_node_attr.else_statement, scope, 2);
            return NULL;
        case WhileNode:
            _traverseAST(tab, node->while_node_attr.condition, scope, 2);
            _traverseAST(tab, node->while_node_attr.statement, scope, 2);
            return NULL;
    }
    return NULL;
}

char* getOpTypeStr(OpType op) {
    switch(op) {
        case OP_ARRAY_ACCESS:
            return "[]";
        case OP_NOT:
            return "!";
        case OP_AND:
            return "and";
        case OP_OR:
            return "or";
        case OP_ASSIGN:
            return ":=";
        case OP_ADD:
            return "+";
        case OP_SUB:
            return "-";
        case OP_MUL:
            return "*";
        case OP_DIV:
            return "/";
        case OP_LT:
            return "<";
        case OP_GT:
            return ">";
        case OP_EQ:
            return "==";
        case OP_GET:
            return ">=";
        case OP_LET:
            return "<=";
        case OP_NEQ:
            return "!=";
        case OP_NEG:
            return "-";
    }
    return "";
}