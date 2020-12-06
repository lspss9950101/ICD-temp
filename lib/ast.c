#include <ast.h>

void addChildNode(Node* parent, Node** parent_attr, Node* child) {
    *parent_attr = child;
    if(child != NULL) child->parent = parent;
}

void createBinaryExpr(OpType op, Node* parent, Node* left, Node* right) {
    parent->nt = ExprNode;
    parent->expression.type = Binary;
    parent->expression.binary_op = op;
    parent->expression.next = NULL;
    addChildNode(parent, &(parent->expression.left), left);
    addChildNode(parent, &(parent->expression.right), right);
}

void createUnaryExpr(OpType op, Node* parent, Node* oprand) {
    parent->nt = ExprNode;
    parent->expression.type = Unary;
    parent->expression.unary_op = op;
    parent->expression.next = NULL;
    addChildNode(parent, &(parent->expression.oprand), oprand);
}

Node* createRefStrip(Node* id_ref, Node* expr_list) {
    Node *top_node = id_ref;
    while(expr_list != NULL) {
        Node *new_node = malloc(sizeof(Node));
        new_node->nt = ExprNode;
        new_node->loc = concat(top_node->loc, expr_list->loc);
        new_node->expression.type = Binary;
        new_node->expression.binary_op = OP_ARRAY_ACCESS;
        new_node->expression.next = NULL;
        addChildNode(new_node, &new_node->expression.left, top_node);
        addChildNode(new_node, &new_node->expression.right, expr_list);

        top_node = new_node;
        expr_list = expr_list->expression.next;
    }

    return top_node;
}

void createIfStatement(Node* tgt, Node* condition, Node* statement, Node* else_statement) {
    tgt->nt = IfNode;
    addChildNode(tgt, &tgt->if_node_attr.condition, condition);
    addChildNode(tgt, &tgt->if_node_attr.statement, statement);
    addChildNode(tgt, &tgt->if_node_attr.else_statement, else_statement);
}

void createWhileLoop(Node* tgt, Node* condition, Node* statement) {
    tgt->nt = WhileNode;
    addChildNode(tgt, &tgt->while_node_attr.condition, condition);
    addChildNode(tgt, &tgt->while_node_attr.statement, statement);
}

void createStmtList(StmtList* list, Node* stmt) {
    list->next = NULL;
    list->stmt = stmt;
}

void createDeclaration(Node* tgt, VarDescription* entry) {
    tgt->nt = DeclarationNode;
    tgt->declaration_node_attr.type = entry->type;
    tgt->declaration_node_attr.list = entry->list;
    tgt->declaration_node_attr.next = NULL;
}

void appendStmt(StmtList* tgt, StmtList* list, StmtList *entry) {
    tgt->stmt = entry->stmt;
    tgt->next = list;
}

char* op_mapping(OpType type) {
    switch (type) {
        case OP_ARRAY_ACCESS:
            return "ARRAY_ACCESS";
        case OP_AND:
            return "AND";
        case OP_OR:
            return "OR";
        case OP_ASSIGN:
            return "ASSIGN";
        case OP_ADD:
            return "ADD";
        case OP_SUB:
            return "SUB";
        case OP_MUL:
            return "MUL";
        case OP_DIV:
            return "DIV";
        case OP_LT:
            return "LT";
        case OP_GT:
            return "GT";
        case OP_EQ:
            return "EQ";
        case OP_GET:
            return "GET";
        case OP_LET:
            return "LET";
        case OP_NEQ:
            return "NEQ";
        default:
            return "";
    }
}

void printAST(Node *node, int level) {
    if(node == NULL) {
        for(int i = 0; i < level; i++) printf("  ");
        printf("Empty\n");
        return;
    }
    switch((int)node->nt) {
        case NullNode:{
            for(int i = 0; i < level; i++) printf("  ");
            printf("Empty\n");
            break;
        }
        case ProgNode:{
            for(int i = 0; i < level; i++) printf("  ");
            printf("ProgNode %s, ret_type: ", node->prog_node_attr.id);
            printVarType(node->prog_node_attr.ret_type);
            printf("\nArgs:\n");
            printAST(node->prog_node_attr.args, level+1);
            printf("Declaration:\n");
            printAST(node->prog_node_attr.declarations, level+1);
            printf("Subprogram:\n");
            printAST(node->prog_node_attr.subprogram_declarations, level+1);
            printf("Statement:\n");
            printAST(node->prog_node_attr.compound_statement, level+1);
            break;
        }
        case IDListNode:{
            for(IDList *cur = node->id_list; cur != NULL; cur = cur->next) {
                for(int i = 0; i < level; i++) printf("  ");
                printf("%s\n", cur->id);
            }
            break;
        }
        case DeclarationNode:{
            for(struct Node *cur = node; cur != NULL; cur = cur->declaration_node_attr.next) {
                for(int i = 0; i < level; i++) printf("  ");
                printVarType(cur->declaration_node_attr.type);
                printf("\n");
                for(IDList *cur_l = cur->declaration_node_attr.list; cur_l != NULL; cur_l = cur_l->next) {
                    for(int i = 0; i < level+1; i++) printf("  ");
                    printf("%s\n", cur_l->id);
                }
            }
            break;
        }
        case SubprogramDeclarationNode:{
            for(struct Node *cur = node; cur != NULL; cur = cur->subprogram_declaration_node_attr.next) {
                for(int i = 0; i < level; i++) printf("  ");
                printf("SubprogNode ret_type: ");
                printVarType(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.ret_type);
                printf("\n");
                for(int i = 0; i < level; i++) printf("  ");
                printf("Args:\n");
                printAST(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.args, level+1);
                for(int i = 0; i < level; i++) printf("  ");
                printf("Declaration:\n");
                printAST(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.declarations, level+1);
                for(int i = 0; i < level; i++) printf("  ");
                printf("Subprogram:\n");
                printAST(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.subprogram_declarations, level+1);
                for(int i = 0; i < level; i++) printf("  ");
                printf("Statement:\n");
                printAST(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.compound_statement, level+1);
            }
            break;
        }
        case CompoundStatementListNode:{
            for(StmtList *stmt = node->compound_stmt_node_attr.stmts; stmt != NULL; stmt = stmt->next) {
                printAST(stmt->stmt, level+1);
            }
            break;
        }
        case ExprNode:{
            for(Node* cur = node; cur != NULL; cur = cur->expression.next) {
                switch(cur->expression.type) {
                    case Primary:
                        for(int i = 0; i < level; i++) printf("  ");
                        if(cur->expression.var_type == VAR_INT) {
                            printf("int: %d\n", cur->expression.val);
                        } else if(cur->expression.var_type == VAR_REAL) {
                            printf("real: %f\n", cur->expression.dval);
                        } else {
                            printf("text: %s\n", cur->expression.text);
                        }
                        break;
                    case Ref:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("var: %s\n", cur->expression.var_id);
                        break;
                    case Unary:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("%s\n", cur->expression.unary_op == OP_NOT ? "NOT" : "NEG");
                        printAST(cur->expression.oprand, level+1);
                        break;
                    case Binary:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("%s\n", op_mapping(cur->expression.binary_op));
                        printAST(cur->expression.left, level+1);
                        printAST(cur->expression.right, level+1);
                        break;
                    case Func:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("FUNC %s\n", cur->expression.id);
                        printAST(cur->expression.args, level+1);
                        break;
                }
            }
            break;
        }
        case IfNode:
            for(int i = 0; i < level; i++) printf("  ");
            printf("IfNode\n");
            for(int i = 0; i < level; i++) printf("  ");
            printf("Condition\n");
            printAST(node->if_node_attr.condition, level+1);
            for(int i = 0; i < level; i++) printf("  ");
            printf("Statement\n");
            printAST(node->if_node_attr.statement, level+1);
            for(int i = 0; i < level; i++) printf("  ");
            printf("Else\n");
            printAST(node->if_node_attr.else_statement, level+1);
            break;
        case WhileNode:
            for(int i = 0; i < level; i++) printf("  ");
            printf("WhileNode\n");
            for(int i = 0; i < level; i++) printf("  ");
            printf("Condition\n");
            printAST(node->while_node_attr.condition, level+1);
            for(int i = 0; i < level; i++) printf("  ");
            printf("Statement\n");
            printAST(node->while_node_attr.statement, level+1);
            break;
    }
}

void printVarType(VarType *type) {
    if(type == NULL) {
        printf("void");
        return;
    }
    switch(type->type) {
        case NONE:
            printf("void");
            break;
        case VAR_ARRAY:
            printf("array of ");
            printVarType(type->of_type);
            break;
        case VAR_INT:
            printf("int");
            break;
        case VAR_REAL:
            printf("real");
            break;
        case VAR_TEXT:
            printf("text");
            break;
        case VAR_FUNCTION:
            printf("func");
            break;
    }
}