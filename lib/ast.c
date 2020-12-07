#include <ast.h>

void createBinaryExpr(OpType op, Node* parent, Node* left, Node* right) {
    parent->nt = ExprNode;
    parent->expression.type = Binary;
    parent->expression.binary.op = op;
    parent->expression.binary.left = left;
    parent->expression.binary.right = right;
    parent->expression.next = NULL;
}

void createUnaryExpr(OpType op, Node* parent, Node* oprand) {
    parent->nt = ExprNode;
    parent->expression.type = Unary;
    parent->expression.unary.op = op;
    parent->expression.unary.oprand = oprand;
    parent->expression.next = NULL;
}

Node* createRefStrip(Node* id_ref, Node* expr_list) {
    Node *top_node = id_ref;
    while(expr_list != NULL) {
        Node *new_node = malloc(sizeof(Node));
        new_node->nt = ExprNode;
        new_node->loc = expr_list->loc;
        new_node->expression.type = Binary;
        new_node->expression.binary.op = OP_ARRAY_ACCESS;
        new_node->expression.binary.left = top_node;
        new_node->expression.binary.right = expr_list;
        new_node->expression.next = NULL;

        top_node = new_node;
        expr_list = expr_list->expression.next;
    }

    return top_node;
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
        case ProgNode:{
            for(int i = 0; i < level; i++) printf("  ");
            printf("ProgNode %s, ret_type: ", node->prog_node_attr.id);
            printVarType(node->prog_node_attr.type);
            printf("\nDeclaration:\n");
            printAST(node->prog_node_attr.declarations, level+1);
            printf("Subprogram:\n");
            printAST(node->prog_node_attr.subprogram_declarations, level+1);
            printf("Statement:\n");
            printAST(node->prog_node_attr.compound_statement, level+1);
            break;
        }
        case DeclarationNode:{
            for(struct Node *cur = node; cur != NULL; cur = cur->declaration_node_attr.next) {
                for(int i = 0; i < level; i++) printf("  ");
                printf("%s : ", cur->declaration_node_attr.id);
                printVarType(cur->declaration_node_attr.type);
                printf("\n");
            }
            break;
        }
        case SubprogramDeclarationNode:{
            for(struct Node *cur = node; cur != NULL; cur = cur->subprogram_declaration_node_attr.next) {
                for(int i = 0; i < level; i++) printf("  ");
                printf("SubprogNode ret_type: ");
                printVarType(cur->subprogram_declaration_node_attr.subprogram->prog_node_attr.type);
                printf("\n");
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
            for(Node *stmt = node->compound_stmt_node_attr.stmts; stmt != NULL; stmt = stmt->compound_stmt_node_attr.next) {
                printAST(stmt->compound_stmt_node_attr.stmts, level+1);
            }
            break;
        }
        case ExprNode:{
            for(Node* cur = node; cur != NULL; cur = cur->expression.next) {
                switch(cur->expression.type) {
                    case Primary:
                        for(int i = 0; i < level; i++) printf("  ");
                        if(cur->expression.primary.var_type == VAR_INT) {
                            printf("int: %d\n", cur->expression.primary.val);
                        } else if(cur->expression.primary.var_type == VAR_REAL) {
                            printf("real: %f\n", cur->expression.primary.dval);
                        } else {
                            printf("text: %s\n", cur->expression.primary.text);
                        }
                        break;
                    case Ref:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("var: %s %d,%d\n", cur->expression.ref.id, cur->loc.first_line, cur->loc.first_column);
                        break;
                    case Unary:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("%s\n", cur->expression.unary.op == OP_NOT ? "NOT" : "NEG");
                        printAST(cur->expression.unary.oprand, level+1);
                        break;
                    case Binary:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("%s\n", op_mapping(cur->expression.binary.op));
                        printAST(cur->expression.binary.left, level+1);
                        printAST(cur->expression.binary.right, level+1);
                        break;
                    case Func:
                        for(int i = 0; i < level; i++) printf("  ");
                        printf("FUNC %s\n", cur->expression.func.id);
                        printAST(cur->expression.func.args, level+1);
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
            printf("array [%d..%d] of ", type->begin->expression.primary.val, type->end->expression.primary.val);
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
            printf("func ret_type: ");
            printVarType(type->ret_type);
            printf(" args: ");
            for(Node* cur = type->args; cur != NULL; cur = cur->declaration_node_attr.next) {
                printf("%s:", cur->declaration_node_attr.id);
                printVarType(cur->declaration_node_attr.type);
                printf(" ");
            }
            break;
    }
}