import * as CST from '../visitor/CST.js';
import * as Template from '../Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId, getEnqueues,StringToFortranCode } from './utils.js';

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/
/** @typedef {import('../visitor/Visitor.js').Enqueues} Enqueues */

/**
 * @implements {Visitor}
 */
export default class FortranTranslator {
    /** @type {ActionTypes} */
    actionReturnTypes;
    /** @type {string[]} */
    actions;
    /** @type {boolean} */
    translatingStart;
    /** @type {string} */
    currentRule;
    /** @type {number} */
    currentChoice;
    /** @type {number} */
    currentExpr;
    /** @type {Enqueues} */
    enqueuesList;
    /** @type {number} */
    GlobalParentCounter;
    /** @type {string[]} */
    ParentesisFunctions;
    /** @type {boolean} */
    IsInParentesis;
    /**
     *
     * @param {ActionTypes} returnTypes
     */
    constructor(returnTypes) {
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
        this.enqueuesList = {};
        this.GlobalParentCounter=0;
        this.ParentesisFunctions=[];
        this.IsInParentesis=false;
    }

    /**
     * @param {CST.Grammar} node
     * @this {Visitor}
     */
    visitGrammar(node) {
        console.log(this.actionReturnTypes);
        const rules = node.rules.map((rule) => rule.accept(this));

        this.enqueuesList = getEnqueues(node.globalCode?.before ?? '');
        const variableDeclarations = `integer :: i, j, temporalCursor`;   // Nuevas variables


        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            rules,
            enqueuesList: this.enqueuesList,
            parentFunctions: this.ParentesisFunctions,
        });
    }

    /**
     * @param {CST.Regla} node
     * @this {Visitor}
     */
    visitRegla(node) {
        this.currentRule = node.id;
        this.currentChoice = 0;

        if (node.start) this.translatingStart = true;
        console.log("node",node)

        const ruleTranslation = Template.rule({
            id: node.id,
            returnType: getReturnType(
                getActionId(node.id, this.currentChoice),
                this.actionReturnTypes
            ),
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>{
                let salida = election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .map((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;
                        const qty = label.labeledExpr.annotatedExpr.qty;
                        let tipo = expr instanceof CST.Identificador ? getReturnType(
                            getActionId(expr.id, 0),
                            this.actionReturnTypes
                        ) : 'character(len=:), allocatable';//no esta encontrando el tipo de retorno
                       
                        let salida = '';
                        if (typeof qty === 'string' && qty !== "?" && expr instanceof CST.Identificador) {
                            if (this.actionReturnTypes[getActionId(expr.id, 0)]) {
                                this.actionReturnTypes[getActionId(expr.id, 0)].isList = true;
                            }
                            if (tipo.includes('character')) {
                                salida = `${tipo} :: expr_${i}_${j}(:)\n`;
                                salida += `${tipo} :: expr_${i}_${j}_temp`
                            } else {
                                salida = `${tipo}, allocatable :: expr_${i}_${j}(:)\n`;
                                salida += `${tipo} :: expr_${i}_${j}_temp`
                            }
                        }else {
                            salida = `${tipo} :: expr_${i}_${j}`;
                        }
                        
                        return salida;
                    })
                    salida.concat(election.exprs
                    .filter((expr) => expr instanceof CST.NegAssertion || expr instanceof CST.Assertion)
                    .map((label, j) => {
                        const expr = label.assertion;
                        let tipo = expr instanceof CST.Identificador ? getReturnType(
                            getActionId(expr.id, 0),
                            this.actionReturnTypes
                        ) : 'character(len=:), allocatable';
                        let salida = '';
                        if (label instanceof CST.NegAssertion) {
                            salida = `${tipo} :: NegAssertion_${i}_${j}`;
                        } else {
                            salida = `${tipo} :: Assertion_${i}_${j}`;
                        }
                        return salida;
                    }))
                    let variable = "cosa";
                    variable.includes("Assertion_");
                    variable.includes("NegAssertion_"); 
                return salida;
                }),
            expr: node.expr.accept(this),
        });

        this.translatingStart = false;

        return ruleTranslation;
    }

    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
        });
    }

    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `expr_${this.currentChoice}_${i}`
        );

        /** @type {string[]} */
        let neededExprs;
        /** @type {string} */
        let resultExpr;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => 
                    matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],

            },this.actionReturnTypes);
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));
        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr:`${this.IsInParentesis ? ``: resultExpr}`,
        });
    }

    /**
     * @param {CST.Pluck} node
     * @this {Visitor}
     */
    visitPluck(node) {
        return node.labeledExpr.accept(this);
    }

    /**
     * @param {CST.Label} node
     * @this {Visitor}
     */
    visitLabel(node) {
        return node.annotatedExpr.accept(this);
    }

    /**
     * @param {CST.Annotated} node
     * @this {Visitor}
     */
    visitAnnotated(node) {
        if (node.qty && typeof node.qty === 'string') {
            if (node.expr instanceof CST.Identificador) {

                // TODO: Implement quantifiers (i.e., ?, *, +)
                return Template.strQtyIdent({
                        qty:node.qty,
                        callRule: `${getExprId(
                            this.currentChoice,
                            this.currentExpr
                        )}_temp = ${node.expr.accept(this)}`,
                        pila: `peg_pull(${getExprId(
                            this.currentChoice,
                            this.currentExpr
                        )} , ${getExprId(
                            this.currentChoice,
                            this.currentExpr
                        )}_temp)
                        `,
                    });
            }
            return Template.strExpr({
                quantifier: node.qty,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {
            // TODO: Implement repetitions (e.g., |3|, |1..3|, etc...)
            throw new Error('Repetitions not implemented.');
        } else {
            if (node.expr instanceof CST.Parentesis) {
                return  Template.ParentExpr({
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }

            if (node.expr instanceof CST.Identificador) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}
                if(.not. accept) cycle
                `;
            }
            return Template.strExpr({
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        }
    }

    /**
     * @param {CST.Assertion} node
     * @this {Visitor}
     */
    visitAssertion(node) {
        if(node.assertion instanceof CST.Identificador) {
            
            return  Template.NoTerminalAssertion({
                callRule: `${getExprId(
                this.currentChoice,
                this.currentExpr
            )} = ${node.assertion.accept(this)}`
            }) 
            ;

            //asercion para identificadores
            //return node.expr.accept(this);
        } else if (node.assertion instanceof CST.Predicate) {
            //asercion para predicados
           return "";
        } 
        return Template.TerminalAssertion({
            expr: node.assertion.accept(this),
        })
       
    }

    /**
     * @param {CST.NegAssertion} node
     * @this {Visitor}
     */
    visitNegAssertion(node) {
        if(node.assertion instanceof CST.Identificador) {
            
            return  Template.NoTerminalNegAssertion({
                callRule: `${getExprId(
                this.currentChoice,
                this.currentExpr
            )} = ${node.assertion.accept(this)}`
            }) 
            ;

            //asercion para identificadores
            //return node.expr.accept(this);
        } else if (node.assertion instanceof CST.Predicate) {
            //asercion para predicados
            return "";
        } 
        return Template.TerminalNegAssertion({
            expr: node.assertion.accept(this),

        })
    }


    // IDEA ASSERCION NEGATIVA 

    
    // visitNegAssertion(node) {
//     if(node.assertion instanceof CST.Identificador) {
//         return Template.NoTerminalNegAssertion({
//             callRule: `${getExprId(
//                 this.currentChoice,
//                 this.currentExpr
//             )} = ${node.assertion.accept(this)}`
//         });
//     } else if (node.assertion instanceof CST.Predicate) {
//         return Template.TerminalNegAssertion({
//             expr: node.assertion.accept(this)
//         });
//     }

//     return node.assertion.accept(this);
// }

    /**
     * @param {CST.Predicate} node
     * @this {Visitor}
     */
    visitPredicate(node) {
        console.log("visitPredicate:",node.params);
        console.log("actionReturnTypes:",this.actionReturnTypes);
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, ruleId]) =>{
                    // let salida = `${getReturnType(
                    //     getActionId(ruleId, this.currentChoice),
                    //     this.actionReturnTypes
                    // )} :: ${this.actionReturnTypes[getActionId(ruleId, this.currentChoice)]?.isList ? `${label}(:)` : label}`;
                    let salida = getReturnType(
                        getActionId(ruleId, 0),
                        this.actionReturnTypes
                    );
                    if (this.actionReturnTypes[getActionId(ruleId, this.currentChoice)]?.isList){
                        if (salida.includes('character')) {
                            salida = `${salida.replace(":","*")}, intent(inout) :: ${label}(:)`
                        } else {
                            salida = `${salida}, allocatable, intent(inout) :: ${label}(:)`
                        }
                    } else {
                        salida = `${salida}, intent(inout) :: ${label}`
                    }
                    return salida;
                }
            ),
            code: node.code,
        });
    }

    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        let result = StringToFortranCode(node.val);

        return `acceptString(${result}, ${node.isCase ? '.true.' : '.false.'})`;
    }

    /**
     * @param {CST.Clase} node
     * @this {Visitor}
     */
    visitClase(node) {
        let characterClass = [];
        const isCase = node.isCase;
        
        const set = node.chars
            .filter((char) => typeof char === 'string')
            .map((char) => `${char}`);
            
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => `acceptRange('${range.bottom}', '${range.top}', ${isCase ? '.true.' : '.false.'})`);
            
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}], ${isCase ? '.true.' : '.false.'})`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return `(${characterClass.join(' .or. ')})`;
    }

    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        // Este método ya no se usa directamente desde visitClase
        return `acceptRange('${node.bottom}', '${node.top}', .false.)`;
    }

    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        return getRuleId(node.id) + '(accept)';
    }

    /**
     * @param {CST.Punto} node
     * @this {Visitor}
     */
    visitPunto(node) {
        return 'acceptPeriod()';
    }

    visitConteoOpciones(node) {
        return ""
    }

    visitConteoRangoOpciones(node) {
        return ""
    }

    visitConteoRango(node) {
        return ""
    }

    visitConteoSimple(node) {
        return ""
    }

    /**
     * @param {CST.Fin} node
     * @this {Visitor}
     */
    visitFin(node) {
        return 'if (.not. acceptEOF()) cycle';
    }

    /**
     * @param {CST.Parentesis} node
     * @this {Visitor}
     */
    visitParentesis(node) {
        let saveInParentesis= this.IsInParentesis;
        this.IsInParentesis=true;
        let saveCurrentChoice= this.currentChoice;
        let saveCurrentExpr= this.currentExpr;
        this.currentExpr = 0;
        this.currentChoice = 0;
        let temporalCounter= this.GlobalParentCounter
        this.GlobalParentCounter++;

        const ruleTranslation = Template.Parentesis({
            id: `${temporalCounter}`,
            returnType: 'character(len=:), allocatable',
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>{
                let salida = election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .map((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;
                        const qty = label.labeledExpr.annotatedExpr.qty;
                        
                        let tipo = expr instanceof CST.Identificador ? getReturnType(
                            getActionId(expr.id, i),
                            this.actionReturnTypes
                        ) : 'character(len=:), allocatable';
                        
                        let salida = '';
                        if (typeof qty === 'string' && qty !== "?" && expr instanceof CST.Identificador) {
                            this.actionReturnTypes[getActionId(expr.id, i)].isList = true;
                            if (tipo.includes('character')) {
                                salida = `${tipo} :: expr_${i}_${j}(:)\n`;
                                salida += `${tipo} :: expr_${i}_${j}_temp`
                            } else {
                                salida = `${tipo}, allocatable :: expr_${i}_${j}(:)\n`;
                                salida += `${tipo} :: expr_${i}_${j}_temp`
                            }
                        }else {
                            salida = `${tipo} :: expr_${i}_${j}`;
                        }
                        return salida;
                    })
                    salida.concat(election.exprs
                    .filter((expr) => expr instanceof CST.NegAssertion || expr instanceof CST.Assertion)
                    .map((label, j) => {
                        const expr = label.assertion;
                        let tipo = expr instanceof CST.Identificador ? getReturnType(
                            getActionId(expr.id, i),
                            this.actionReturnTypes
                        ) : 'character(len=:), allocatable';
                        let salida = '';
                        if (label instanceof CST.NegAssertion) {
                            salida = `${tipo} :: NegAssertion_${i}_${j}`;
                        } else {
                            salida = `${tipo} :: Assertion_${i}_${j}`;
                        }
                        return salida;
                    }))
                    let variable = "cosa";
                    variable.includes("Assertion_");
                    variable.includes("NegAssertion_"); 
                return salida;
                }),
            expr: node.expr.accept(this),
        });
        this.ParentesisFunctions.push(ruleTranslation);
        let salida=`peg_P${temporalCounter}(accept)`

        this.currentChoice= saveCurrentChoice
        this.currentExpr=saveCurrentExpr
        this.IsInParentesis=saveInParentesis;
        return salida;
    }
}
