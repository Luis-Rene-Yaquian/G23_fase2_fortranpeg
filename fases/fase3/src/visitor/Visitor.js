
// Auto-generated

/** @typedef {import('./Node.js').default} Node*/
/**
 * @typedef {{
 *  [rule: string]: { type: string; isList: boolean; isPointer: boolean;}
 * }} ActionTypes
 */

/** @typedef {{ [key: string]: { Declaration: string; nameFunction: string; } }} Enqueues*/
/**
 * @interface
 * @template T
 */
export default class Visitor {
    /**
     * Table that contains the function identifier for a rule's choices
     * and their fortran return type.
     * If a rule's choice doesn't have an entry in the table, it means
     * it doesn't have an action associated with it and should
     * be interpreted as having a character(len=:) return type by default.
     * @type {ActionTypes}
     */
    actionReturnTypes;
    /**
     * List with all the actions' code.
     * @type {string[]}
     */
    actions
    /**
     * Wheter we are traversing the starting rule or not.
     * @type {boolean}
     */
    translatingStart;
    /**
     * Id of the current rule we are traversing.
     * @type {string}
     */
    currentRule;
    /**
     * A rule can have many choices (e.g., rule = a/b/c). This variable stores
     * the index of the current choice in the current rule.
     * @type {number}
     */
    currentChoice;
    /**
     * A choice can have many expresions (e.g., rule = abc). This variable stores
     * the index of the current expression in the current choice.
     * @type {number}
     */
    currentExpr;
    /** 
     *  This variable saves the current classes that are being used in the grammar
     *  because they are used in the generated code.
     * @type {Enqueues}
     */
     enqueuesList;
    /**
     * Count partent funcntions
     * @type {number}
     */
    GlobalParentCounter;
     /**
     * list with all the parent functions
     * @type {string[]}
     */
     ParentesisFunctions
      /**
     * list with all the parent functions
     * @type {boolean}
     */
      IsInParentesis;
    
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitGrammar(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitRegla(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitOpciones(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitUnion(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitPredicate(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitConteoRango(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitConteoSimple(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitConteoOpciones(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitConteoRangoOpciones(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitPluck(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitLabel(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitAnnotated(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitAssertion(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitNegAssertion(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitString(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitParentesis(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitClase(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitRango(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitIdentificador(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitPunto(node){
        throw new Error('Implement in subclass');
    }
	
    /**
     * @abstract
     * @param {Node} node
     * @returns {T}
     */
    visitFin(node){
        throw new Error('Implement in subclass');
    }
}