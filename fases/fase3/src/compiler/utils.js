import FortranTranslator from './Translator.js';

/** @typedef {import('../visitor/CST.js').Grammar} Grammar*/
/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor*/
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/
/** @typedef {import('../visitor/Visitor.js').Enqueues} Enqueues*/

/**
 *
 * @param {Grammar} cst
 */
export async function generateParser(cst) {
    /** @type {ActionTypes} */
    const ruleReturnTypes = {};
    for (const rule of cst.rules) {
        rule.expr.exprs.forEach((concat, i) => {
            if (concat.action) {
                const functionId = `peg_${rule.id}_f${i}`;
                ruleReturnTypes[functionId] = {type:concat.action.returnType,
                    isList:false,
                    isPointer:concat.action.returnType.toLowerCase().includes('pointer')};
            }
        });
    }

    /** @type {Visitor} */
    const translator = new FortranTranslator(ruleReturnTypes);
    return cst.accept(translator);
}

/**
 *
 * @param {string} ruleId
 * @param {number} choice
 * @returns
 */
export function getActionId(ruleId, choice) {
    return `peg_${ruleId}_f${choice}`;
}

/**
 *
 * @param {string} functionId
 * @param {ActionTypes} actionReturnTypes
 * @returns
 */
export function getReturnType(functionId, actionReturnTypes) {
    return actionReturnTypes[functionId]?.type ?? 'character(len=:), allocatable';
}

/**
 *
 * @param {number} choice
 * @param {number} index
 * @returns
 */
export function getExprId(choice, index) {
    return `expr_${choice}_${index}`;
}

/**
 *
 * @param {string} rule
 * @returns
 */
export function getRuleId(rule) {
    return `peg_${rule}`;
}



/**
 * @param {string} data
 * @returns {Enqueues}
*/
export const getEnqueues = (data) => {
    /** @type {Enqueues} */
    const actionType = {};
    // vamos a ver si el codigo global de fortran contiene types hechos por el usuario
    // y luego vamos a crear la funcion de add_to_list
    data.match(/type\s+::\s+\w+/g)?.map(match => {
        const clase = match.split('::')[1].trim();
        if (match.split('::')[0].includes('extends')) {
            actionType[clase] = { Declaration: `class(${clase})`,
            nameFunction: `add_to_list${clase}` };
        } else {
            actionType[clase] = { Declaration: `type(${clase})`,
            nameFunction: `add_to_list${clase}` };
        }
    });

    delete actionType['character'];

    actionType['integer'] = { Declaration: 'integer',
        nameFunction: 'add_to_list_integer' };
    actionType['real'] = { Declaration: 'real',
        nameFunction: 'add_to_list_real' };
    actionType['logical'] = { Declaration: 'logical',
        nameFunction: 'add_to_list_logical' };
    return actionType;
}
export function StringToFortranCode(input) {

    const escapeMap = {
        "\\n": 'achar(10)',
        "\\t": 'achar(9)',  
        "\\r": 'achar(13)', 
        "\\f": 'achar(12)', 
        "\\v": 'achar(11)', 
        "\\b": 'achar(8)',  
    };
 
    if (input in escapeMap) {
        return escapeMap[input];
    }

    let result = input.replace(/(\\[ntvbrf0\\])/g, match => {
        return `"//${escapeMap[match]}//"`;
    });
        result = `"${result}"`.replace(/\/\/"\/\/|\/\/"$/g, '//');

    return result;
}
