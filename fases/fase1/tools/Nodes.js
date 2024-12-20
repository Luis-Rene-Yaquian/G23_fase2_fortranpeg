const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['isOptional', 'id', 'label', 'expr', 'qty'],
    String: ['val', 'isCase'],
    Corchetes: ['chars', 'isCase'],
    Identificador: ['id'], 
    Parentesis: ['contenido'], 
    Punto: [], 
    NegacionPunto: [], 
    ConteoSimple: ['val'], 
    ConteoRango: ['inicio', 'fin'], 
    ConteoOpciones: ['val', 'opciones'], 
    ConteoRangoOpciones: ['inicio', 'fin', 'opciones'], 
    Contenido: ['elementos'],
    Rango: ['bottom', 'top'],
};  

export default nodes;


// const nodes = {
//     Producciones: ['id', 'expr', 'alias'],
//     Opciones: ['exprs'],
//     Union: ['exprs'],
//     Expresion: ['isOptional', 'id', 'label', 'expr', 'qty'],
//     String: ['val', 'isCase'],
//     Corchetes: ['lista', 'isCase'],
//     Etiqueta: ['isOptional', 'id', 'varios'],
//     Identificador: ['id'], 
//     Parentesis: ['contenido'], 
//     Punto: [], 
//     NegacionPunto: [], 
//     ConteoSimple: ['valor'],   //Se cambio valor tambíen en la gramatica por si truena
//     ConteoRango: ['inicio', 'fin'], 
//     ConteoOpciones: ['valor', 'opciones'],  //Se cambio valor tambíen en la gramatica por si truena 
//     ConteoRangoOpciones: ['inicio', 'fin', 'opciones'], 
//     Contenido: ['elementos'],
//     Numero: ['valor'], //Se cambio valor tambíen en la gramatica por si truena 
//     Rango: ['bottom', 'top'],
// };

// export default nodes;
