const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['expr', 'label', 'qty'],
    String: ['val', 'isCase'],
    Corchetes: ['lista', 'isCase'],
    Etiqueta: ['isOptional', 'id', 'varios'],
    Identificador: ['id'], 
    Parentesis: ['contenido'], 
    Punto: [], 
    NegacionPunto: [], 
    ConteoSimple: ['valor'], 
    ConteoRango: ['inicio', 'fin'], 
    ConteoOpciones: ['valor', 'opciones'], 
    ConteoRangoOpciones: ['inicio', 'fin', 'opciones'], 
    Contenido: ['elementos'],
    Numero: ['valor'],
};

export default nodes;
