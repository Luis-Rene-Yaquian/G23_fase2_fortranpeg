
// Auto-generated
import Node from './Node.js';

export class Producciones extends Node {
    constructor(id, expr, alias) {
        super();
        this.id = id;
		this.expr = expr;
		this.alias = alias;
    } 

    accept(visitor) {
        return visitor.visitProducciones(this);
    }
}
    
export class Opciones extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitOpciones(this);
    }
}
    
export class Union extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitUnion(this);
    }
}
    
export class Expresion extends Node {
    constructor(isOptional, id, label, expr, qty) {
        super();
        this.isOptional = isOptional;
		this.id = id;
		this.label = label;
		this.expr = expr;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitExpresion(this);
    }
}
    
export class String extends Node {
    constructor(val, isCase) {
        super();
        this.val = val;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitString(this);
    }
}
    
export class Corchetes extends Node {
    constructor(chars, isCase) {
        super();
        this.chars = chars;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitCorchetes(this);
    }
}
    
export class Identificador extends Node {
    constructor(id) {
        super();
        this.id = id;
    }

    accept(visitor) {
        return visitor.visitIdentificador(this);
    }
}
    
export class Parentesis extends Node {
    constructor(contenido) {
        super();
        this.contenido = contenido;
    }

    accept(visitor) {
        return visitor.visitParentesis(this);
    }
}
    
export class Punto extends Node {
    constructor() {
        super();
        
    }

    accept(visitor) {
        return visitor.visitPunto(this);
    }
}
    
export class NegacionPunto extends Node {
    constructor() {
        super();
        
    }

    accept(visitor) {
        return visitor.visitNegacionPunto(this);
    }
}
    
export class ConteoSimple extends Node {
    constructor(val) {
        super();
        this.val = val;
    }

    accept(visitor) {
        return visitor.visitConteoSimple(this);
    }
}
    
export class ConteoRango extends Node {
    constructor(inicio, fin) {
        super();
        this.inicio = inicio;
		this.fin = fin;
    }

    accept(visitor) {
        return visitor.visitConteoRango(this);
    }
}
    
export class ConteoOpciones extends Node {
    constructor(val, opciones) {
        super();
        this.val = val;
		this.opciones = opciones;
    }

    accept(visitor) {
        return visitor.visitConteoOpciones(this);
    }
}
    
export class ConteoRangoOpciones extends Node {
    constructor(inicio, fin, opciones) {
        super();
        this.inicio = inicio;
		this.fin = fin;
		this.opciones = opciones;
    }

    accept(visitor) {
        return visitor.visitConteoRangoOpciones(this);
    }
}
    
export class Contenido extends Node {
    constructor(elementos) {
        super();
        this.elementos = elementos;
    }

    accept(visitor) {
        return visitor.visitContenido(this);
    }
}
    
export class Rango extends Node {
    constructor(bottom, top) {
        super();
        this.bottom = bottom;
		this.top = top;
    }

    accept(visitor) {
        return visitor.visitRango(this);
    }
}
    