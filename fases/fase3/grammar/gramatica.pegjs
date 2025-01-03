{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';

}}

gramatica
  = _ code:globalCode? prods:regla+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    return new n.Grammar(prods, code);
  }

globalCode
  = "{" before:$(. !"contains")* [ \t\n\r]* "contains" [ \t\n\r]* after:$[^}]* "}" {
    return after ? {before, after} : {before}
  }

regla
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Regla(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:parsingExpression rest:(_ @parsingExpression !(_ literales? _ "=") )* action:(_ @predicate)? {
    const exprs = [expr, ...rest];
    const labeledExprs = exprs
        .filter((expr) => expr instanceof n.Pluck)
        .filter((expr) => expr.labeledExpr.label);
    if (labeledExprs.length > 0) {
        action.params = labeledExprs.reduce((args, labeled) => {
            const expr = labeled.labeledExpr.annotatedExpr.expr;
            args[labeled.labeledExpr.label] =
                expr instanceof n.Identificador ? expr.id : '';
            return args;
        }, {});
        console.log("action", action.params);
    }
    return new n.Union(exprs, action);
  }

parsingExpression
  = pluck
  / '!' assertion:(match/predicate) { // ya no se podria hacer pluck en una negacion  !@id, al igual que en una assertion 
    return new n.NegAssertion(assertion); 
  }
  / '&' assertion:(match/predicate) {
    return new n.Assertion(assertion);
  }
  / "!." {
    return new n.Fin();
  }

pluck
  = pluck:"@"? _ labeledExpr:label {
    return new n.Pluck(labeledExpr, pluck ? true : false);
  }

label
  = label:(@identificador _ ":")? _ expr:annotated {
    return new n.Label(expr, label);
  }

annotated
  = text:"$"? _ expr:match _ qty:([?+*]/conteo)? {
    return new n.Annotated(expr, qty, text ? true : false);
  }

match
  = id:identificador {
    usos.push(id)
    return new n.Identificador(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase ? true : false);
  }
  / "(" _ opciones:opciones _ ")"{
    return new n.Parentesis(opciones)
  }
  / chars:clase isCase:"i"? {
    return new n.Clase(chars, isCase ? true : false);
  }
  / "." {
    return new n.Punto();
  }


conteo   = "|" _ valor:(numero / identificador/predicate) _ "|" { //conteo 
      return new n.ConteoSimple(valor);
    }
  / "|" _ inicio:(numero / identificador/predicate)? _ ".." _ fin:(numero / identificador/predicate)? _ "|" { //min .. max
      return new n.ConteoRango(inicio, fin);
    }
  / "|" _ valor:(numero / identificador/predicate)? _ "," _ opciones:(match/opciones/predicate)+ _ "|" { //conteo, delimitador
      return new n.ConteoOpciones(opciones, valor);
    }
  / "|" _ inicio:(numero / identificador/predicate)? _ ".." _ fin:(numero / identificador/predicate)? _ "," _ opciones:(match/opciones)+ _ "|" {// min .. max, delimitador
      return new n.ConteoRangoOpciones(opciones, inicio, fin);
    }



// Conteo con match 
//conteo
  //= "|" _ (numero / id:identificador) _ "|"
  // / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
  // / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
  // / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

predicate
  = "{" [ \t\n\r]* returnType:predicateReturnType code:$[^}]* "}" {
    return new n.Predicate(returnType, code, {})
  }

predicateReturnType
  = t:$(. !"::")+ [ \t\n\r]* "::" [ \t\n\r]* "res" {
    return t.trim();
  }

clase
  = "[" @contenidoClase+ "]"

contenidoClase
  =  "\\" escape:escape2 { console.log(escape.charCodeAt(0)); 
    return `achar(${escape.charCodeAt(0)})`;}
  /bottom:$[^\[\]] "-" top:$[^\[\]] {
    return new n.Rango(bottom, top);
  }
  / $[^\[\]] {
    if (text() === " ") {
      return `achar(${text().charCodeAt(0)})`;
    }
    return `"${text()}"`
  }

escape2 = "b" { return "\b"; } 
        / "f" { return "\f"; } 
        / "n" { return "\n"; } 
        / "r" { return "\r"; } 
        / "t" { return "\t"; } 
        / "v" { return "\v"; }


literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    
numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ = (Comentarios /[ \t\n\r])*

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
