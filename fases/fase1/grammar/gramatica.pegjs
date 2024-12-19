{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';
}}

gramatica
  = _ prods:producciones+ _ {
      console.log("Producciones reconocidas:", prods);
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    return prods;
  }

producciones
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    console.log("Producción reconocida:", { id, alias, expr });
    ids.push(id);
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    console.log("Opciones reconocidas:", { expr, rest });
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )* {
    console.log("Unión reconocida:", { expr, rest });
    return new n.Union([expr, ...rest]);
  }

expresion
  = label:$(etiqueta/varios)? _ expr:expresiones _ qty:$([?+*]/conteo)? {
    return new n.Expresion(expr, label, qty);
  }

//Completar -- COMPLETADO
etiqueta
  = isOptional:(@"@" _)? id:identificador _ ":" varios:(varios)? {
       console.log("Etiqueta reconocida:", { isOptional, id, varios });
    return new n.Etiqueta(!!isOptional, id, varios || null);
  }

varios = ("!"/"$"/"@"/"&")

//Completar
expresiones
  //Completar
  = id:identificador {
        console.log("Identificador reconocido:", id);
    usos.push(id)
        console.log("Identificador reconocido:", id);
    return new n.Identificador(id); 
  }
  / val:$literales isCase:"i"? {return new n.String(val.replace(/['"]/g, ''), isCase);} //Listo
  / "(" _ opciones _ ")" { 
      return new n.Parentesis(opciones); 
  }           //COMPLETADO  opciones
  / lista:corchetes isCase:"i"? //Completar
  {return new n.Corchetes(lista, isCase)}
  / "." { 
      return new n.Punto(); 
  } //COMPLETADO
  / "!." { 
      return new n.NegacionPunto(); 
  }//COMPLETADO

//Completar
conteo   = "|" _ valor:(numero / identificador) _ "|" { //conteo 
      return new n.ConteoSimple(valor);
  }
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero / id:identificador)? _ "|" { //min .. max
      return new n.ConteoRango(inicio, fin);
  }
  / "|" _ valor:(numero / identificador)? _ "," _ opciones:opciones _ "|" { //conteo, delimitador
      return new n.ConteoOpciones(valor, opciones);
  }
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero / identificador)? _ "," _ opciones:opciones _ "|" {// min .. max, conteo
      return new n.ConteoRangoOpciones(inicio, fin, opciones);
  }


// Regla principal que analiza corchetes con contenido

//Completar, unicamnete guarda el contenido de los corchetes
corchetes
    = "[" contenido:(rango/contenido)+ "]" {
        return `Entrada válida: [${input}]`;
    }
    //[0abc0-9]=[0,a,b,c,0,1,2,3,4,5,6,7,8,9]

// Regla para validar un rango como [A-Z]
//Completar -- COMPLETADO 
rango
    = inicio:caracter "-" fin:caracter {
        if (inicio.charCodeAt(0) > fin.charCodeAt(0)) {
            throw new Error(`Rango inválido: [${inicio}-${fin}]`);
        }
        const result = [];
        for (let i = inicio.charCodeAt(0); i <= fin.charCodeAt(0); i++) {
          result.push(String.fromCodePoint(i));
        }
        return result;//retornar toda la lista 
    }

// Regla para caracteres individuales

//Completado
caracter
    = [a-zA-Z0-9_ ] { return text()}

// Coincide con cualquier contenido que no incluya "]"

//Completar
contenido
  = elementos:(corchete / texto)+ {
      return new n.Contenido(elementos);
  }

corchete
    = "[" contenido "]"


texto
    = [^\[\]]+

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

//Completar
numero
  = valor:[0-9]+ {
      return new n.Numero(parseInt(valor.join(''), 10));
  }

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
