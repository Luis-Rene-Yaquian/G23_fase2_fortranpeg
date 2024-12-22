{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos, usosRef} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';
}}

gramatica
  = _ prods:producciones+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    console.log("gramatica, producciones:", prods);
    return prods;
  }

producciones
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    // console.log("Producción reconocida:", { id, alias, expr });
    ids.push(id);
    if (alias) {
      alias = alias.toString().replace(/['"]/g, '');
      return new n.Producciones(id, expr, alias.replace(/['"]/g, ''));
    }
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    // console.log("Opciones reconocidas:", [expr, ...rest]);
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )* {
    // console.log("Unión reconocidaa:", [expr, ...rest] );
    return new n.Union([expr, ...rest]);
  }

expresion
  = isOptional:"@"?  id:(@identificador _ ":")? label:varios?  _ expr:expresiones _ qty:([?+*]/conteo)? {
        // console.log("Expresión reconocida:", { isOptional, id, label, expr, qty });
        return new n.Expresion(isOptional, id, label, expr, qty);
  }

//Errres encontrados: podia venir @pluck:@"expresion"  o 
/*expresion  = (etiqueta/varios)? _ expresiones _ ([?+*]/conteo)?

etiqueta = ("@")? _ id:identificador _ ":" (varios)?

 varios = ("!"/"$"/"@"/"&")*/

varios = ("!"/"&"/"$")


//Completar
expresiones
  //Completar
  = id:identificador {
      let idTemp = new n.Identificador(id);
      usos.push(id);
      usosRef.push(idTemp);
      return idTemp; 
    }
  / val:$literales isCase:"i"? {
      return new n.String(val.replace(/['"]/g, ''), isCase);
    } //Listo
  / "(" _ opciones:opciones _ ")" { 
      return new n.Parentesis(opciones); 
    }           //COMPLETADO  opciones
  / chars:corchetes isCase:"i"?{
      return new n.Corchetes(chars, isCase) //Completar
    }
  / "." { 
      return new n.Punto(); 
    } //COMPLETADO
  / "!." { 
      return new n.NegacionPunto(); 
    }//COMPLETADO

//Completar
conteo   = "|" _ valor:(numero / identificador) _ "|" { //conteo 
      return new n.ConteoSimple(val);
    }
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero / id:identificador)? _ "|" { //min .. max
      return new n.ConteoRango(inicio, fin);
    }
  / "|" _ valor:(numero / identificador)? _ "," _ opciones:opciones _ "|" { //conteo, delimitador
      return new n.ConteoOpciones(val, opciones);
    }
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero / identificador)? _ "," _ opciones:opciones _ "|" {// min .. max, delimitador
      return new n.ConteoRangoOpciones(inicio, fin, opciones);
    }


// Regla principal que analiza corchetes con contenido

//Completar, unicamnete guarda el contenido de los corchetes
// corchetes
//     = "[" contenido:(rango/texto)+ "]" {
      
//         return [...new Set(contenido.reduce((acc, curr) => acc.concat(curr), []))];
//     }
//     //[0abc0-9]=[0,a,b,c,0,1,2,3,4,5,6,7,8,9]

// // Regla para validar un rango como [A-Z]
// //Completar -- COMPLETADO 
// rango
//     = inicio:caracter "-" fin:caracter {
//         if (inicio.charCodeAt(0) > fin.charCodeAt(0)) {
//             throw new Error(`Rango inválido: [${inicio}-${fin}]`);
//         }
//         const result = [];
//         for (let i = inicio.charCodeAt(0); i <= fin.charCodeAt(0); i++) {
//           result.push(String.fromCodePoint(i));
//         }
//         return result;//retornar toda la lista 
//     }

corchetes
  = "[" @contenidoChars+ "]"


contenidoChars
  = "\\" escape:escape {return `\\${escape}`}
  /  bottom:$[^\[\]] "-" top:$[^\[\]] {
    return new n.Rango(bottom, top);
  }
  / $[^\[\]]
  
// Regla para caracteres individuales

//Completado
// caracter
//     = [a-zA-Z0-9_ ] { return text()}

// Coincide con cualquier contenido que no incluya "]"

/* GRAMATICAS ANTERIORES, DAN ERROR AL TRATAR DE RECONOCER EJ: [abc0-3], reconocimiento esperado: [a,b,c,1,2,3]
                                                                  Salida que se obtiene: [a,b,c,1,-,3]

Completar
 contenido
   = elementos:(corchete / texto)+ {
      return new n.Contenido(elementos);
  }

 corchete
    = "[" contenido "]" 
*/

// texto
//     = [^\[\]]

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    //(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
                    // / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    //(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
                    // / continuacionLinea

//(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
// continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t" {return `achar(${text().toCharCode(0)})`}
        / "v"
        / "u"

//(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
// secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

//Completar
numero
  = val:[0-9]+ {
      return parseInt(valor.join(''), 10);
  }

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
