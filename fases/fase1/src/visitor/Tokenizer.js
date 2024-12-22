import Visitor from './Visitor.js';
import * as n from '../visitor/CST.js';
let noTerminals = [];
let firstNonTerminal = false;

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        //a los no terminales le asignamos las grammar que son las producciones
        noTerminals = grammar;
        // console.log("grammar en generateTokenizer: ",grammar)
        return `
module tokenizer
    implicit none
    private :: to_lowercase
    public :: nextSym

    contains
    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        integer :: i
        logical :: salida

        if (cursor > len(input)) then
            allocate( character(len=3) :: lexeme )
            lexeme = "EOF"
            return
        end if
        salida = ${grammar[0].id}(input, cursor)

        
        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        lexeme = "ERROR"
        end function nextSym

        ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    function to_lowercase(str) result(lower)
        character(len=*), intent(in) :: str
        integer :: i, char_code
        character(len=len(str)) :: lower

        do i = 1, len_trim(str)
            char_code = iachar(str(i:i))
            ! Si el carácter es una letra mayúscula (A-Z), convertirlo a minúscula
            if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                lower(i:i) = achar(char_code + 32)
            else
                lower(i:i) = str(i:i)
            end if
        end do
    end function to_lowercase

end module tokenizer 
        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        let salida = " if "
        return node.exprs.map((node) => node.accept(this)).join('\n');
    }
    visitUnion(node) {//recorrer toda la lista 
        return node.exprs.map((node) => node.accept(this)).join('\n');
    }
    visitExpresion(node) {
        if (!firstNonTerminal){
            firstNonTerminal = node;
        }
        if (node.expr instanceof n.Identificador) {
            //entonces es un no terminal, debemos de obtenerlo de las producciones
            let noTerminal = noTerminals.find((produccion) => produccion.id === node.expr.id);
            console.log("se sustituyo el no terminal: ", noTerminal.id)
            if (noTerminal === undefined) {
                console.log("no terminal: ", noTerminals)
                throw new Error('No se encontro el no terminal');
            }
            node.expr = noTerminal.expr;
        }
        let salida = ""
        if (!(node.qty ==null || node.qty == undefined || node.qty == '')) {
            if (node.qty == '*') {
                salida =`
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 1
                do while (loopStack(loopStackPosition) >= 1)
                    loopStack(loopStackPosition) = 0
                    ${node.expr.accept(this)}
                end do
                loopStackPosition = loopStackPosition - 1
                valido = .true.`
            } else if (node.qty == '+') {
                salida = `
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 1
                do while (loopStack(loopStackPosition) >= 1)
                    loopStack(loopStackPosition) = 0
                    ${node.expr.accept(this)}
                    if (loopStack(loopStackPosition) == 0) then
                        print *, "error, la expresion no cumple con '+' ", cursor, ', "'//input(cursor:cursor)//'"'
                        lexeme = "ERROR"
                        return
                    end if
                end do`
            } else if (node.qty == '?') {
                salida = `
                loopStackPosition += 1
                loopStack(loopStackPosition) = 0
                do while (loopStack(loopStackPosition) == 1 .and. loopStack(loopStackPosition) == 0)
                    ${node.expr.accept(this)}
                    if (loopStack(loopStackPosition) >= 2) then
                        print *, "error, la expresion no cumple con '?' ", cursor, ', "'//input(cursor:cursor)//'"'
                        lexeme = "ERROR"
                        return
                    end if
                end do`
            } else {
                throw new Error('signo de cantidad invalido');
            }
        } else {
            salida = node.expr.accept(this);
        }
        return salida;
    }
    visitString(node) {
        let salida = ''
        if (node.isCase !== undefined) {
            salida = `  if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then`;
        } else {
            salida = `  if ("${node.val.toLowerCase()}" == to_lowercase(input(cursor:cursor + ${node.val.length - 1}))) then`;
        }
        salida +=`
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        end if`
        return salida;
    ;
    }

    generateCaracteres(chars) {
        if (chars.length == 0) return '';
        return `
    if (findloc([${chars
        .map((char) => `"${char}"`)
        .join(', ')}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }

    visitCorchetes(node) {
        // console.log(node)
        return `
        i = cursor
        ${this.generateCaracteres(
            node.chars.filter((node) => typeof node === 'string')
        )}
        ${node.chars
            .filter((node) => node instanceof n.Rango)
            .map((range) => range.accept(this))
            .join('\n')}
            `;
    }

    visitRango(node) {
        return `
        if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
            lexeme = input(cursor:i)
            cursor = i + 1
            return
        end if
            `;
    }

	visitIdentificador(node) {
        let salida =`
        if ("${node.id}" == input(cursor:cursor + ${node.id.length - 1})) then
            allocate( character(len=${node.id.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.id.length - 1})
            cursor = cursor + ${node.id.length}
        end if`;
    return salida;
    ;
    }
	visitParentesis(node) {
        return node.contenido.accept(this);
    }
	visitPunto(node) {

    }
	visitNegacionPunto(node) {

    }
	visitConteoSimple(node) {

    }
	visitConteoRango(node) {

    }
	visitConteoOpciones(node) {

    }
	visitConteoRangoOpciones(node) {

    }
}
