import Visitor from './Visitor.js';
import * as n from '../visitor/CST.js';
let noTerminals = [];
let firstNonTerminal = false;
let inBucle = false;
let zeroOrMore = false;
let OneOrMore=false;
let ZeroOrOne = false;

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        //a los no terminales le asignamos las grammar que son las producciones
        noTerminals = grammar;
        // console.log("grammar en generateTokenizer: ",grammar)
        let texto = `
module module_Queue
    implicit none
    type :: node
        character(:), allocatable :: token
        type(node), pointer :: next => null()
    end type node

    type :: Queue
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
        contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: isEmpty
        procedure :: getTokens
    end type Queue

    contains
  
    subroutine enqueue(this, token)
        class(Queue), intent(inout) :: this
        character(*), intent(in) :: token
        type(node), pointer :: newNode
        allocate(newNode)
        newNode%token = token
        newNode%next => null()
        if (associated(this%head)) then
            this%tail%next => newNode
            this%tail => newNode
        else
            this%head => newNode
            this%tail => newNode
        end if
    end subroutine enqueue

    function dequeue(this) result(token)
        class(Queue), intent(inout) :: this
        type(node), pointer :: temp
        character(:), allocatable :: token
        if (associated(this%head)) then
            token = this%head%token
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        else
            token = "ERROR"
        end if
    end function dequeue

    function isEmpty(this) result(isEmptyResult)
        class(Queue), intent(in) :: this
        logical :: isEmptyResult
        isEmptyResult = .not. associated(this%head)
    end function isEmpty

    function getTokens(this) result(tokens)
        class(Queue), intent(in) :: this
        character(:), allocatable :: tokens, tokensTemp
        type(node), pointer :: current
    
        ! Verificar si la pila está vacía
        if (.not. associated(this%head)) then
            tokens = "ERROR, no tokens found"
            return
        end if
    
        ! Inicializar tokens con el primer nodo
        current => this%head
        allocate(character(len=len(current%token)) :: tokens)
        tokens = current%token
    
        ! Procesar los nodos siguientes
        do
            current => current%next
            if (.not. associated(current)) then
                exit
            end if
    
            tokensTemp = current%token
    
            ! Redimensionar tokens para concatenar el siguiente token
            tokens = tokens // "\n" // tokensTemp
        end do
    end function getTokens
    
end module module_Queue
        
module tokenizer
    use module_Queue
    implicit none

    contains

    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        type(Queue) :: LocalStack
        logical:: valido
        valido = .false.

        if (cursor > len(input)) then
            allocate(character(len=3) :: lexeme)
            lexeme = "EOF"
            return
        end if

        !!este es un id
        valido = ${grammar[0].id}(input, cursor, LocalStack)

        if (LocalStack%isEmpty() .or. .not. valido) then
            allocate(character(len=5) :: lexeme)
            print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
            lexeme = "ERROR"
        else 
            lexeme = LocalStack%getTokens()
        end if
    end function nextSym
`
        
        for (let i = 0; i < grammar.length; i++) {
            texto +=`
            function ${grammar[i].id}(input, cursor, cola) result (valido)
            character(*),intent(in) :: input
            integer, intent(inout) :: cursor
            type(Queue), intent(inout) :: cola
            character(len=5) :: error = "ERROR"
            integer, dimension(20) :: matchStack
            integer, dimension(20) :: loopStack
            integer :: loopStackPosition = 1
            type(node), pointer:: puntero
            integer :: cursorTemporal
            logical:: valido
            valido = .false.
            `
            texto += grammar[i].accept(this);
            texto += `
            end function ${grammar[i].id}
            `
        }
`
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
    // if (input(cursor:cursor) == '+') then
    //             call cola%enqueue(input(cursor:cursor))
    //             cursor = cursor + 1
    //             valido = .true.
    //         else if (input(cursor:cursor) == '-') then
    //             call cola%enqueue(input(cursor:cursor))
    //             cursor = cursor + 1
    //             valido = .true.
    //         else
    //             valido = .false.
    //             exit
    //         end if 

    /*
        opciones = terminal notermial if(terminal)
                /noterminal dsafas fasd         else if(noterminal(input,,cursor,cola))
                /suma else if(suma(input, cursor, cola)) 
        suma = 8 + 5
    */ 
    visitOpciones(node) {
        
        let salida 
        for (let index = 0; index < node.exprs.length; index++) {
            if (index==0) {
                salida = `
                if ${node.exprs[index].accept(this)}` 
                salida+= `
                `            
                if(node.exprs.length==1){
                    salida +=`
                end if`
                }
                continue
            }
            salida+= `
            else if ${node.exprs[index].accept(this)}
            `
        }
        salida+= `
        else
            valido = .false.
        `
        if (inBucle){
            salida+=`exit`
        }
        salida+= `end if`
        return salida;//
    }
    // Completado
    visitUnion(node) {
        let salida = '';
        for (let index = 0; index < node.exprs.length; index++) {
            if (index == 0) {
                salida = `
                (${node.exprs[index].accept(this)}) then
                    valido = .true.
                    return
                end if
             !!/////////////////// No estamos seguro si funciona para ciclos/falta la validacion para el ciclo jaja /////////////////////////
        `}
        }
        salida += `
            else
                valido = .false.
        `;
        if (inBucle) {
            salida += `
                exit
            `;
        }
        salida += `
            end if
        `;
        return salida;
    }
    visitExpresion(node) {
        ///IMPORTANTE no lo logro resolver
        //puede venir identificador, parentesis o no terminal junto a cuantificadores xd
        //de opciones viene: if-else if, de Union viene ($condicion), buscando la condicion, pero en expresiones 
        // puede venir *,+,? por lo que poner codigo de un loop es problematico xd lo que podria salir: if(loopStackPosition = loopStackPosition + 1 )

        // if (!firstNonTerminal){
        //     firstNonTerminal = node;
        // }
        // if (node.expr instanceof n.Identificador) {
        //     //entonces es un no terminal, debemos de obtenerlo de las producciones
        //     let noTerminal = noTerminals.find((produccion) => produccion.id === node.expr.id);
        //     console.log("se sustituyo el no terminal: ", noTerminal.id)
        //     if (noTerminal === undefined) {
        //         console.log("no terminal: ", noTerminals)
        //         throw new Error('No se encontro el no terminal');
        //     }
        //     node.expr = noTerminal.expr;
        // }
        let salida = ""
        if (!(node.qty ==null || node.qty == undefined || node.qty == '')) {
            if (node.qty == '*') {
                inBucle = true;
                salida =`${node.expr.accept}`

                inBucle = false
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

    // Completado
    visitString(node) {
        let salida = '';
        if (node.isCase !== undefined) {
            salida = `  if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then`;
        } else {
            salida = `  if ("${node.val.toLowerCase()}" == to_lowercase(input(cursor:cursor + ${node.val.length - 1}))) then`;
        }
        salida += `
            allocate(character(len=${node.val.length}) :: lexeme)
            call cola%enqueue(input(cursor:cursor + ${node.val.length - 1}))
            lexeme = input(cursor:cursor + ${node.val.length - 1})
            cursor = cursor + ${node.val.length}`;
        
        if (inBucle) {
            salida += `
                valido = .false.
                exit`;
        } else {
            salida += `
                valido = .false.
                return`;
        }
    
        salida += `
            else
                cursor = cursor + 1
            end if`;
    
        return salida;
    };
    

    /* visitString(node) {
    let salida = '';
    if (node.isCase !== undefined) {
        salida = `if (input(cursor:cursor + ${node.val.length - 1}) == "${node.val}") then`;
    } else {
        salida = `if (to_lowercase(input(cursor:cursor + ${node.val.length - 1})) == "${node.val.toLowerCase()}") then`;
    }
    salida += `
    allocate(character(len=${node.val.length}) :: lexeme)
    call cola%enqueue(input(cursor:cursor + ${node.val.length - 1}))
    lexeme = input(cursor:cursor + ${node.val.length - 1})
    cursor = cursor + ${node.val.length}
    valido = .true.`;
    if (inBucle) {
        salida += `
        exit`;
    } else {
        salida += `
        return`;
    }
    salida += `
    end if`;
    return salida;
} */





    // Completado
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
    
    /*
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

*/

// Completado
    visitCorchetes(node) {
        let salida = `
        i = cursor
        ${this.generateCaracteres(
            node.chars.filter((node) => typeof node === 'string')
        )}
        ${node.chars
            .filter((node) => node instanceof n.Rango)
            .map((range) => range.accept(this))
            .join('\n')}
        valido = .true.`;
        if (inBucle) {
            salida += `
            exit`;
        } else {
            salida += `
            return`;
        }
        return salida;
    }
    /*
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
*/

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
