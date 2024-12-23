import Visitor from './Visitor.js';
import * as n from '../visitor/CST.js';
let noTerminals = [];
let caseInsensitive= false;
let firstNonTerminal = false;
let inBucle = [];
let zeroOrMore = false;
let OneOrMore=false;
let ZeroOrOne = false;

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        //a los no terminales le asignamos las grammar que son las producciones
        noTerminals = grammar;
        // console.log("grammar en generateTokenizer: ",grammar)
        let salida = `
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
            tokens = tokens // "," // tokensTemp
        end do
    end function getTokens
    
end module module_Queue
        
module parser
    use module_Queue
    implicit none

        public :: parse

    contains

    subroutine parse(input)

        character(len=*), intent(in) :: input
        character(len=:), allocatable :: salida
        integer :: cursor
        
        cursor = 1

        salida = nextSym(input, cursor)
        print *, salida
    end subroutine parse

    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        type(Queue) :: cola
        logical:: valido
        valido = .true.

        if (cursor > len(input)) then
            allocate(character(len=3) :: lexeme)
            lexeme = "EOF"
            return
        end if
            `
            if(!(grammar[0].alias == null || grammar[0].alias == undefined || grammar[0].alias == '')){
                //guardar el id y el alias
                salida += `
                valido = ${grammar[0].alias}(input, cursor, cola)`
            } else{
                //guardar id id 
                salida += `
                valido = ${grammar[0].id}(input, cursor, cola)`
            }
            salida +=`

        if (cola%isEmpty()) then
            allocate(character(len=5) :: lexeme)
            print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
            lexeme = "ERROR"
        else 
            
            lexeme = cola%getTokens()
            lexeme = lexeme//",EOF"
        end if
    end function nextSym`
        console.log("expresionesss: ",grammar)
        for (let i = 0; i < grammar.length; i++) {
            console.log("alias")
            console.log(grammar.alia)
            if(!(grammar[i].alias == null || grammar[i].alias == undefined || grammar[i].alias.trim() == '')){
                //guardar el id y el alias
                salida += `
    function ${grammar[i].alias}(input, cursor, cola) result (valido)`
            } else{
                //guardar id id 
                salida +=`
    function ${grammar[i].id}(input, cursor, cola) result (valido)`
            }
            salida +=`
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        type(Queue):: colatemp
        character(len=5) :: error = "ERROR"

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal, i
        logical:: valido
        valido = .true.
            `
            salida += grammar[i].accept(this);
            salida += `
    end function`
        }
        salida += `
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

end module parser `
        return salida;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }

    visitOpciones(node) {
        //("op1"/"op2"/"op3")
        let salida = `

        if (cursor > len(input)) then
            valido = .false.
            return
        end if

        if (valido) then
            localFirstOptionalFounded = localFirstOptionalFounded + 1
            firstOptionalFounded(localFirstOptionalFounded) = .false.`
            for (let i = 0; i < node.exprs.length; i++) {
                salida += node.exprs[i].exprs[0].accept(this); //solo analizamos la primera expresion de la i opcion
                salida += `
                if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                    firstOptionalFounded(localFirstOptionalFounded) = .true.
                    !pondremos un do para absorver un posible return de error y sea un exit
                    `;
                for (let j = 1; j < node.exprs[i].exprs.length; j++) {
                    salida += node.exprs[i].exprs[j].accept(this);
                }
                salida += `
                end if
                valido = .true.`;
            }
            salida += `
            if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
                valido = .false.
            end if
            firstOptionalFounded(localFirstOptionalFounded) = .false.
            localFirstOptionalFounded = localFirstOptionalFounded - 1
        end if
        
        `
            return salida;
    }
    // Completado
    visitUnion(node) {
        let salida = '';
        
        for (let index = 0; index < node.exprs.length; index++) {
            //como union solo puede almacenar de tipo expresion
            salida +=node.exprs[index].accept()
        }
        return salida;
    }



    visitExpresion(node) {
        let salida = ""
        if (!(node.qty ==null || node.qty == undefined || node.qty == '')) {
            if (node.qty == '*') {
                salida =`
                if (cursor > len(input)) then
                    valido = .false.
                    return
                end if

                if (valido) then
                    loopStackPosition = loopStackPosition + 1
                    loopStack(loopStackPosition) = 1
                    matchStack(loopStackPosition) = 1
                    do while (matchStack(loopStackPosition) >= 1 .and. valido)
                        puntero => cola%tail  
                        cursorTemporal = cursor
                        matchStack(loopStackPosition) = 0
                        !!
                        ${node.expr.accept(this)}
                        !!
                    end do
                    loopStackPosition = loopStackPosition - 1
                    if (matchStack(loopStackPosition) >= 1 .and. .not. valido) then
                        cola%tail => puntero
                        cola%tail%next => null() 
                        cursor = cursorTemporal
                    end if
                    valido = .true.
                end if`
            } else if (node.qty == '+') {
                salida = `
                if (cursor > len(input)) then
                    valido = .false.
                    return
                end if
                if (valido) then
                    loopStackPosition = loopStackPosition + 1
                    loopStack(loopStackPosition) = 0
                    matchStack(loopStackPosition) = 0
                    do while (.true.)
                        puntero => cola%tail  
                        cursorTemporal = cursor
                        matchStack(loopStackPosition) = 0
                        !!
                        ${node.expr.accept(this)}
                        !!
                        if (matchStack(loopStackPosition) == 0 .and. loopStack(loopStackPosition) == 0) then
                            !print *, "error, la expresion no cumple con '+' ", cursor, ', "',input(cursor:cursor),'"'
                            !call cola%enqueue(error//","//input(cursor:cursor))
                            valido = .false.
                        end if
                        if (matchStack(loopStackPosition) == 0 .and. loopStack(loopStackPosition) /= 0) then
                            valido = .true.
                            exit
                        end if
                        loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
                    end do
                    matchStack(loopStackPosition) = 0
                    loopStackPosition = loopStackPosition - 1
                    if (valido .eqv. .false.) then
                        cola%tail => puntero
                        cola%tail%next => null() 
                        cursor = cursorTemporal
                    end if
                end if`
            } else if (node.qty == '?') {
                salida = `
            if (cursor > len(input)) then
                valido = .false.
                return
            end if
            if (valido) then
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 0
                matchStack(loopStackPosition) = 0
                valido = .true.
                do while (.true.)
                    puntero => cola%tail  
                    cursorTemporal = cursor
                    matchStack(loopStackPosition) = 0
                    !!
                    ${node.expr.accept(this)}
                    !!
                    valido = .true.
                    exit
                end do
                if (.not. valido .and. matchStack(loopStackPosition) /= 0) then
                    cola%tail => puntero
                    cola%tail%next => null() 
                    cursor = cursorTemporal
                end if
                matchStack(loopStackPosition) = 0
                loopStackPosition = loopStackPosition - 1
            end if`
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
        let salida = `
        if (cursor > len(input)) then
            valido = .false.
            return
        end if
        if (valido) then`;
        if (node.isCase !== undefined) {
            // Compara case sensitive
            salida += `
            if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then`;
        } else {
            // Si es case insensiive se disminuye a minus
            salida += `
            if ("${node.val.toLowerCase()}" == to_lowercase(input(cursor:cursor + ${node.val.length - 1}))) then`;
        }
        
        salida += `
                call cola%enqueue(input(cursor:cursor + ${node.val.length - 1}))
                cursor = cursor + ${node.val.length}
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                valido = .true.
            else
                !print *, "error, la cadena no coincide", cursor, ', "',input(cursor:cursor + ${node.val.length - 1}),'", "',"${node.val}",'"'
                valido = .false.
            end if
        end if`;
        return salida;
    };

    // Completado
    generateCaracteres(chars) {
        console.log("generate character-lista de caracteres que llegan: ")
        console.log(chars)
        if (chars.length == 0) return '';
        return `
        if (cursor > len(input)) then
            valido = .false.
            return
        end if
        if (valido) then
            if (findloc([${chars
                .map((char) => `${char}`)
                .join(', ')}], input(i:i), 1) > 0) then
                call cola%enqueue(input(cursor:i))
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                cursor = i + 1
                valido = .true.
            end if
        end if`;
    }
    
// Completado
    visitCorchetes(node) {
        if (node.isCase !== undefined){
            caseInsensitive= false
        } else{
            caseInsensitive= true
        }
        let salida = `
        i = cursor
        ${this.generateCaracteres(
            node.chars.filter((node) => typeof node === 'string')
        )}
        ${node.chars
            .filter((node) => node instanceof n.Rango)
            .map((range) => range.accept(this))
            .join('\n')}`
        return salida;
    }

    // visitRango(node) {
    //     return `
    //     if (cursor > len(input)) then
    //         valido = .false. 
    //         print *, "error cursor en visit rango", cursor
    //         return 
    //     end if
    //     if (valido) then
    //     `if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then`} :
    //             call cola%enqueue(input(cursor:i))
    //             matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
    //             cursor = i + 1
    //             valido = .true.
    //         end if
    //     end if`;
    // }
    visitRango(node) {
        return `
        if (cursor > len(input)) then
            valido = .false.
            print *, "error cursor en visit rango", cursor
            return 
        end if
        if (valido) then
            ${caseInsensitive ? 
                `if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then` :
                `if (to_lowercase(input(i:i)) >= "${node.bottom.toLowerCase()}" .and. to_lowercase(input(i:i)) <= "${node.top.toLowerCase()}") then`
            }
                call cola%enqueue(input(cursor:i))
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                cursor = i + 1
                valido = .true.
            end if
        end if`;
    }




	visitIdentificador(node) {
        // Se busca en noTerminals si existe una producción con ese id
        const produccion = noTerminals.find(p => p.id === node.id);
            let salida = `
                        !!aqui hay un identificador con alias
        if (cursor > len(input)) then
            valido = .false.
            return
        end if
        if (valido) then`
        if (produccion.alias) {
            salida += `
            valido = ${produccion.alias}(input, cursor, cola)`
            } else {
                salida += `
            valido = ${node.id}(input, cursor, cola)`;
        }
        salida += `
        end if`
        return salida;
    }
	visitParentesis(node) {
        return node.contenido.accept(this);
    }
	visitPunto(node) {
       return `
       if(valido)then
       print*,"llega a probar final del input"
        if (cursor > len(input)) then
                call cola%enqueue("EOF")
                valido = .false.
                return
            end if
        end if` 
        
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
