/** @typedef {import('./visitor/Visitor.js').Enqueues} Enqueues */
/** @typedef {import('./visitor/Visitor.js').ActionTypes} ActionTypes */
import * as CST from './visitor/CST.js';


/**
 *
 * @param {{
 *  beforeContains: string
 *  afterContains: string
 *  startingRuleId: string;
 *  startingRuleType: string;
 *  rules: string[];
 *  actions: string[];
 *  enqueuesList: Enqueues;
 *  parentFunctions: string[];
 * }} data
 * @returns {string}
 */
export const main = (data) => `
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input , expected
    integer, private :: savePoint, lexemeStart, cursor

    interface toStr
        module procedure intToStr
        module procedure strToStr
        module procedure intListToStr
        module procedure strListToStr
    end interface
    
    interface peg_push
        module procedure add_to_list_alloc_character
        ${Object.values(data.enqueuesList).map(item => `module procedure ${item.nameFunction}`).join('\n')}
    end interface
    
    ${data.beforeContains}

    contains
    
    ${data.afterContains}

    function parse(str) result(res)
        character(len=:), allocatable :: str
        logical :: accept
        ${data.startingRuleType} :: res

        input = str
        cursor = 1
        ${(() => {
            if (data.startingRuleType.toLowerCase().includes("pointer")) {
                return `res => ${data.startingRuleId}(accept)`;
            } else {
                return `res = ${data.startingRuleId}(accept)`;
            }
        })()}
        if (accept) then
            print *, "La cadena es valida"
        else
            call pegError()
        end if
    end function parse

    ${data.rules.join('\n')}

    ${data.parentFunctions.join('\n')}

    ${data.actions.join('\n')}

    ${generateEnqueues(data.enqueuesList)}

subroutine add_to_list_alloc_character(list, value)
    character(len=:), allocatable, intent(inout) :: list(:)
    character(len=:), allocatable, intent(in) :: value
    integer :: current_size
    character(len=:), allocatable :: temp(:)

    ! Determinar el size actual de la lista
    if (.not. allocated(list)) then
        current_size = 0
    else
        current_size = size(list)
    end if

    ! Si la lista tiene elementos, copiar su contenido
    if (current_size > 0) then
        allocate(temp(current_size), mold=list)
        temp = list
        deallocate(list)
    end if

    ! Redimensionar la lista e insertar el nuevo valor
    allocate(list(current_size + 1), mold=value)
    if (current_size > 0) list(1:current_size) = temp
    list(current_size + 1) = value

    ! Liberar la memoria temporal
    if (allocated(temp)) deallocate(temp)
end subroutine add_to_list_alloc_character


function acceptString(str, caseInsensitive) result(accept)
    character(len=*) :: str
    logical :: caseInsensitive
    logical :: accept
    integer :: offset
    character(len=:), allocatable :: input_fragment

    offset = len(str) - 1
    if (cursor + offset > len(input)) then
        accept = .false.
        expected = str
        return
    end if

    if (caseInsensitive) then
        ! Case insensitive comparison
        input_fragment = lowercase(input(cursor:cursor + offset))
        if (lowercase(str) /= input_fragment) then
            accept = .false.
            expected = str
            return
        end if
    else
        ! Case sensitive comparison
        input_fragment = input(cursor:cursor + offset)
        if (str /= input_fragment) then
            accept = .false.
            expected = str
            return
        end if
    end if

    cursor = cursor + len(str)
    accept = .true.
end function acceptString

function acceptRange(bottom, top, caseInsensitive) result(accept)
    character(len=1) :: bottom, top
    logical :: caseInsensitive
    logical :: accept
    character(len=1) :: currentChar

    if (cursor > len(input)) then
        accept = .false.
        return
    end if

    currentChar = input(cursor:cursor)
    
    if (caseInsensitive) then
        ! Convertir todo a minúsculas para comparación
        if (.not. (lowercase(currentChar) >= lowercase(bottom) .and. lowercase(currentChar) <= lowercase(top))) then
            accept = .false.
            return
        end if
    else
        ! Comparación case-sensitive
        if (.not. (currentChar >= bottom .and. currentChar <= top)) then
            accept = .false.
            return
        end if
    end if
    
    cursor = cursor + 1
    accept = .true.
end function acceptRange

function acceptSet(set, caseInsensitive) result(accept)
    character(len=1), dimension(:) :: set
    logical :: caseInsensitive
    logical :: accept
    character(len=1) :: currentChar
    integer :: i

    if (cursor > len(input)) then
        accept = .false.
        return
    end if

    currentChar = input(cursor:cursor)

    if (caseInsensitive) then
        ! Comparación insensible a mayúsculas/minúsculas
        do i = 1, size(set)
            if (lowercase(set(i)) == lowercase(currentChar)) then
                cursor = cursor + 1
                accept = .true.
                return
            end if
        end do
    else
        ! Comparación sensible a mayúsculas/minúsculas
        do i = 1, size(set)
            if (set(i) == currentChar) then
                cursor = cursor + 1
                accept = .true.
                return
            end if
        end do
    end if

    accept = .false.
end function acceptSet



    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            return
        end if
        accept = .true.
    end function acceptEOF

        function lowercase(str) result(lowered)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lowered
        integer :: i

        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lowered(i:i) = achar(iachar(str(i:i)) + 32)
            else
                lowered(i:i) = str(i:i)
            end if
        end do
    end function lowercase
    
    
    function consumeInput() result(substr)
        character(len=:), allocatable :: substr

        substr = input(lexemeStart:cursor - 1)
    end function consumeInput

    subroutine pegError()
        print '(A,I0,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

        call exit(1)
    end subroutine pegError

    function intToStr(int) result(cast)
        integer :: int
        character(len=31) :: tmp
        character(len=:), allocatable :: cast

        write(tmp, '(I0)') int
        cast = trim(adjustl(tmp))
    end function intToStr

    function intListToStr(intList) result(cast)
        integer, intent(in) :: intList(:)
        character(len=:), allocatable :: cast
        integer :: i
        character(len=31) :: tmp

        cast = ""
        do i = 1, size(intList)
            write(tmp, '(I0)') intList(i)
            cast = trim(cast) // trim(adjustl(tmp))
        end do
    end function intListToStr

    function strToStr(str) result(cast)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: cast

        cast = str
    end function strToStr

    function strListToStr(strList) result(cast)
        character(len=*), intent(in) :: strList(:)
        character(len=:), allocatable :: cast
        integer :: i

        cast = ""
        do i = 1, size(strList)
            cast = trim(cast) // trim(strList(i))
        end do
    end function strListToStr
end module parser
`;

/**
 *
 * @param {{
 *  id: string;
 *  returnType: string;
 *  exprDeclarations: string[];
 *  expr: string;
 * }} data
 * @returns
 */
export const rule = (data) => `
    function peg_${data.id}(accept) result (res)
        ${data.returnType} :: res
        ${data.exprDeclarations.join('\n')}
        integer :: i
        logical, intent(out) :: accept

        accept=.false.
        savePoint = cursor
        ${data.expr}
        accept = .true.
    end function peg_${data.id}
`;

/**
 *
 * @param {{
 *  exprs: string[]
 * }} data
 * @returns
 */
export const election = (data) => `
        do i = 0, ${data.exprs.length}
            select case(i)
            ${data.exprs
                .map(
                    (expr, i) => `
            case(${i})
                cursor = savePoint
                ${expr}
                exit
            `
                )
                .join('\n')}
            case default
            accept=.false.
            return
            end select
        end do
`;


/**
 *
 * @param {{
 *  exprs: string[]
 *  startingRule: boolean
 *  resultExpr: string
 * }} data
 * @returns
 */
export const union = (data) => `
                ${data.exprs.join('\n')}
                ${data.startingRule ? 'if (.not. acceptEOF()) cycle' : ''}
                ${data.resultExpr}
`;
/**
 *
 * @param {{
*  qty: (string|CST.ConteoRango|CST.ConteoSimple|CST.ConteoOpciones|CST.ConteoRangoOpciones);
*  callRule: string
*  pila: string
* }} data
* @returns
*/

export const strQtyIdent= (data) => {
    const qty = data.qty;
    const condition = data.callRule;
    const addInPila = data.pila;
    if (qty === '+' || (qty instanceof CST.ConteoRango && (qty.inicio === "1") && (qty.fin === null))) {
        return `
        ${condition}
        if (.not. (accept)) then
            cycle
        end if
        ${addInPila}
        do while (.not. cursor > len(input))
            ${condition}
            if (.not. (accept)) then
                exit
            end if
            ${addInPila}
        end do
        `
    } else if (qty === '*'|| (qty instanceof CST.ConteoRango && (qty.inicio === "0" || qty.inicio === null) && (qty.fin === null))) {
        return `do while (.true.)
            ${condition}
            if (.not. (accept)) then
                exit
            end if
            ${addInPila}
        end do`;
    } else if (qty === '?'|| (qty instanceof CST.ConteoRango && (qty.inicio === null) && (qty.fin === "1"))) {
        return `
        ${condition}
        if ((accept)) then
            ${addInPila}
        end if
        `;
    } else if (qty instanceof CST.ConteoSimple) {
        return `
        j = 0
        do while (.not. cursor > len(input) .and. j < ${qty.val})
            ${condition}
            if (.not. (accept)) then
                exit
            end if
            ${addInPila}
            j = j + 1
        end do
        if ( j /= ${qty.val} ) then
            cycle
        end if 
        `
    } else if (qty instanceof CST.ConteoRango) {
        return `
        j = 0
        do while (.not. cursor > len(input) .and. j < ${qty.fin})
            ${condition}    
            if (.not. (accept)) then
                exit
            end if
            j = j + 1
            ${addInPila}
        end do
        if ( j < ${qty.inicio}) then
            cycle
        end if 
        `
    }  else if (qty instanceof CST.ConteoOpciones){
        let delimiter = qty.opciones.map((opcion) => opcion.accept(this)).join(' .or. '); //posiblemente esto se deba de arreglar
        return `
        j = 0
        ${condition}
        if (.not. (accept)) then
            cycle
        end if
        ${addInPila}
        j = j + 1
        do while (.not. cursor > len(input) .and. j < ${qty.val})
            if (.not. (${delimiter})) then
                exit
            end if
            ${condition}
            if (.not. (accept)) then
                exit
            end if
            ${addInPila}
            j = j + 1
        end do
        if ( j /= ${qty.val} ) then
            cycle
        end if 
        `
    } else if (qty instanceof CST.ConteoRangoOpciones){
        let delimiter = qty.opciones.map((opcion) => opcion.accept(this)).join(' .or. ');
        return `
        j = 0
        ${condition}
        if (.not. (accept)) then
            cycle
        end if
        j = j + 1
        do while (.not. cursor > len(input) .and. j < ${qty.fin})
            if (.not. (${delimiter})) then
                exit
            end if
            ${condition}
            if (.not. (accept)) then
                exit
            end if
            ${addInPila}
            j = j + 1
        end do
        if ( j < ${qty.inicio}) then
            cycle
        end if 
        `
    }
}

/**
 *
 * @param {{
 *  expr: string;
 *  destination: string
 *  quantifier?: string;
 * }} data
 * @returns
 */
export const strExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) cycle
                ${data.destination} = consumeInput()
        `;
    }
    switch (data.quantifier) {
        case '+':
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) cycle
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case "*":
            return `
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput() 
            `;
        case "?":
            return `
                lexemeStart = cursor
                if ( ${data.expr}) then
                    ${data.destination} = consumeInput()
                    !pushear?
                end if
            `;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
};


/**
 *
 * @param {{
*  expr: string;
*  destination: string;
* }} data
* @returns
*/

export const ParentExpr = (data) => {
    return  `
                ${data.destination}= ${data.expr}
                if(.not. accept) cycle
    `

}

/**
 *
 * @param {{
*  callRule: string;
* }} data
* @returns
*/

export const NoTerminalAssertion = (data) => {

    return `
    ${data.callRule}
    if (.not. accept) cycle
    `;
}


/**
 *
 * @param {{
*  expr: string;
* }} data
* @returns
*/

export const TerminalAssertion = (data) => {
    return `
    if (.not. ${data.expr}) cycle
    `;
}
/**
 *
 * @param {{
*  callRule: string;
* }} data
* @returns
*/
export const NoTerminalNegAssertion = (data) => {

    return `
    ${data.callRule}
    if ( accept) cycle
    `;
}

/**
 *
 * @param {{
*  expr: string;
* }} data
* @returns
*/
export const TerminalNegAssertion = (data) => {
    return `
    if (${data.expr}) cycle
    `;
}
/**
 *
 * @param {{
 *  exprs: string[];
 * }} data
 * @returns
 */
export const strResultExpr = (data) => `
                res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')}
`;

/**
 *
 * @param {{
 *  fnId: string;
 *  exprs: string[];
 * }} data
 * @param {ActionTypes} actionReturnTypes
 * @returns
 */
export const fnResultExpr = (data,actionReturnTypes) => {
    if (actionReturnTypes[data.fnId].isPointer) {
        return `
                res => ${data.fnId}(${data.exprs.join(', ')})
        `;
    } else {
        return `
                res = ${data.fnId}(${data.exprs.join(', ')})
        `;
    }
};

/**
 *
 * @param {{
*  id: string;
*  returnType: string;
*  exprDeclarations: string[];
*  expr: string;
* }} data
* @returns
*/
export const Parentesis = (data) => `
    function peg_P${data.id}(accept) result (res)
        ${data.returnType} :: res
        ${data.exprDeclarations.join('\n')}
        integer :: i
        logical, intent(out) :: accept

        accept=.false.
        savePoint = cursor
        ${data.expr}
        res="Parentesis"
        accept = .true.
    end function peg_P${data.id}
`;
/**
 *
 * @param {{
 *  ruleId: string;
 *  choice: number
 *  signature: string[];
 *  returnType: string;
 *  paramDeclarations: string[];
 *  code: string;
 * }} data
 * @returns
 */
export const action = (data) => {
    const signature = data.signature.join(', ');
    return `
    function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
        ${data.paramDeclarations.join('\n')}
        ${data.returnType} :: res
        ${data.code}
    end function peg_${data.ruleId}_f${data.choice}
    `;
};

/**
 * @param {Enqueues} value
 * @returns
 */
export const generateEnqueues = (value) => {
    //ahora que tenemos todos los tipos vamos a crear la funcion add_to_list
    const salida = Object.values(value).map(data => `
    subroutine ${data.nameFunction}(list, value)
        ${data.Declaration.replace(":","*")}, allocatable, intent(inout) :: list(:)
        ${data.Declaration.replace(":","*")}, intent(in) :: value
        integer :: current_size
        ${data.Declaration}, allocatable :: temp(:)

        ! Determinar el size actual de la lista
        if (.not. allocated(list)) then
            current_size = 0
        else
            current_size = size(list)
        end if

        ! Si la lista tiene elementos, copiar su contenido
        if (current_size > 0) then
            ${data.Declaration.includes("character") ? "allocate(temp, source=list)" : "allocate(temp(current_size))"}
            temp = list
            deallocate(list)
        end if

        ! Redimensionar la lista e insertar el nuevo valor
        allocate(list(current_size + 1))
        if (current_size > 0) list(1:current_size) = temp
        list(current_size + 1) = value

        ! Liberar la memoria temporal
        if (allocated(temp)) deallocate(temp)
    end subroutine ${data.nameFunction}
    `
    ).join('\n');
    return salida;
}
