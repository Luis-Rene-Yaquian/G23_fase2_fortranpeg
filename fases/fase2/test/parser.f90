



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

contains

subroutine parse(input)
    character(len=:), intent(inout), allocatable :: input
    character(len=:), allocatable :: lexeme
    integer :: cursor
    type(Queue) :: LocalStack
    cursor = 1
    do while (lexeme /= "EOF" )
        if(lexeme == "ERROR") THEN 
            cursor = cursor + 1
            lexeme = nextSym(input, cursor,LocalStack)
        else 
            lexeme = nextSym(input, cursor, LocalStack)
            
        end if
        print *, lexeme
    end do
    print*,"////IMPRESION DE LA COLAD/////"
    lexeme= LocalStack%getTokens()
    print *, lexeme
end subroutine parse

function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
end function tolower

function replace_special_characters(input_string) result(output_string)
    implicit none
    character(len=:), allocatable, intent(in) :: input_string
    character(len=:), allocatable :: temp_string
    character(len=:), allocatable :: output_string
    integer :: i, length

    temp_string = ""
    length = len(input_string)

    do i = 1, length
        select case (ichar(input_string(i:i)))
        case (10) ! Nueva línea
            temp_string = temp_string // '\n'
        case (9)  ! Tabulación
            temp_string = temp_string // '\t'
        case (13) ! Retorno de carro
            temp_string = temp_string // '\r'
        case (32) ! Espacio
            if (input_string(i:i) == " ") then
                temp_string = temp_string // "_"
            else
                temp_string = temp_string // input_string(i:i)
            end if
        case default
            temp_string = temp_string // input_string(i:i)
        end select
    end do
    allocate(character(len=len(temp_string)) :: output_string)
    output_string = temp_string
end function

function nextSym(input, cursor,cola) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    type(Queue), intent(inout) :: cola
    character(len=:), allocatable :: lexeme
    character(len=:), allocatable :: buffer 
    logical :: concat_failed
    integer :: initialCursor

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    
    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (";" == input(cursor:cursor + 0 ))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "lista"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
     
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. ("-" == input(cursor:cursor + 0 )))
            cursor = cursor + 1
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        else
            cursor = initialCursor
            concat_failed = .true.
            buffer = ""
        end if
        if(len(buffer) > 0) then
            call cola%enqueue(buffer)
        end if
        
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "lista"
        return
    end if
        
    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (";" == input(cursor:cursor + 0 ))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "lista"
        return
    end if
        

















    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. ("Piezas" == input(cursor:cursor + 5 ))) then
            call cola%enqueue(input(cursor:cursor + 5)) 
            buffer = buffer // input(cursor:cursor + 5)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 6
            
        else
            concat_failed = .true.
            buffer = ""
        end if

        if (cursor <= len_trim(input) .and. (tolower(" blancas") == tolower(input(cursor:cursor + 7 )))) then
            call cola%enqueue(input(cursor:cursor + 7)) 
            buffer = buffer // input(cursor:cursor + 7)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 8
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "blancas"
        return
    end if
        
    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (":" == input(cursor:cursor + 0 ))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "blancas"
        return
    end if
        


    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. ("Piezas" == input(cursor:cursor + 5 ))) then
            call cola%enqueue(input(cursor:cursor + 5)) 
            buffer = buffer // input(cursor:cursor + 5)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 6
            
        else
            concat_failed = .true.
            buffer = ""
        end if

        if (cursor <= len_trim(input) .and. (tolower(" negras") == tolower(input(cursor:cursor + 6 )))) then
            call cola%enqueue(input(cursor:cursor + 6)) 
            buffer = buffer // input(cursor:cursor + 6)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 7
            
        else
            concat_failed = .true.
            buffer = ""
        end if

        if (cursor <= len_trim(input) .and. (":" == input(cursor:cursor + 0 ))) then 
            call cola%enqueue(input(cursor:cursor + 0))
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "negras"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (((input(cursor:cursor) == "R"))& 
    .or. ((input(cursor:cursor) == "D"))& 
    .or. ((input(cursor:cursor) == "T")))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "pieza"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (((input(cursor:cursor) == "A"))& 
    .or. ((input(cursor:cursor) == "C"))& 
    .or. ((input(cursor:cursor) == "P")))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "pieza"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (((iachar(input(cursor:cursor)) >= iachar("a") .and. &
        iachar(input(cursor:cursor)) <= iachar("h"))))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if

        if (cursor <= len_trim(input) .and. (((input(cursor:cursor) == "1"))& 
    .or. ((input(cursor:cursor) == "2"))& 
    .or. ((input(cursor:cursor) == "3"))& 
    .or. ((input(cursor:cursor) == "4"))& 
    .or. ((input(cursor:cursor) == "5"))& 
    .or. ((input(cursor:cursor) == "6"))& 
    .or. ((input(cursor:cursor) == "7"))& 
    .or. ((input(cursor:cursor) == "8")))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if

        if (cursor <= len_trim(input) .and. (((input(cursor:cursor) == "+"))& 
    .or. ((input(cursor:cursor) == "#")))) then 
            call cola%enqueue(input(cursor:cursor + 0))
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "casilla"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
     
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. (((input(cursor:cursor) == char(32)))& 
    .or. ((input(cursor:cursor) == char(9)))))
            cursor = cursor + 1
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        else
            cursor = initialCursor
            concat_failed = .true.
            buffer = ""
        end if
        if(len(buffer) > 0) then
            call cola%enqueue(buffer)
        end if
        
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "espacio"
        return
    end if
        

    concat_failed = .false.
    buffer = ""
    
        if (cursor <= len_trim(input) .and. (((input(cursor:cursor) == char(10)))& 
    .or. ((input(cursor:cursor) == char(13))))) then
            call cola%enqueue(input(cursor:cursor + 0)) 
            buffer = buffer // input(cursor:cursor + 0)
            buffer = replace_special_characters(buffer)
            cursor = cursor + 1
            
        else
            concat_failed = .true.
            buffer = ""
        end if
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // "salto"
        return
    end if
        

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module parser 
        