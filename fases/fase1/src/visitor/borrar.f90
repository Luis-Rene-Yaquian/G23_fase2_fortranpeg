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

!!!!!aqui esta lo demas

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
        valido = Expression(input, cursor, LocalStack)

        if (LocalStack%isEmpty() .or. .not. valido) then
            allocate(character(len=5) :: lexeme)
            print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
            lexeme = "ERROR"
        else 
            lexeme = LocalStack%getTokens()
        end if
    end function nextSym

    function Expression(input, cursor, cola) result (valido)
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

        !!estructura de funcion

        valido = term(input, cursor, cola)
        if (.not. valido) then
            return  !| cycle
        end if

        valido = whitespace(input, cursor, cola)
        if (.not. valido) then
            return  !| cycle
        end if

        !! estructura del "*"

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            puntero => cola%tail
            cursorTemporal = cursor
            loopStack(loopStackPosition) = 0
            !!
            if (whitespace(input, cursor, cola) .and. .not. valido) then
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                valido = .true.
            else 
                exit
            end if

            if (input(cursor:cursor) == '+') then
                call cola%enqueue(input(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else if (input(cursor:cursor) == '-') then
                call cola%enqueue(input(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else
                valido = .false.
                exit
            end if 

            if (whitespace(input, cursor, cola)) then
                valido = .true.
            end if

            if (Factor(input, cursor, cola)) then
                valido = .true.
            end if
            !!
            valido = .true.
            
        end do
        loopStackPosition = loopStackPosition - 1
        if (valido .eqv. .false.) then
            cola%tail => puntero
            cola%tail%next => null() 
            cursor = cursorTemporal
        end if
        valido = .true.
        !! fin de la estructura del "*"
        !!estructura de funcion

    end function Expression

    function Term(input, cursor, cola) result (valido)
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

        valido = Factor(input, cursor, cola)
        if (.not. valido .and. loopStackPosition==1 ) then
            call cola%enqueue(error)
            return
        end if

        !! estructura del "*"

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            puntero => cola%tail
            cursorTemporal = cursor
            loopStack(loopStackPosition) = 0
            !!
            if (whitespace(input, cursor, cola)) then
                valido = .true.
            end if

            ! ("*" / "/")

            if (input(cursor:cursor) == '*') then
                call cola%enqueue(input(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else if (input(cursor:cursor) == '/') then
                call cola%enqueue(input(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else
                valido = .false.
            end if 

            if (whitespace(input, cursor, cola)) then
                valido = .true.
            end if

            if (Factor(input, cursor, cola)) then
                valido = .true.
            end if
            !!
            valido = .true.
        end do
        loopStackPosition = loopStackPosition - 1
        if (valido .eqv. .false.) then
            cola%tail => puntero
            cola%tail%next => null() 
            cursor = cursorTemporal
        end if
        !! fin de la estructura del "*"

    end function Term

    function Factor(input, cursor, cola) result (valido)
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

        if (input(cursor:cursor) == '(') then
            call cola%enqueue(input(cursor:cursor))
            cursor = cursor + 1
            valido = .true.
            if (whitespace(input, cursor, cola)) then
                valido = .true.
                if (expression(input, cursor, cola)) then
                    valido = .true.
                    if (whitespace(input, cursor, cola)) then
                        valido = .true.
                        if (input(cursor:cursor) == ')') then
                            call cola%enqueue(input(cursor:cursor))
                            cursor = cursor + 1
                            valido = .true.
                        else
                            valido = .false.
                        end if
                    else
                        valido = .false.
                    end if
                else
                    valido = .false.
                end if
            else
                valido = .false.
            end if
        else if  (whitespace(input, cursor, cola)) then
            if(Integer(input, cursor, cola)) then
                valido = .true.
            else
                valido = .false.
            end if
        else
            valido = .false.
        end if

    end function Factor

    function Integer(input, cursor, cola) result (valido)
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

        !! estructura del "+"
        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 0
        matchStack(loopStackPosition) = 0
        valido = .false.
        do while (.true.)
            puntero => cola%tail  
            cursorTemporal = cursor
            matchStack(loopStackPosition) = 0
            !!
            if (achar(48) <= input(cursor:cursor) .and. input(cursor:cursor) <= achar(57)) then
                call cola%enqueue(input(cursor:cursor))
                cursor = cursor + 1
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                valido = .true.
            else 
                valido = .false.
            end if
            !!
            if (matchStack(loopStackPosition) == 0 .and. loopStack(loopStackPosition) == 0) then
                print *, "error, la expresion no cumple con '+' ", cursor, ', "',input(cursor:cursor),'"'
                call cola%enqueue(error//","//input(cursor:cursor))
                valido = .false.
                return
            end if
            if (matchStack(loopStackPosition) == 0 .and. loopStack(loopStackPosition) /= 0) then
                valido = .true.
                exit
            end if
                
            if (loopStack(loopStackPosition) == 0) then
                loopStack(loopStackPosition) = 1
            end if
        end do
        matchStack(loopStackPosition) = 0
        loopStackPosition = loopStackPosition - 1
        if (valido .eqv. .false.) then
            cola%tail => puntero
            cola%tail%next => null() 
            cursor = cursorTemporal
        end if
        !! fin de la estructura del "+"


    end function Integer

    function whitespace (input, cursor, cola) result (valido)
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        type(node), pointer:: puntero
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        integer:: cursorTemporal
        logical:: valido
        valido = .false.

        !! estructura del "*"

        ![abcd]*  ...
        ![abcd]  ...

        if (findloc([character(len=1) :: achar(32), achar(9), achar(10), achar(13)], input(cursor:cursor), 1) > 0) then
            call cola%enqueue(input(cursor:cursor))
            loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
            cursor = cursor + 1
            valido = .true.
        else
            valido = .false.
        end if

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            puntero => cola%tail  
            cursorTemporal = cursor
            loopStack(loopStackPosition) = 0
            !!
            if (findloc([character(len=1) :: achar(32), achar(9), achar(10), achar(13)], input(cursor:cursor), 1) > 0) then
                call cola%enqueue(input(cursor:cursor))
                loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
                cursor = cursor + 1
                valido = .true.
            else
                valido = .false.
                exit 
            end if

            if (findloc([character(len=1) :: achar(32), achar(9), achar(10), achar(13)], input(cursor:cursor), 1) > 0) then
                call cola%enqueue(input(cursor:cursor))
                loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
                cursor = cursor + 1
                valido = .true.
            else  
                valido = .false.
                exit 
            end if
            !!
            valido = .true.
        end do
        if (valido .eqv. .false.) then
            cola%tail => puntero
            cola%tail%next => null() 
            cursor = cursorTemporal
        end if
        loopStackPosition = loopStackPosition - 1
        valido = .true.
        !! fin de la estructura del "*"
    end function whitespace
end module tokenizer