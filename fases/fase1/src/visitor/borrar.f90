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
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        integer :: i
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
            lexeme = "ERROR"
        else 
            lexeme = LocalStack%getTokens()
        end if
        print *, "tokens"
        print *, lexeme
        print *, "end tokens"

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        lexeme = "ERROR"
    end function nextSym

    function Expression(texto, cursor, cola) result (valido)
        character(*),intent(in) :: texto
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        logical:: valido
        valido = .false.

        valido = term(texto, cursor, cola)
        if (.not. valido) then
            return
        end if

        !! estructura del "*"

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            loopStack(loopStackPosition) = 0
            !!
            if (whitespace(texto, cursor, cola)) then
                valido = .true.
            end if

            if (texto(cursor:cursor) == '+') then
                call cola%enqueue(texto(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else if (texto(cursor:cursor) == '-') then
                call cola%enqueue(texto(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else
                valido = .false.
            end if 

            if (whitespace(texto, cursor, cola)) then
                valido = .true.
            end if

            if (Factor(texto, cursor, cola)) then
                valido = .true.
            end if
            !!
            valido = .true.
        end do
        loopStackPosition = loopStackPosition - 1
        !! fin de la estructura del "*"

    end function Expression

    function Term(texto, cursor, cola) result (valido)
        character(*),intent(in) :: texto
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        logical:: valido
        valido = .false.

        valido = Factor(texto, cursor, cola)
        if (.not. valido .and. loopStackPosition==1 ) then
            call cola%enqueue(error)
            return
        end if

        !! estructura del "*"

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            loopStack(loopStackPosition) = 0
            !!
            if (whitespace(texto, cursor, cola)) then
                valido = .true.
            end if

            if (texto(cursor:cursor) == '*') then
                call cola%enqueue(texto(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else if (texto(cursor:cursor) == '/') then
                call cola%enqueue(texto(cursor:cursor))
                cursor = cursor + 1
                valido = .true.
            else
                valido = .false.
            end if 

            if (whitespace(texto, cursor, cola)) then
                valido = .true.
            end if

            if (Factor(texto, cursor, cola)) then
                valido = .true.
            end if
            !!
            valido = .true.
        end do
        loopStackPosition = loopStackPosition - 1
        !! fin de la estructura del "*"

    end function Term

    function Factor(texto, cursor, cola) result (valido)
        character(*),intent(in) :: texto
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        logical:: valido
        valido = .false.

        if (texto(cursor:cursor) == '(') then
            call cola%enqueue(texto(cursor:cursor))
            cursor = cursor + 1
            valido = .true.
            if (whitespace(texto, cursor, cola)) then
                valido = .true.
                if (expression(texto, cursor, cola)) then
                    valido = .true.
                    if (whitespace(texto, cursor, cola)) then
                        valido = .true.
                        if (texto(cursor:cursor) == ')') then
                            call cola%enqueue(texto(cursor:cursor))
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
        else if  (whitespace(texto, cursor, cola)) then
            if(Integer(texto, cursor, cola)) then
                valido = .true.
            else
                valido = .false.
            end if
        else
            valido = .false.
        end if

    end function Factor

    function Integer(texto, cursor, cola) result (valido)
        character(*),intent(in) :: texto
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        logical:: valido
        valido = .false.

        !! estructura del "+"
        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        matchStack(loopStackPosition) = 0
        valido = .false.
        do while (.true.)
            matchStack(loopStackPosition) = 0
            !!
            if (achar(48) <= texto(cursor:cursor) .and. texto(cursor:cursor) <= achar(57)) then
                call cola%enqueue(texto(cursor:cursor))
                cursor = cursor + 1
                matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
                valido = .true.
            else 
                valido = .false.
            end if
            !!
            if (matchStack(loopStackPosition) == 0 .and. loopStack(loopStackPosition) == 0) then
                print *, "error, la expresion no cumple con '+' ", cursor, ', "',texto(cursor:cursor),'"'
                call cola%enqueue(error//","//texto(cursor:cursor))
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
        !! fin de la estructura del "+"


    end function Integer

    function whitespace (texto, cursor, cola) result (valido)
        character(*),intent(in) :: texto
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        integer, dimension(20) :: matchStack
        integer, dimension(20) :: loopStack
        integer :: loopStackPosition = 1
        logical:: valido
        valido = .false.

        !! estructura del "*"

        loopStackPosition = loopStackPosition + 1
        loopStack(loopStackPosition) = 1
        do while (loopStack(loopStackPosition) >= 1)
            loopStack(loopStackPosition) = 0
            !!
            print *, findloc([character(len=1) :: achar(32), achar(9), achar(10), achar(13)], texto(cursor:cursor), 1) > 0
            if (findloc([character(len=1) :: achar(32), achar(9), achar(10), achar(13)], texto(cursor:cursor), 1) > 0) then
                call cola%enqueue(texto(cursor:cursor))
                loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
                cursor = cursor + 1
                valido = .true.
            else 
                valido = .false.
            end if
            !!
            valido = .true.
        end do
        loopStackPosition = loopStackPosition - 1
        print *, "token ", texto(cursor:cursor)
        !! fin de la estructura del "*"
    end function whitespace
end module tokenizer
