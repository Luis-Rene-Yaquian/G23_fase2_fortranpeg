
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
        character(len=:), allocatable :: lexeme

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal
        logical:: valido
        valido = .false.
            
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        valido = Term(input, cursor, cola)
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 1
                do while (loopStack(loopStackPosition) >= 1)
                    puntero => cola%tail  
                    cursorTemporal = cursor
                    loopStack(loopStackPosition) = 0
                    !!
                    
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        valido = _(input, cursor, cola)
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
          if ("+" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
              if ("-" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        valido = _(input, cursor, cola)valido = Term(input, cursor, cola)
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
                    !!
                    valido = .true.
                end do
                loopStackPosition = loopStackPosition - 1
                if (valido .eqv. .false.) then
                    cola%tail => puntero
                    cola%tail%next => null() 
                    cursor = cursorTemporal
                end if
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
    end function 
            function Term(input, cursor, cola) result (valido)
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        character(len=:), allocatable :: lexeme

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal
        logical:: valido
        valido = .false.
            
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        valido = Factor(input, cursor, cola)
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 1
                do while (loopStack(loopStackPosition) >= 1)
                    puntero => cola%tail  
                    cursorTemporal = cursor
                    loopStack(loopStackPosition) = 0
                    !!
                    
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        valido = _(input, cursor, cola)
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
          if ("*" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
              if ("/" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        valido = _(input, cursor, cola)valido = Factor(input, cursor, cola)
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
                    !!
                    valido = .true.
                end do
                loopStackPosition = loopStackPosition - 1
                if (valido .eqv. .false.) then
                    cola%tail => puntero
                    cola%tail%next => null() 
                    cursor = cursorTemporal
                end if
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
    end function 
            function Factor(input, cursor, cola) result (valido)
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        character(len=:), allocatable :: lexeme

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal
        logical:: valido
        valido = .false.
            
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
          if ("(" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                valido = _(input, cursor, cola)valido = Expression(input, cursor, cola)valido = _(input, cursor, cola)  if (")" == input(cursor:cursor + 0)) then
            allocate(character(len=1) :: lexeme)
            call cola%enqueue(input(cursor:cursor + 0))
            lexeme = input(cursor:cursor + 0)
            cursor = cursor + 1
            matchStack(loopStackPosition) = matchStack(loopStackPosition) + 1
            valido = .true.
            
                valido = .false.
                exit
            end if
                exit !salimos del do
                end do
            end if
            valido = _(input, cursor, cola)
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                valido = Integer(input, cursor, cola)
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
    end function 
            function ,i,,n,,t,,e,,g,,e,,r(input, cursor, cola) result (valido)
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        character(len=:), allocatable :: lexeme

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal
        logical:: valido
        valido = .false.
            
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 0
                matchStack(loopStackPosition) = 0
                valido = .false.
                do while (.true.)
                    puntero => cola%tail  
                    cursorTemporal = cursor
                    matchStack(loopStackPosition) = 0
                    !!
                    
        i = cursor
        
        
        if (input(i:i) >= "0" .and. input(i:i) <= "9") then
            lexeme = input(cursor:i)
            cursor = i + 1
            return
        end if
            
        valido = .true.
            exit
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
                    loopStack(loopStackPosition) = loopStack(loopStackPosition) + 1
                end do
                matchStack(loopStackPosition) = 0
                loopStackPosition = loopStackPosition - 1
                if (valido .eqv. .false.) then
                    cola%tail => puntero
                    cola%tail%next => null() 
                    cursor = cursorTemporal
                end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
    end function 
            function ,w,,h,,i,,t,,e,,s,,p,,a,,c,,e(input, cursor, cola) result (valido)
        character(*),intent(in) :: input
        integer, intent(inout) :: cursor
        type(Queue), intent(inout) :: cola
        character(len=5) :: error = "ERROR"
        character(len=:), allocatable :: lexeme

        !!variable para almacenar la logica de *,+,?
        integer, dimension(20) :: matchStack = 0
        integer, dimension(20) :: loopStack = 0
        integer :: loopStackPosition = 1

        !!variable para almcenar la opcion
        logical, dimension(20) :: firstOptionalFounded = .false.
        integer :: localFirstOptionalFounded = 1

        type(node), pointer:: puntero
        integer :: cursorTemporal
        logical:: valido
        valido = .false.
            
        localFirstOptionalFounded = localFirstOptionalFounded + 1
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        
                loopStackPosition = loopStackPosition + 1
                loopStack(loopStackPosition) = 1
                do while (loopStack(loopStackPosition) >= 1)
                    puntero => cola%tail  
                    cursorTemporal = cursor
                    loopStack(loopStackPosition) = 0
                    !!
                    
        i = cursor
        
        if (findloc([" ", "\", "t", "\", "n", "\", "r"], input(i:i), 1) > 0) then
            lexeme = input(cursor:i)
            cursor = i + 1
            return
        end if
            
        
        valido = .true.
            exit
                    !!
                    valido = .true.
                end do
                loopStackPosition = loopStackPosition - 1
                if (valido .eqv. .false.) then
                    cola%tail => puntero
                    cola%tail%next => null() 
                    cursor = cursorTemporal
                end if
            if (valido .and. .not. firstOptionalFounded(localFirstOptionalFounded)) then
                firstOptionalFounded(localFirstOptionalFounded) = .true.
                !pondremos un do para absorver un posible return de error y sea un exit
                do
                
                exit !salimos del do
                end do
            end if
            
        if (.not. firstOptionalFounded(localFirstOptionalFounded)) then
            valido = .false.
        end if
        firstOptionalFounded(localFirstOptionalFounded) = .false.
        localFirstOptionalFounded = localFirstOptionalFounded - 1
        
    end function 
            
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