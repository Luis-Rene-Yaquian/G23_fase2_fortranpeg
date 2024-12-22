program parser
    use tokenizer
    implicit none

    character(len=*), parameter :: input = "2 * (3 + 4)"
    character(len=:), allocatable :: salida
    integer :: cursor
    
    cursor = 1

    salida = nextSym(input, cursor)
    print *, salida
end program parser