import Visitor from './Visitor.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
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

        if (cursor > len(input)) then
            allocate( character(len=3) :: lexeme )
            lexeme = "EOF"
            return
        end if

    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        lexeme = "ERROR"
    end function nextSym

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
        return node.exprs[0].accept(this);
    }
    visitUnion(node) {
        return node.exprs[0].accept(this);
    }
    visitExpresion(node) {
        return node.expr.accept(this);
    }
    visitString(node) {
        console.log(node.isCase);
        let salida = ''
        if (node.isCase !== undefined) {
            salida = `  if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then`;
        } else {
            salida = `  if ("${node.val}" == trim(adjustl(to_lowercase(input(cursor:cursor + ${node.val.length - 1}))))) then`;
        }
        salida +=`
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if`;
    }
}
