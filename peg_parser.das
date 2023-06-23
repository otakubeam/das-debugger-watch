require strings public
require daslib/match public


variant Token
    eos: void?

    ident: string

    number: int64

    symbol: int // * + ( ) ? |

    invalid: string


def tokenize_number(chars: iterator<int>; var ahead: int&): Token
    var str : array<uint8>
    str |> push(uint8(ahead))

    while next(chars,ahead) && is_number(ahead)
        str |> push(uint8(ahead))

    if length(str) == 1 && str[0] == '0'u8 && (ahead == 'x' || ahead == 'X')
        str |> push(uint8(ahead))

        while next(chars,ahead) && (is_number(ahead) || (ahead>='a' && ahead<='f') || (ahead>='A' && ahead<='F'))
    	    str |> push(uint8(ahead))

        return [[Token number=to_int64(string(str), true)]]

    return [[Token number=to_int64(string(str))]]


def tokenize_ident(chars: iterator<int>; var ahead: int&): Token
    var str : array<uint8>
    str |> push(uint8(ahead))

    while next(chars,ahead) && (is_alpha(ahead) || is_number(ahead) || ahead=='_' || ahead=='`')
        str |> push(uint8(ahead))

    return [[Token ident=string(str)]]


def tokenize(st: string)
    return <- generator<Token> () <|

        var tst := st
        var chars <- unsafe(each(tst))
        var ahead = ' '

        while !empty(chars)
            while is_white_space(ahead) && next(chars, ahead)
                pass

            if empty(chars)
                break

            if (ahead=='+' || ahead=='-' || ahead=='*' || ahead=='/' || ahead=='[' || ahead==']' || ahead=='(' || ahead==')' || ahead=='.' ||
                ahead=='!' || ahead=='~' || ahead=='&' || ahead=='|' || ahead=='^' || ahead=='<' || ahead=='>' || ahead=='=')
                yield [[Token symbol= ahead]]
                next(chars, ahead)

            elif is_number(ahead)
	    	    yield tokenize_number(chars, ahead)

            elif is_alpha(ahead) || ahead=='_' || ahead=='`'
                yield tokenize_ident(chars, ahead)
                
            else
                yield [[Token invalid="invalid character {to_char(ahead)}"]]
                return false

        yield [[Token eos = null]]
        return false




[export]
def main

    return 0
