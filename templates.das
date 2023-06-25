require daslib/rtti
require daslib/ast
require daslib/templates_boost

let foo2 = 123

[export]
def main
    var foo = "foo2"

    var fun <- qmacro_function("madd") <| $ ( a, b )
        return $i(foo) * a + b

    compiling_module() |> add_function(fun)
    
    madd(1, 2)

    print("{madd(1, 2)}\n")
