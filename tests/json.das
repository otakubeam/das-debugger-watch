options indenting = 4
options no_unused_block_arguments = false
options no_unused_function_arguments = false
options no_aot = true
options strict_smart_pointers = true

require dastest/testing_boost
require daslib/utf8_utils
require daslib/json
require peg/peg

options stack = 10000000

options log

def into_table(var src: array<tuple<auto(K); auto(V)>>): table<K; V>
    var result: table<K; V>
    for elem in src
        result[elem._0] <- elem._1
    return <- result


def get_parser_with(json: string): tuple<succes:bool; value:JsonValue?; endpos:int>
    parse json
        var JsonValue: JsonValue?

        rule(WS, Element as e, EOF) <|
            return <- e



        var Element: JsonValue?

        rule(Array as a) <|
            return <- JV(a)

        rule(Object as o) <|
            return <- JV(o)

        rule(Value as v) <|
            return <- v





        var Array: array<JsonValue?>

        // Optional element list;
        // If items are present (not []), then no trailing comma is allowed

        rule("[", WS, MB(ElementList) as list, "]", WS) <|
            return <- [[array<JsonValue?>]] if list |> empty
            return <- list[0] // Take the value from optional


        var ElementList: array<JsonValue?>

        // Multiple (possibly none) comma-separated elements followed by the final one
        // If the list is not empty, there must always be a final one

        rule(*CommaSeparatedElements as els, Element as last) <|
            els |> push <| last
            return <- els


        var CommaSeparatedElements: JsonValue?

        // Helper to shave off commas

        rule(Element as e, ",", WS) <|
            return <- e




        var Object: table<string; JsonValue?>

        rule("\{", WS, MB(MappingList) as list, "\}", WS) <|
            var empty: table<string; JsonValue?>
            return <- empty if list |> empty
            return <- list[0] |> into_table


        var MappingList: array<tuple<string; JsonValue?>>

        rule(*CommaSeparatedMappings as els, Mapping as last) <|
            els |> push <| last
            return <- els


        var CommaSeparatedMappings: tuple<string; JsonValue?>

        rule(Mapping as m, ",", WS) <|
            return <- m


        var Mapping: tuple<string; JsonValue?>

        rule(string_ as s, WS, ":", WS, Element as e) <|
            return <- [[auto s, e]]






        var Value: JsonValue?

        rule("true", WS) <|
            return <- JV(true)

        rule("false", WS) <|
            return <- JV(false)

        rule("null", WS) <|
            return <- JVNull()

        rule(double_ as d, WS) <|  // <- builtin double_ rule
            return <- JV(d)

        rule(string_ as s, WS) <|  // <- builtin string_ rule
            return <- JV(s)

        option(tracing)
        option(color)



// [test]
// def test1(t: T?)
//     let input = "[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]"

//     var res_json <- get_parser_with(input).value

//     var discard_error: string
//     var json <- read_json(input, discard_error)

//     t |> equal(json |> write_json(), res_json |> write_json)


[test]
def test2(t: T?)
    let input = "\{\"a\" : 123.53, \"b\": true \}"
    var res_json = get_parser_with(input)
    print("{res_json}\n")
    var discard_error: string
    var json <- read_json(input, discard_error)

    //t |> equal(json |> write_json(), res_json |> write_json)


// [test]
// def test3(t: T?)
//     let input = "[\"123\", true,[], \{\}, \{\"hello\" : true, \"less\": \"more\"\}, [[[true]]], \"123\"]"
//     var res_json <- get_parser_with(input).value

//     var discard_error: string
//     var json <- read_json(input, discard_error)

//     t |> equal(json |> write_json(), res_json |> write_json)


// [test]
// def test4(t: T?)
//     let input = "\{\"message\": \"Hello, world!\", \"nested\": \{\"nested_again\":
//     \{\"is_it_deep\": true, \"numbers\": [1, 2, 3, 4]\}\}, \"array\": [false, null, \"mixed array\"]\}"
//     var res_json <- get_parser_with(input).value

//     var discard_error: string
//     var json <- read_json(input, discard_error)

//     t |> equal(json |> write_json(), res_json |> write_json)


// [test]
// def test5(t: T?)
//     let input = "[\{\"name\": \"John\", \"age\": 30, \"city\": \"New York\", \"siblings\":
//     [\{\"name\": \"Jane\", \"age\": 25, \"city\": \"Chicago\"\},
//     \{\"name\": \"Joe\", \"age\": 35, \"city\": \"San Francisco\"\}]\}, true, 123, null, [1, 2, 3]]"
//     var res_json <- get_parser_with(input).value


//     var discard_error: string
//     var json <- read_json(input, discard_error)

//     t |> equal(json |> write_json(), res_json |> write_json)


// [test]
// def test6(t: T?)
//     let input = "\{\"colors\": [\{\"colorName\": \"red\", \"hexValue\": \"#f00\"\},
//     \{\"colorName\": \"green\", \"hexValue\": \"#0f0\"\},
//     \{\"colorName\": \"blue\", \"hexValue\": \"#00f\"\},
//     \{\"colorName\": \"cyan\", \"hexValue\": \"#0ff\"\},
//     \{\"colorName\": \"magenta\", \"hexValue\": \"#f0f\"\},
//     \{\"colorName\": \"yellow\", \"hexValue\": \"#ff0\"\},
//     \{\"colorName\": \"black\", \"hexValue\": \"#000\"\}]\}"
//     var res_json <- get_parser_with(input).value


//     var discard_error: string
//     var json <- read_json(input, discard_error)

//     t |> equal(json |> write_json(), res_json |> write_json)
