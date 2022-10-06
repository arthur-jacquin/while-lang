# An interpreter for the WHILE programming language

The WHILE language is a simple imperative language. The instruction set is very limited:
- arithmetic operations on integers (addition, substraction, multiplication),
- boolean operations (AND, OR, NOT, ==, <=),
- variable assignments,
- if-then-else and while control flow statement.

`interpreter.ml` is an interpreter of this language.


## Notes

- The interpreter has NOT been extensively tested.
- One should use the Reverse Polish Notation in arithmetic expressions.
- Negative number input is not supported. Please use `0 <integer> -` instead.
- For usability concerns, some features has been added on top of the language:
    - Add of the `print <arithmetic expression>` command,
    - Support for indentation (ignored by the interpreter),
    - Support for comments (lines starting with `#`) and empty lines (both ignored by the interpreter).


## Syntax

### Inline syntax

- Variable: any uppercase letter

- Arithmetic expressions: any RPN expression with:
    - integers or variables as literals
    - `+`, `-` or `*` as operators

- Boolean operators and literals:
    - `TRUE`
    - `FALSE`
    - `(<condition>) AND (<condition>)` 
    - `(<condition>) OR (<condition>)` 
    - `NOT (<condition>)` 
    - `<arithmetic expression> == <arithmetic expression>`
    - `<arithmetic expression> <= <arithmetic expression>`

### Instruction syntax

#### Comment

```
# <comment>
```

#### Assignment

```
<variable> := <arithmetic expression>
```

#### Flow control

Indentation of code blocks is not needed (the interpreter ignores it), but is supported to write clearer scripts.

```
if <condition> then {
    <code block>
} else {
    <code block>
}    
```
```
while <condition> do {
    <code block>
}
```

#### Printing

```
print <arithmetic expression>
```

### Additionnal details

Some `.while` scripts are included in this repository as examples. 

For a more precise description of the syntax used, one can either:
- look at the BNF-like description in `syntax.bnf`,
- read the parsing section of the code,
- wait for a syntax diagram ;).


## License

Please see `LICENSE`.


## Use

To interpret any `.while` script, one can either:
- use the ocaml interpreter: `ocaml interpreter.ml <file name>`,
- compile the interpreter to get a standalone executable: `ocamlopt -o while_interpreter interpreter.ml`.
