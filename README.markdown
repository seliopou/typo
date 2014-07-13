# Typo

Typo is a programming language that runs in Haskell's type system. A Typo
program is compiled to a Haskell module that's then typechecked to compute the
result. All computation happens in Haskell's type system. 

## Installation

Clone the Typo repository and run the following commands to build and install
it from source:

    runhaskell Setup.lhs configure
    runhaskell Setup.lhs build
    runhaskell Setup.lhs install

This will install the compiler&mdash;`typoc`&mdash;on your system where the
rest of your Cabal executables live. There is another
executable&mdash;`typo`&mdash;that should be installed, but Cabal does not
support installing non-Haskell executables. It's up to you to get the `typo`
executable in your `PATH`. Or, just run it from the repository root every
time you want to use it.

If you prefer not to install the `typoc` binary, you can skip the install step
and run `typo` like so from the repository root:

    PATH=./dist/build/typoc:$PATH typo

## Language Features

Typo is a Scheme-like language. A program consists of zero or more function
definitions and an expression, which is required. The result of a Typo program
is the result of the final expression when evaluated in the context of the
preceding definitions and the built-in integer and boolean operators.

### Example

    $ typo <<EOF
    (define (fac n)
      (if (== n 0)
          1
          (* n (fac (- n 1)))))

    (fac 5)
    EOF

This program evaluates to `120`. Note that you have to provide `typo` with its
input program via `/dev/stdin`. You can use a [here document][here] like above
to write one-off programs right on the command line. You can use the `echo`
command for even shorter one-liners:

[here]: http://en.wikipedia.org/wiki/Here_document

    $ echo "(* 5 (* 4 (* 3 (* 2 1))))" | typo
    120

You can use `cat` for larger programs you've been developing in a file:

    $ cat examples/fac.typo | typo
    120

And finally, you can mix and match them with [command grouping][grp]. This is
really useful if you want to organize your definitions in separate files and
then combine them to produce a program:

    $ { cat examples/fac-defines.typo; echo "(fac 5)" } | typo
    120

[grp]: http://www.gnu.org/software/bash/manual/html_node/Command-Grouping.html

### Literals

Typo supports two data types: integers and booleans. You can specify any
non-negative integer using its numeral in decimal form, e.g., `0`, `1`, `2`,
`3`, `5`, `7`, `11`, `13`, `17`, etc. You can specify true using `#t` and false
using `#f`.

### Syntactic Forms

There are four syntactic forms for expressions, and a separate form for
definitions.

#### Expressions

  * `(op e1 e2)`: binary operator application, where `op` must be one of the
    integer operators or binary operators, and `e1` and `e2` are expressions.
  * `(fn e ...)`: function application, where `fn` is a function name followed
    by zero or more expressions.
  * `(let (id b) e)`: let-binding, where `id` is an identifier and `b` is an
    expression whose result will be bound to `id` in the expression `e`.
  * `(if c t f)`: conditional branching, where `c`, `t`, and `f` are
    expressions. If the expression `c` evaluates to `#t`, then the entire
    conditional evaluates to the result of the expression `t`. If `c` evaluates
    to `#f`, then the entire conditional evaluates to the result of the
    expresison `f`.

#### Definitions

`(define (fn id...) e)` defines a function whose name is `fn`, whose body is
the expression `e`, and which takes zero or more arguments. A function
application binds the formal arguments to the actual argument names and then
evaluates the body `e`.

### Integer Operators

Typo supports the following integer operations:

* `+`: addition
* `-`: subtraction
* `*`: multiplication
* `\`: integer division or quotient
* `%`: integer remainder or modulus
* `<`: less than
* `==`: equality

All integer operators are binary operators, so there is no negation operator.
To mimic negation, use `(- 0 n)`, where `n` is the number you wish to negate.
Alternatively you can define your own integer negation function to use instead:

    (define (negate n)
      (- 0 n))

### Boolean Operators

Typo supports the following boolean operations:

* `&&`: and
* `||`: or
* `->`: implication

All boolean operators are binary operators, so there is no negation operator.
To mimic negation, use `(-> b #f)`, where `f` is the boolean you wish to
negate. Alternatively you can define your own boolean negation function use
instead:

    (define (not b)
      (-> b #f))

# Historical Note

This is the first programming language to be publicly released from a plane
flying over the Atlantic Ocean. I'm almost certain of that.

# License

BSD3, see LICENSE file for its text.
