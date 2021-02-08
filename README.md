# bethon

Bethon is a minimal subset of Python designed for teaching programming using Systematic Program Design

## Instalation

### Dependencies

- Rust Programming Language compiler.
  - To install it, follow the official guide [here](https://www.rust-lang.org/tools/install)

> Tip: you can check if you already have Rust installed by typing `cargo --version` in a terminal.
> (If a message like "cargo x.xx.x" returns by the command, it is already installed)

### Instalation proccess

1. Clone or [download](https://github.com/Viniciusvcr/bethon/archive/master.zip) this repository.
2. In a terminal, enter the repository's folder:

```bash
cd bethon
```

3. Finally, to install the interpreter, use

```bash
cargo install --path .
```

## Language guide

### Primitive types

Like Python, Bethon's primitive types are:

- `int` for integer values:

  ```python
  10, 20, 0, -40
  ```

- `float` for floating point values (real numbers):
  ```python
  0.4, -1.0, 555.5
  ```
- `str` for string literals (words or phrases surrounded by `"`):

  ```python
  "Hello, world!"
  ```

- `bool` for boolean types:

  ```python
  True, False
  ```

There is also the `None` type, which represents an empty value.

### Operators

#### Assignment operator

| Operator |   Usage    |       Description       |
| :------: | :--------: | :---------------------: |
|    =     | var = expr | Assigns `expr` to `var` |

#### Comparison operators

| Operator |    Usage     |       Description        |
| :------: | :----------: | :----------------------: |
|    ==    | expr == expr |   Equality comparison    |
|    !=    | expr != expr |  Nonequality comparison  |
|    >     | expr > expr  | Greater than comparison  |
|    >=    | expr >= expr | Greater equal comparison |
|    <     | expr < expr  |   Less than comparison   |
|    <=    | expr <= expr |  Less equal comparison   |

#### Binary arithmetic operators

| Operator |    Usage     |          Description          |
| :------: | :----------: | :---------------------------: |
|    +     | expr + expr  |      Arithmetic addition      |
|    -     | expr - expr  |    Arithmetic subtraction     |
|    \*    | expr \* expr |   Arithmetic multiplication   |
|    /     | expr / expr  |      Arithmetic division      |
|    %     | expr % expr  | Arithmetic remainder operator |

#### Unary operators

| Operator | Usage |       Description        |
| :------: | :---: | :----------------------: |
|    +     | +expr | Returns `expr` unchanged |
|    -     | -expr |   Arithmetic negation    |

#### Logical operators

| Operator |     Usage     | Description |
| :------: | :-----------: | :---------: |
|   and    | epxr and epxr | Logical and |
|    or    | epxr or expr  | Logical or  |
|   not    |   not epxr    | Logical not |

> An expression ('expr') is any value produced or written into (literal) the program

#### Operator precedence

The operator precedence matches the Python's rule of precedence:

|     Operator     |
| :--------------: |
|        or        |
|       and        |
|       not        |
|       and        |
| == != > >= < <=  |
|       + -        |
|      \* / %      |
| unary +, unary - |
|     (`expr`)     |

> Operators in the same box have the same precedence and are evaluated left-to-right in the code

### User defined types

#### Classes

In Bethon, classes are based on Python's dataclass construction.

To maintain the status of subset, the `dataclass` module have to be imported into the program:

```python
from dataclasses import dataclass
```

or

```python
import dataclasses
```

To define a new class, mark it as a dataclass using `@dataclass` and use the keyword `class` folowed by the name of the new class.

Next, define the attributes and its types.

```python
from dataclasses import dataclass

@dataclass
class Point:
    x: float
    y: float
```

or

```python
import dataclasses

@dataclasses.dataclass
class Point:
    x: float
    y: float
```

To create a new instance of your class, call it (C-style) with the attributes in order of definition:

```python
Point(10.0, 20.0) # 10.0 will be set to 'x' and 20.0 to 'y'
```

### Constants

Constants can be defined just by writing a name anywhere in the code followed by an equal (`=`) and anything that is or produces a value.

```python
x = 10 # int literal

some_string = "some string" # string literal

is_valid = True # boolean literal

point = Point(10.0, 20.0) # User defined type constructor

fun_ret = create_point(z, y) # function call

nothing = None # an empty value
```

You can ensure the type of a constant by declaring a type after its name:

```python
int_var: int = 10
str_var: str = "hello, world!"

some_var: str = 10 # this will raise an error
```

### Functions

Function definitions start with the keyword `def` followed by:

- the functions name;
- its parameters around parentheses;
- the type of value it will produce (if any) after a `->` and;
- the function's body after a colon, like in the examples:

```python
def create_point(x: float, y: float) -> Point:
""" Creates a new Point based on the parameters (works like the default Point constructor) """

    return Point(x, y)

# no return type declared because the function does not produce any value
def print_point(p: Point):
""" Prints a Point to the screen """
    print("Point:")
    print("  x:", p.x)
    print("  y:", p.y)
```

You can call any function like:

```python
p1 = create_point(10.0, 20.0)
print_point(p1)
```

> You can write any valid code inside a function body.

> The keyword `return` will end the execution of the function and return the value produced by the expression in front of it to the function's caller

### Control flow

Control flow can be done with `if/else`

> The if condition has to evaluate to a boolean value

```python
origin = Point(0.0, 0.0)
p1 = Point(10.0, 20.0)

if p1 == origin:
    print("p1 is origin")
else:
    print("p1 is not the origin")
```

> You can write any valid code inside a if/else body.

### Assertion

It is useful to be sure about the correctness of values. `assert` can do this.

```python
assert 10 == 10
assert 10.5 > 10.4

assert True or False == True
assert True and False == False

origin = Point(0.0, 0.0)
p1 = Point(10.0, 20.0)
assert p1 != origin
```

Be careful using assert, if the expression does not evaluate to True, the program will exit with an error. Try this:

```python
assert 10 + 10 == 10
```
