# Functional programming

## Background reading for Knossos

[F# for fun and profit](https://fsharpforfunandprofit.com/) is a great
site for learning about functional programming in general.  Although
the site is focused on F#, most of the lessons carry over to other
functional languages.  For an introduction to programming in Knossos I
recommend reading the following the following pages, in order.  I've
noted where they mention F#-related concepts that don't apply to
Knossos.

* <https://fsharpforfunandprofit.com/posts/thinking-functionally-intro/>

* <https://fsharpforfunandprofit.com/posts/mathematical-functions/>

  (At the end, the article briefly mentions passing multiple values to
  functions with a technique called "currying".  In Knossos we use
  tuples to hold multiple arguments and we don't really use
  "currying".)

* <https://fsharpforfunandprofit.com/posts/function-values-and-simple-values/>

  (Note that Knossos does not, at the moment, support binding
  functions to new names, although it may in the future.)

* <https://fsharpforfunandprofit.com/posts/expressions-vs-statements/>

## Expression-based style versus statement-based style

Here are some functions in statement-based style in Python, and the
equivalents in expression-based style, in Python and Haskell.

### Single conditional

If a function consists of a single conditional then the returns from
the function can be at the end of each branch.

#### Python statement-based

```python
def f1(x):
    if x > 0:
        return x
    else:
        return 0
```

#### Python expression-based

In expression-based style we don't use early return.  Instead we can
use Python's expression-based ternary conditional operator.  The
expression-based conditional operator *itself* has a value that can be
assigned to a variable or returned from a function.

```python
def f1(x):
    return x if x > 0 else 0
```

#### Haskell (expression-based)

```haskell
f1 x = if x > 0 then x else 0
```

### Multiple conditionals

Early return was convenient when the function consisted of a single
`if` but it doesn't work if we want to return a result based on two
conditionals.  Instead we have to create new varibles in each branch
to hold the "result" of the `if`.

#### Python statement-based

```python
def f2(x, y):
    if x > 0:
        xr = x
    else
        xr = 0


    if y > 5:
        yr = y * y
    else:
        yr = 0


    return xr + yr
```

#### Python expression-based

If we use expression-based `if` then the `if` expression itself
returns its value.  It is generally more compact than the
statement-based form.

```python
def f2(x, y):
    xr = x if x > 0 else 0
    yr = y * y if y > 5 else 0

    return xr + yr
```

#### Haskell (expression-based)

```haskell
f2 x y = let xr = if x > 0 then x else 0
	         yr = if y > 5 then y * y else 0
	     in xr + yr
```

### Conditionals that contain more than a single expression

Statement-based `if` has no difficulty when its branches contain more
than a single expression.

#### Python statement-based

```python
def f3(x):
    if x > 0:
        y = x * x + 3 * x
        return y * y - x
    else
        return 0
```

#### Python expression-based doesn't work

Expression-based `if` struggles, however.  There's nothing we can fill
in for `...`!  The assignment to `y` followed by the calculation of
the return value is not a single expression.  Even a lambda won't
work, because the body of the lambda itself has to be a single
expression.


```python
def f3(x):
    return ... if x > 0 else 0
```

#### Python expression-based with helper function

If we really want to use the conditional expression form then we can
define a helper function, but it feels a bit clumsy.

```python
def f3(x):
    def f():
        y = x * x + 3 * x
        return y * y - x

    return f() if x > 0 else 0
```

#### Haskell (expression-based)

On the other hand, functional languages allow assigning a variable and
using the result within the same expression.  In Haskell we can write

```haskell
f3 = if x >= 0
     then let y = x * x + 3 * x
          in y * y - x
     else 0
 ```

### Creating lists/arrays

#### Python statement-based

Python can imperatively create a list by appending values.

```python
def h1(n):
   ret = []
   for i in range(n):
       ret.append(i * i)
   return ret
```

#### Python expression-based

It's generally much clearer to use a generator expression which gives
the entry for every element of the list.

```python
def h1(n):
    return list(i * i for i in range(n))
```

#### Haskell (expression-based)

In Haskell, `[1..n]` is like Python's `range(n)`.

```haskell
def h1 n = [i * i | i <- [0..n-1]
```

### Creating lists/arrays, more complicated

The iterative approach has no difficulty creating a list from a
multi-line block of code.

#### Python statement-based

```python
def h1(n):
   ret = []
   for i in range(n):
       j = i * i + 6
       ret.append(j * j)
   return ret
```

#### Python expression-based

The expression-based style doesn't work here.  If we try a generator
expression then, like in the case for `if` above, there is nothing we
can fill in for `...`.

```python
def h1(n):
    return list(... for i in range(n))
```

#### Python expression-based

Although we can use the expression-based form with a helper function
it's a bit clumsy.


```python
def h1(n):
    def f(i):
		j = i * i + 6
		return j * j

    return list(f(i) for i in range(n))
```

#### Haskell (expression-based)

Functional languages don't have any difficulty with this because they
support binding variables within expressions.

```haskell
h1 n = [let j = i * i + 6 in j * j | i <- [0..n-1]]
```

