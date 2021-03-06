class: center, middle

# Introduction to Haskell

## Paul Meng (MnO2)
## 2013/07/26

---


## Who am I?

* Translator of [Learn you a Haskell for great good](http://learnyouahaskell-zh-tw.csie.org/) 
* Several side projects in Haskell
    * [encoding-mashup-server](https://github.com/g0v/encoding-mashup-server) for [g0v](http://g0v.tw/)
    * [pulseaudio binding](https://github.com/favonia/pulse)
* Initiator of [#haskell.tw](https://plus.google.com/communities/103015857789020103513) (G+ community and Freenode channel)


---


## Why giving a FPL tech talk?

* CMU Professor Robert Harper starts to [teach Functional Programming to freshman](http://existentialtype.wordpress.com/2011/03/15/teaching-fp-to-freshmen/) in 2011
* Now lots of freshman course teaches OCaml or Standard ML.
* Most CS students in Taiwan never have a Programming Language class.


---



## A little history of Haskell

* Pronounced as /ˈhæskəl/
* Named after Logician Haskell Curry
* A strong static type, purely functional programming language with non-strict semantic
* Appeared in 1990 (PHP appeared in 1995, Python appeared in 1991)
* Famous code snippet is quick sort

    ```
    quickSort :: Ord a => [a] -> [a]
    quickSort []     = []
    quickSort (x:xs) = quickSort[a | a <- xs, a < x]
                        ++ [x] 
                        ++ quickSort [b | b <- xs, b >= x]
    ```



---




## Why Haskell?

* Create a whole new experience towards programming (if you don't have any before)
* Different beast from Lisp family
* More mature ecosystem than OCaml and Standard ML
* And some advertising benefits
    * Small language core provides big flexibility
    * Strong typing catches many bugs at compile time
    * Functional code permits better testing methodologies
    * Can parallelize non-concurrent code without changing semantics
* It also has disadvantages, but in general it is much closer to the state-of-the-art research results.


---



## Who is using Haskell (1)

* Google
    * [ganeti](https://code.google.com/p/ganeti/) is a cluster virtual server management software tool built on top of existing virtualization technologies such as Xen or KVM and other Open Source software.

![](image/ganeti.png)



---


## Who is using Haskell (2)

* Facebook
    * [lex-pass](https://github.com/facebook/lex-pass)
    * manipulate a php codebase using haskell to transform the abstract-syntax-tree

![](image/lex-pass.png)



---


## Who is using Haskell (3)

* Haskell Consulting/Education & Infrastructure Building
    * [FP Complete](https://www.fpcomplete.com/)

* Start-ups
    * [Bump](https://bu.mp/)
    * [Netcycler](http://www.netcycler.com/)

* Hedge Funds
    * [Alpha Heavy](http://www.alphaheavy.com/)
    * [Tsuru Capital](http://www.tsurucapital.com/en/)



---


## Projects using Haskell (1)

* [xmonad](http://xmonad.org/), A tiling window manager

![](image/xmonad.png)



---


## Projects using Haskell (2)

* [pandoc](http://johnmacfarlane.net/pandoc/)

<iframe src="http://johnmacfarlane.net/pandoc/try/" width="100%" height="500">
</iframe>



---


## Projects using Haskell (3)

* [git annex](http://git-annex.branchable.com/), managing large file with git.

<iframe src="http://git-annex.branchable.com/not/" width="100%" height="500">
</iframe>



---


## Projects using Haskell (4)

* [A port of Wolfenstein 3D](https://twitter.com/id_aa_carmack/status/331918309916295168)

![](image/john_carmark.png)



---


## Projects using Haskell (5)

* [Pugs](http://www.pugscode.org/) by Audrey Tang

<iframe src="http://www.pugscode.org/" width="100%" height="500">
</iframe>



---


## What is Functional Programming?




---



## Programming is Encoding

* Encoding the solution description into some (formal) system
* Therefore Functional Programming is encoding the description into (mathematical) function
* Function is first-class value

![](image/func_compose.png)



---


## Comparing to other Programming Paradigms (1)

* Encoding the pattern into the Jacquard Loom

![](image/jacquard_loom.jpg)



---


## Comparing to other Programming Paradigms (2)

* Imperative Programming encodes the description into lines of instructions, and its data
    * From Machine instructions to Assembly Language
    * Categorize bit-pattern on a physical machine.
* Procedural Programming encodes the description into lines of subprograms



---


## Comparing to other Programming Paradigms (3)

* Object-Oriendted Programming encodes the description into Object interaction and Message Passing
    * At this moment in most cases, when we are saying "Design Patterns", what we are saying is "OOP Design Patterns"
    * [Alan Kay stress that the most important property of OOP is message passing, but not Object](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html)



---


## Comparing to other Programming Paradigms (4)

* Visual Programming
    * A programming language doesn't have to be text-based
    * Encode description into Graph



---


## Theorem Prover

* Logicism: The theory that mathematics is an extension of logic and therefore some or all mathematics is reducible to logic
* Encode the description into a propositional logic statement.
* A logical statement is a theorem
* Programming is proving a theorem



---


## Human makes Mistakes

* We introduces different paradigm of composing, but we human make mistakes when doing composition.
* If no type, it will cause runtime error, or burn-out circuit boards

![](image/type_unsafe_compose.png)

* To prevent ridiculous errors, we check type.

![](image/type_check_compose.png)



---


## Real World example of Untyped (or single type)

* Untyped example: Bash
    * put a file named as "-rf"
    * then "rm *"
    * then all of directories and files will be deleted.

* In Bash, the only type of the language is "String"



---


## Mainstream PL type system defects

* Tony Hoare said "Null Reference" is his billion-dollar mistake.
* In java, "Integer" type is actually consists of "integer" and "null"
* Option Type:  "Just 1" or "Nothing".
* Compiler would complain when pass an option type value to a Integer slot.

<iframe src="http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare" width="100%" height="400">
</iframe>




---


## More Advanced Type System

* Type inference (Could be Hindley Milner, System F, or System Fw)

    ```
    Prelude> :t (1+1)
    (1+1) :: Num a => a
    Prelude> :t (1+1.0)
    (1+1.0) :: Fractional a => a
    ```

* Algebraic data types  (recursively defined types)

    ```
    data List a = Nil | Cons a (List a)
    ```

* Parametric Type variale 

    ```
    data Maybe a = Just a | Nothing
    ```



---


## Pure (Function)

* The function always evaluates the same result value given the same argument value(s). The function result value cannot depend on any hidden information or state that may change as program execution proceeds or between different executions of the program, nor can it depend on any external input from I/O devices.
* Evaluation of the result does not cause any semantically observable side effect or output, such as mutation of mutable objects or output to I/O devices.



---


## Lazy Evaluation

* An evaluation strategy which delays the evaluation of an expression until its value is needed (lazy evaluation) and which also avoids repeated evaluations (sharing)

    ```
    if a then b else c
    ```

* evaluates (a), then if and only if (a) evaluates to true does it evaluate (b), otherwise it evaluates (c). That is, either (b) or (c) will not be evaluated.




---


## An Overloook of Haskell




---


## Getting started with Haskell

* Install the [Haskell Platform](http://www.haskell.org/platform/), which includes the GHC(Glasgow Haskell Compiler)
* Create a file called `hello.hs` with the following contents:

    ```
    main = putStrLn "Hello, world!"
    ```

* Compile your program to a native executable like this:

    ```
    $ ghc --make hello
    [1 of 1] Compiling Main             ( hello.hs, hello.o )
    Linking hello ...
    $ ./hello
    Hello, world!
    ```



---


## Bindings (1)

* Haskell uses the `=` sign to declare *bindings*:

    ```
    x = 2                   -- Two hyphens introduce a comment
    y = 3                   --    ...that continues to end of line.
    main = let z = x + y    -- let introduces local bindings
           in print z       -- program will print 5
    ```

* A binding may declare a *function* of one or more arguments
    * Function and arguments are separated by spaces (when defining or
      invoking them)

    ```
    add arg1 arg2 = arg1 + arg2   -- defines function add
    five = add 2 3                -- invokes function add
    ```

* Parentheses can wrap compound expressions, must do so for arguments

    ```
    bad = print add 2 3     -- error! (print should have only 1 argument)
    ```

    ```
    main = print (add 2 3)  -- ok, calls print with 1 argument, 5
    ```



---


## Bindings (2)

* Unlike variables in imperative languages, Haskell bindings are
    * *immutable* - can only bind a symbol once in a give scope<br>
      (We still call bound symbols "variables" though)

    ```
    x = 5
    x = 6                      -- error, cannot re-bind x
    ```

    * *order-independent* - order of bindings in source code does not
       matter
    * *lazy* - definitions of symbols are evaluated only when needed

    ```
    safeDiv x y =
        let q = div x y        -- safe as q never evaluated if y == 0
        in if y == 0 then 0 else q
    main = print (safeDiv 1 0) -- prints 0
    ```



---



## Bindings (3)

* *recursive* - the bound symbol is in scope within its own
       definition

    ```
    x = 5                 -- this x is not used in main

    main = let x = x + 1  -- introduces new x, defined in terms of itself
           in print x     -- program "diverges" (i.e., loops forever)
    ```



---



## How to program without mutable variables? (1)

* In C, we use mutable variables to create loops:

	``` 
	long factorial (int n)
	{
	  long result = 1;
	  while (n > 1)
	    result *= n--;
	  return result;
	}
	```



---


## How to program without mutable variables? (2)

* In Haskell, can use recursion to "re-bind" argument symbols in new
  scope

	``` 
	factorial n = if n > 1
	              then n * factorial (n-1)
	              else 1
	```

    * Recursion often fills a similar need to mutable variables
    * But the above Haskell factorial is inferior to the C one--why?




---


## Tail recursion (1)

* Each recursive call may require a stack frame
    * This Haskell code requires `n` stack frames

        ``` 
        factorial n = if n > 1 then n * factorial (n-1) else 1
        ```

    * By contrast, our C factorial ran in constant space
* Fortunately, Haskell supports optimized *tail recursion*
    * A function is tail recursive if it ends with a call to itself
    * Unfortunately, `factorial n` multiplies by `n` *after*
      evaluating `factorial (n-1)`



---


## Tail recursion (2)

* Idea: use *accumulator* argument to make calls tail recursive

    ``` 
    factorial n = let loop acc n' = if n' > 1
                                    then loop (acc * n') (n' - 1)
                                    else acc
                  in loop 1 n
    ``` 

    * Here `loop` is tail recursive, compiles to an actual loop



---


## Every expression and binding has a type (1)

* Some basic types:
    * `Bool` - either `True` or `False`
    * `Char` - a unicode code point (i.e., a character)
    * `Int` - fixed-size integer
    * `Integer` - an arbitrary-size integer
    * `Double` - an IEEE double-precision floating-point number
    * *type1* `->` *type2* - a function from *type1* to *type2*
    * `(`*type1*`,` *type2*`,` ...`,` *typeN*`)` - a tuple
    * `()` - a zero-tuple, pronounced *unit* (kind of like `void` in
      C); there only one value of this type, also written `()`



---


## Every expression and binding has a type (2)

* You can declare the type of a symbol or expression with `::`

    ``` 
    x :: Integer
    x = (1 :: Integer) + (1 :: Integer) :: Integer
    ```

    * `::` has lower precedence than any function operators (including
      `+`)




---


## More on types (1)

* Function application happens one argument at a time
  (a.k.a. "*currying*")

    ``` 
    add :: Integer -> (Integer -> Integer)
    add arg1 arg2 = arg1 + arg2
    ```

    * So `add 2 3` is equivalent to `(add 2) 3`
    * `(add 2)` takes 3 returns 5, so `(add 2) has type Integer -> Integer`
    * `->` associates to the right, so parens usually omitted in
      multi-argument function types:<br>
      `fn ::` *argType1* `->` *argType2* `->` ... `->` *argTypeN* `->`
      *resultType*



---


## More on types (2)

* Usually the compiler can infer types
    * You can ask GHCI to show you inferred types with `:t`

    ```
    *Main> :t add
    add :: Integer -> Integer -> Integer
    ```

    * Good practice to declare types of top-level bindings
      anyway (compiler warns if missing)



---


## User-defined data types (1)

* The `data` keyword declares user-defined data types (like `struct`
  in C), E.g.:

    ``` 
    data PointT = PointC Double Double deriving Show
    ```

    * This declaration declares a new type, `PointT`, and constructor,
      `PointC`
    * A value of type `PointT` contains two `Double`s
    * `deriving Show` means you can print the type (helpful in GHCI)

* Note that data types and constructors must start with capital letters



---


## User-defined data types (2)

* Types and constructors can use the same name (often do), E.g.:
    
    ``` 
    data Point = Point Double Double deriving Show
    ``` 

* One type can have multiple constructors (like a tagged union):

    ``` 
    data Point = Cartesian Double Double
               | Polar Double Double
                 deriving Show
    ```

    ``` 
    data Color = Red | Green | Blue | Indigo | Violet deriving Show
    ```



---


## Using data types (1)

* Constructors act like functions producing values of their types

    ``` 
    data Point = Point Double Double deriving Show
    myPoint :: Point
    myPoint = Point 1.0 1.0
    ```

    ``` 
    data Color = Red | Green | Blue | Indigo | Violet deriving Show
    myColor :: Color
    myColor = Red
    ```



---


## Using data types (2)

* `case` statements & function bindings "de-construct" values with
  *patterns*

    ``` 
    getX, getMaxCoord :: Point -> Double
    getX point = case point of
                   Point x y -> x
    getMaxCoord (Point x y) | x > y     = x
                            | otherwise = y
    ```

    ``` 
    isRed :: Color -> Bool
    isRed Red = True        -- Only matches constructor Red
    isRed c   = False       -- Lower-case c just a variable
    ```



---


## Parameterized types (1)

* Types can have parameters sort of the way functions do
    * Type parameters start with lower-case letters
    * Some examples from the standard Prelude

    ``` 
    data Maybe a = Just a
                 | Nothing
    ```

    ``` 
    data Either a b = Left a
                    | Right b
    ```


---



## Parameterized types (2)

* You can see these at work in GHCI:

    ```
    Prelude> :t Just True
    Just True :: Maybe Bool
    Prelude> :t Left True
    Left True :: Either Bool b   
    ```

* Notice the type of `Left True` contains a type variable, `b`
    * Expression `Left True` can be of type `Either Bool b` for any
      type `b`
    * This is an example of a feature called *parametric polymorphism*



---


## More deconstruction tips

* Special variable "`_`" can be bound but not used
    * Use it when you don't care about a value:

    ``` 
    isJust :: Maybe a -> Bool      -- note parametric polymorphism
    isJust (Just _) = True
    isJust Nothing  = False
    ```

    ``` 
    isRed Red = True
    isRed _   = False              -- we don't need the non-red value
    ```

    * Compiler warns if a bound variable not used; `_` avoids this



---


## Lists (1)

* We could define homogeneous lists with the `data` keyword

    ``` 
    data List a = Cons a (List a) | Nil

    oneTwoThree = (Cons 1 (Cons 2 (Cons 3 Nil))) :: List Integer
    ```



---


## Lists (2)

* But Haskell has built-in lists with syntactic sugar
    * Instead of `List Integer`, the type is written `[Integer]`
    * Instead of `Cons`, the constructor is called `:` and is *infix*
    * Instead of `Nil`, the empty list is called `[]`

    ``` 
    oneTwoThree = 1:2:3:[] :: [Integer]
    ```

    * But there are even more convenient syntaxes for the same list:

    ``` 
    oneTwoThree' = [1, 2, 3]    -- comma-separated elements within brackets
    oneTwoThree'' = [1..3]      -- define list by a range
    ```

* A `String` is just a list of `Char`, so `['a', 'b', 'c'] == "abc"`


---



## Some basic list functions in Prelude (1)

``` 
head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"
```

``` 
tail :: [a] -> a             -- all but first element
tail (_:xs) = xs
tail []     = error "tail: empty list"
```

``` 
a ++ b :: [a] -> [a] -> [a]  -- infix operator concatenate lists
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
```



---


## Some basic list functions in Prelude (2)

``` 
length :: [a] -> Int         -- This code is from language spec
length []    =  0            -- GHC implements differently, why?
length (_:l) =  1 + length l
```

``` 
filter :: (a -> Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  = filter pred xs
```

Note function `error :: String -> a` reports assertion failures



---


## Lambda abstraction

* Sometimes you want to name the arguments but not the function
* Haskell allows anonymous functions through *lambda abstraction*
    * The notation is `\`*variable(s)* `->` *body* (where `\` is
      pronounced "lambda")

* Example:

    ``` 
    countLowercaseAndDigits :: String -> Int
    countLowercaseAndDigits = length . filter (\c -> isLower c || isDigit c)
    ```



---


## Hoogle

* [Hoogle] is a search engine just for Haskell functions



---


## Try Haskell

* [Try Haskell](http://tryhaskell.org/)



---


## Courses

* [FLOLAC 2012](http://flolac.iis.sinica.edu.tw/flolac12/doku.php?id=zh-tw:start)
* [CS240h](http://www.scs.stanford.edu/11au-cs240h/notes/)
* [Functional Programming](http://verify.rwth-aachen.de/fp05/)
* [Functional Programming (Video)](http://video.s-inf.de/#FP.2005-SS-Giesl.(COt).HD_Videoaufzeichnung)



---


## Books 

* [Getting Started with Haskell](http://bob.ippoli.to/archives/2013/01/11/getting-started-with-haskell/)
* [Write yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
* [Wikibook - Haskell](http://en.wikibooks.org/wiki/Haskell)
* [Learn you a Haskell for GG](http://learnyouahaskell.com/)
* [Real World Haskell](http://book.realworldhaskell.org/)



---



## Thank you
