--the following line can be read outloud as a program from inputs into outputs
program :: input -> output

--a compiler is itself a program, which could be notated as
program :: source code -> executable

--simplest possible program, the program returns the input it was given
program :: input -> input
--however it could also be viewed as the output of the program is also its input
program :: output -> output

--left of the arrow is in the pinput tpe and to the right of the arrow is the output type
program :: x -> x

--this is an identity. In Haskell we call this program id for short. id is not a mandatory name in Haskell.
id :: x -> x

--the "programs" above are just functions, like a typical programing lanuage

--natural numbers are the zero and all integer numbers greater than it, for example
--12, 0, 4599123854
--4.68, pi, and -58 are not.

--this function takes a natural number as input and returns natural number as output
--the type of this function is natural -> natural.
id :: Natural -> Natural

-- what happens if we named the id function for natural numbers something different?
add_one :: Natural -> Natural
add_two :: Naural -> Natural
add_three :: Natural -> Natural

--This previous portion is to demonstrate that naming is arbirtary and doesn't always represent anything meaningful. We are relying
--on optional meaningful names to decide what a function does.


--Creating an add one function
--Including the expected input and output types
--And then the expression to define the function, add_one
add_one :: Natural -> Natural
add_one = \x -> x + 1

-- :: specifies the type for add_one (input of type Natural and output of type Natural)
-- = is specifying the expression of add_one
-- read aloud we would say
-- the name add_one, whose type is Natural -> Natural, is equal to the expression \x -> x + 1
-- the expression defines the behavior of a function, it starts with a backslash, \, and extends all the way to the right
-- this is called a lambda expression
-- the expression, \x -> x + 1, can be read out loud as the function whose input we name x, and whose output is x + 1.
