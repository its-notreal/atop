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


-- a fundamental difference between types and expressions is the time when they exist
-- type checking is performed as part of the compilation process of our program so if type-checking fails then
-- compilation will also fail
-- the expressions exist when the program is executed, known as run time

-- anything that can exist at runtime is an expression of some type
-- in the book, does type in the sentence above refer to the type of the data or is it saying anything that exists at
-- run time is some type of expression? Ambiguity throwing me off there.
-- I think in this case it means anything at run time is an expression that has a formal type

-- value, expression, and term can be used interchangeably ("more or less")

-- this is how a function is called with inputs in Haskell, apparently
-- in Haskell we want to use the word "apply" instead of call
-- I assume this has to do with the concepts in functional programming that I will learn more about later on
-- We're not calling add_one and inputting 3, we're applying add_one to the Natural 3
add_one 3

--this should return 4. Is this really the syntax?

--this is how we assign the output of add_one to a new term. In this case four is now equal to the
-- application of add_one on the input 3
four = add_one 3

-- the type checker would tell us though that add_one 3 results in an expression of type natural
-- four :: Natural

-- the type of four is Natural
-- the expression of four is add_one 3


--we can also use Haskell this way
-- putting the expression directly in the term assignment inside of parentheses
four = (\x -> x + 1) 3

-- we could also do this
four = (\3 -> 3 + 1) 3

--the above is accidentally "correct"
--we're not actually applying the 3 outside of the parentheses, we've hardcoded the values. The 3 at the end does nothing

--we could also just do
four = 3 + 1

-- going from an expression to just 3 + 1 is called a beta reduction, stylized as  β-reduction.
-- Beta reducing (\x -> x + 1) 3 results in 3 + 1 NOT \x -> 3 + 1

--we can make expressions of expressions
-- chain add_one three times to get add three
add_three :: Natural -> Natural
add_three = \x -> add_one (add_one (add_one x)))

-- but this is obviously only for illsutrative purposes. This does not scale.
-- we need something to add two numbers, e.g.


add :: Natural -> Natural -> Natural
add = \x y -> x + y

seven :: Natural
seven = add 2 5

-- applying a function named add to two inputs separated by white space
-- our type on our function has changed though
-- before we had Input -> Output and they were Natural -> Natural
-- but now we have 3 Naturals in our type definition for add

--In Haskell the type to the right of the right most arrow (->) is the output type
--Everything before is a parameter input

--a function with type A0 -> A1 -> .. -> B can be defined as
--\a0 a1 .. -> b
-- the names writen in lowercase correspond t othe type written in upper case


--where do types come from?
-- some are built in, e.g. Natural

data Season = Winter | Spring | Summer | Fall

-- Season is the name of the new TYPE of data
--This means Season is now a type that we can use in different places
-- data is the syntax keyword for defining a new data type
--types always start with an upper case letter (Season, Natural, etc)

--to the right of the equal sign thte seasons are enumerated and separated by vertical bars
--Season can only ever be one of the four values specific, Wintter, Spring, Summer, or Fall.
--These are called the constructors of the season type because only by using one of those can we construct a value
--for the type Season
--To construct a Season type the value can ONLY be ONE of these four values, always just one.
--The listed order of the constructors is not important


my_favorite_season :: Season
my_favorite_season = Fall


opposite_season :: Season -> Season
opposite_season = \x ->
    case x of
        { Winter -> Summer
        ; Spring -> Fall
        ; Summer -> Winter
        ; Fall -> Spring
        }

--The function above, opposite_season, takes a Season input type and returns a Season output type,
-- The expression uses a case function to look up the input X and return the season opposite of that.

--case expressions have the formal shape:
--case s of { p0 -> e0; p1 -> e1; ...}


--we don't need the braces in Haskell if we use proper indentation and spacing?

opposite_season :: Season -> Season
opposite_season = \x ->
    case x of
        Winter -> Summer
        Spring -> Fall
        Summer -> Winter
        Fall -> Spring

--Haskell compiler will let us know if we do not enumerate each potential pattern, for example leaving out Fall as a case
--to match against will cause the compiler to Fail and refuse to compile

--We call this a "sum type" and the compiler is doing an "exhaustiveness" check on the input of the opposite_season
--to ensure all possible types are handled by the function
--"if you only take one thing out of this book, take sum types


--Haskell is one of the very few languages to support the concept of a sum type


season_to_temperature :: Season -> String
season_to_temperature = \x ->
    case x of
        Winter -> "cold"
        Spring -> "warm"
        Summer -> "hot
        Fall -> "chilly

--first mention of String type

temperature_to_season :: String -> Season
temperature_to_season = \x ->
    case x of
        "cold" -> Winter
        "warm" -> Spring
        "hot" -> Summer
        "chilly" -> Fall

--The above function doesn't compile? Why. Lets think about this before reading on in the book to see if we can understand
--Since Haskell is exhaustive in its type checking and we must know all possible input types, this will fail because
--we are not accounting for every possible string input because there are (near?) infinite string inputs
--we probably need a default case/else type handler in the function to handle all unknown inputs, but then
--we do not have an output type of Season that would correspond to unknown.
--At this point we might be able to add an 'Unknown' Season but maybe there is an error handling option instead
--that would return something that is usable in the runtime that is just an error message that would not crash the program
--Can programs in Haskell return 2 possible data types? String -> (Season OR TRY_AGAIN).
--Maybe you need to construct an additional data type that is Season_Result that returns the season and/or the result of
--the season to handle errors.
--Just thinking critically and ahead instead of having the book spoonfeed me everything and zoning out.
--Finaly prediction, we need to add some sort of catch all to the case AND handle the error in some graceful way
--because the default won't be able to match to a season


--Surpsingly the book says to just make a new data type for Temperature:

data Temperature
    = Cold
    | Warm
    | Hot
    | Chilly

--then define temperature_to_season as
temperature_to_season :: Temperature -> Season
temperature_to_season = \x ->
    case x of
        Cold -> Winter
        Warm -> Spring
        Hot -> Summer
        Chilly -> Fall

--This also makes sense, it just isn't the direction I expected the book to go. This leaves me wondering, does Haskell
--have default case statement abilities? Is there a graceful way to handle unknown inputs and errors on the output?
--Surely there are but maybe not, maybe the point of Haskell is to never do this under any circumstances.


--Ah, now it seems we're getting into handling errors or default cases
--we use the Maybe keyword
--I find this hilarious given the other rigid seriousness of the language
--we also have a Just is a constructor I am trying to understand

data Maybe a = Nothing | Just a

string_to_temperature :: String -> Maybe Temperature
string_to_temperature = \x ->
    case x of
        "cold" -> Just Cold
        "warm" -> Just Warm
        "hot" -> Just Hot
        "chilly" -> Just Chilly
        _ -> Nothing

--_ is a wildcard symbol that will result in Nothing
--but the Just part...
--We're applying Just to the temperature data type.
--However it is not modifying the value in anyway.
--I'm still confused as to what Just is doing here even after reading.
--Is it a neccessary part of the Maybe?
--is data Maybe a = Nothing | Just a creating a type constructor to pass arbitrary types to?
--Can we do Maybe Season and get back Nothing or Just Season?
--Hoping going further into the book explain this or I get more examples
--Hopefully by continuing on I am not short changing myself

--Moving on though, we have another sum type in Haskell called Either to say something must be either this or that
--coming back to this I am not sure anymore. I think data Maybe a = Nothing | Just a is actually needed and it is
--defining our usage of Maybe

data Either x y = Left x | Right y

--The book notes left and right are insignificant and Haskell doesn't care about the names.
--Does this mean Just was invented above simply to wrap the type in? It is not a native reserve word in Haskell?
--Can I do anything in that case, e.g. data Maybe a = Nothing | Thisisjustawordforanything a? Confusing.
--Reading more on stackover flow it seems liek Just is a specifi keyword that must accompany Nothing, sort of an
--inverse null to show that we must return either Nothing (null) or the expected data type.
--So I do not believe data Maybe a = Nothing | Just a has anything to do with the strting_to_temperature function
--the maybe portion there is defined in the defintion and since we're using maybe we have to use just in the actual
--expression.
--I think I understand now but correctness of my understanding is hard to know until I do some actual examples or
--write some of my own code.
--However this doesn't seem to be the case with Left and Right in the Either example.
--Left and Right could be anything as long as they are consistent.

rhyme :: Either Temperature Season -> String
rhyme = \x ->
    case x of
        Left Cold -> "old"
        Left Warm -> "strong"
        Left Hot -> "pot"
        Left Chilly -> "lily"
        Right Winter -> "sphincter"
        Right Spring -> "bring"
        Right Summer -> "bummer"
        Right Fall -> "ball"

--there are 8 possible combinations for this sum type of "Either Temperature Season"
--This is the "cardnality" of Either Temperature Season.


--How is the either constructor different from
data ThisOrThat = This Temperature | That Season

--This means we must use temperature and season, it is not generic and can no longer be used with other types.
--Right Summer is not exactly equal to That Summer
--It contains the same information but they are represented differently.
--We do not call them equal, we call them isomorphic
--This means, roughly, that Right Summer and be converted into That Summer (or vice versa) without information loss

--But how do we prove this? We need a proof
--and a proof is a demonstration that a particular statement is always objectively and unquestionably true

--We want to prove that Either Temperature Season and ThisOrThat are isomorphic to one another.

fromEither :: Either Temperature Season -> This OrThat
fromEither = \x ->
    case x of
        Left t -> This t
        Right s -> That s

toEither :: ThisOrThat -> Either Temperature Season
toEither = \x ->
    case x of
        This t -> Left t
        That s -> Right s

--However these functions are not isomorphic, we will lost data when going back and forthwhen applying

fromEither :: Either Temperature Season -> ThisOrThat
fromEither = \_ -> This Hot

toEither :: ThisOrThat -> Either Temperature Season
toEither = \_ -> Right Summer


--duality in mathematics
--opposite relationship to things that the original had
--the dual of an exit door would be an entry door
--the dual of an input would be an output and the dual of function a-> b ould be function b -> a

--we can also make a type that has both x and y.

data Pair x y = MakePair x y

--a pair of Season and Temperature would have the type
--Pair Season Temperature
--Pair and Either are duals of one another
--In Either we pick one of two possible constructors, each with a different payload
--In pair we have just one possible constructor carrying both x and y as a payload


--The Make (or Mk) prefix is common when defining value cosntructors, it makes them eassier to tell apart the constructor
--from the type, which can be desirable.
--But it is common to reuse the name of the type constructor as the value constructor.
--The compiler doesn't care because the types and values exist in two completely different realms and don't overlap!

data Pair x y = Pair x y

--When we have pairs we no longer havea sum type, we have a product type.
--The cardnality of product types is equal to the product of the cardnalities of the parts.
--4 Seasons plus 4 Temperatures equals 8 for the sum type.
--4 Seasons times 4 Temperatures though is 16, so there are 16 possible values for the type Pair Season Temperature.

--MakePair Winter Cold
--MakePair Winter Warm
--etc

--A sumtype can be called a coproduct. The co prefix in coproduct means it is the dual of something. In coproduct
--we are saying that it is the dual type of a product type.
--We could say products are cosums as well but due to tradition we do not do that.


