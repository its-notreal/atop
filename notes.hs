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

--The book notes left and Right are insignificant and Haskell doesn't care about the names.
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

--We can make pairs of pairs or eithers?

--returns either a natural or (season or string)
--the type says we will provide either a type of Natural or an Either Season String type. Its nested.
--I think I grok it, but not well enough to explain it back to someone else, I guess.


my_season :: Either Natural (Either Season String)
my_season = Right (Left Fall)


--This can also be done with pairs

my_things :: Pair (Pair Natural Season) String
my_things = MakePair (MakePair 5 Summer) "songs"

f :: Pair (Pair x y) z
    -> Pair x (Pair x y)
f = \(MakePair (MakePair x y) z) ->
    MakePair x (MakePair z y)

--we could even combine pairs and either as well
--Either (Pair Natural Season) (Pair Season Temperature)
--This would be a type of either a Natural and a Season or a Season and a Temperature

--moving on...

three_things :: (Season, Natural, Temperature)
three_things = (Summer, 2, Hot)

add3 :: (Natural, Natural, Natural) -> Natural
add3 = \(x, y, z) -> x + y + z

--sometimes tuples are called anonymous product types
--there is no special syntax for anonymous sum types
--as a result we are forced to nest eithers to accomplish this task
--in practice we don't and instead we define and name a new sum type when we need one


--tuples have a fixed size
--we need a variable size container for values
--enter the linked list
--linked lists allow grouping together of 0 or more values of the same type without needing to know how many ahead of time
--we can construct a linked list from maybe?
--Maybe is a coproduct (or sum type) where one of the constructors, conveys the idea of the list being empty
--and the other one conveys that some a is in the list
--if we expand on this concept...

data Maybe a
    = Nothing
    | Just1 a
    | Just2 a a
    | Just3 a a
    | .... etc


--this is a recursive datatype
--the data type lists itself by name
--the List type can have an empty or a list a.
data List a = Empty | OneMore a (List a)


--empty is a value of type List a for any a of our choosing.
--In this case we have made a a type of Natural
--this indicates that our list will contain Natural numbers
Empty :: List Natural

--we will now use the OneMore constructor to add to our list
OneMore 8 Empty :: List Natural

--Adding element 5 to the list
OneMore 5 (OneMore 8 Empty) :: List Natural


--Nil means nothing
--Cons means "constructing a pair"
data List a = Nil | Cons a (List a)



--The lists is inductively recursive. There are other types of recursion.
--Inductive datatype definitions always start from a base case
--0 is the base case for Natural numbers
-- -1 is the base case for negative integers
-- etc
--We are going to prove that natural numbers are infinite.
--We need a new data type, Nat, abbreviated to prevent confusion with the built in Haskell data type.
--One of the constructors for our datatype will be Zero, akin to Nil in our previous list. It is the smallst Nat.

--Succ stands for Successor
--No matter what naural number we have, we can always obtain its successor, the natural number immediately after

data Nat = Zero | Succ Nat

--This works so that:

zero = Zero :: Nat
one = Succ Zero :: Nat
two = Succ (Succ Zero) :: Nat
three = Succ (Succ (Succ Zero)) :: Nat


zero    = Zero          :: Nat
one     = Succ zero     :: Nat
two     = Succ one      :: Nat
three   = Succ two      :: Nat


--We're not going to convert from Nat to Natural.
fromNat :: Nat -> Natural
fromNat = \x ->
    case x of
        Zero -> 0
        Succ y-> 1 + fromNat y


fromNatural :: Naturall -> Nat
fromNatural = \x ->
    case x of
        0 -> Zero
        _ -> Succ (fromNatural(x-1))



--creating a lambda calculus
--initially only concerned with expressions
--our lambda calculus tells us what kind of expressions we can use, how, and what they mean
--lambda calculuses are one of three things
--reference to a value
--a lambda expression
--application of an expression to some other expression

data Expr
    = Lam String Expr
    | App Expr Expr
    | Var String

expr_id :: Expr
expr_id = Lam "x" (Var "x")

--We make expr_id a type of Expr
--Lam "x" is saying that expr_id will be a function taking one parameter as input, which is called x


expr_five :: Expr
expr_five = App expr_id (Var "five")


--now lets go back to linked lists, or just lists as they will be called
--to add the element 3 to a list of Natural numbers, with a type of [Natural], the syntax is:
3 : []

--You could also do [3] but this does to acheive the goal of explicitly consing (contructing the pair) the list to grow it
--We can add more numbers:
1 : 2 : 3 : []
--which is the same as
[1, 2, 3]
--this is an empty list, or nul:
[]
--this is "cons", which from earlier means "constructing a pair":
:


--lets increae a list of numbers
add_ones :: [Natural] -> [Natural]
add_ones = \[a, b ,c] -> [a + 1, b + 1, c + 1]

--this does not scale for lists of varying lengths, empty lists, etc.
--we are going to introduce mapping now.

map :: (x -> y) -> [x] -> [y]

--map says, given a function from a value of type x to a value of type y, and a list of said x values will return
--a list of values of type y
--internally map will apply thte given function to each element of the given list individually, effectively transforming each value
--x into one type of y

map :: (x->y) -> [x] -> [y]
map = \f xs ->
    case xs of
        [] -> []
        x : rest -> f x : map f rest

--constructing this from our own list element

map :: (x->y) -> List x -> List y
map = \f xs ->
    case cs of
        Nil -> Nil
        Cons x rest -> Cons (f x) (map f rest)


add_ones :: [Natural] -> [Natural]
add_ones = \xs -> map add_one xs

--add_ones = \xs -> map add_one xs can be rewritten as
-- add_ones = map add_one
-- another example is that
-- f is the same as \x -> f x
-- when the input and output are the same then you do not need to add them to the lambda expression
-- This is called pointfree style of defining a function.
-- This is just a syntax difference. Unclear on the benefits as of now but for now the book says htat it improves
-- readability in some situations when removing or adds clarity when writing the more verbose form.
--"this has its roots in the concept of 'Eta-conversion', stylized as n-conversion, which says that this is possible"
--I don't understand the implications of this sentence, it seems tautological to say that it has roots in a concept that says it is possible
--maybe it will be elaborated on later
--however the more verbose form is called the n-expanded form but it is apparently okay to forget this?


--returning to add_ones = map add_one
--the map is only "partially applied" inside of the pointfree definition of add_ones
--this is because not all of th parameters the map was expecting have been provided yet, only part of them have


--In Haskell partial application works because when there is a function type like:
-- a -> b -> c -> d
--is, in reality, a function with a type like this:
-- a -> (b -> ( c -> d))
-- In Haskell there is no function taking more than one parameter, all functions take only one parameter in Haskell.
--They immediately return something right away.
--It is just that sometimes the thing a function returns is another function, so we take a mental short cut and say
-- a -> b -> c -> d
-- but a -> b -> c -> d is not a function that takes three parameters as input and returns d as an out put.
-- a -> b -> c -> d is actually a -> (b -> (c -> d)) that takes a as an input and returns
-- a function b -> (c -> d) as an output
--which takes b as an input and returns a function c -> d
--the paraentheses are rarely written down, but they are always there.


--something similar happens when a lambda expression is used to define a function
--we write something like \a b c -> d when a function is written that takes more than one parameter as an input.
--but we have acknowledged that taking more than one parameter is not actually a think in Haskell, so what is actually happening?

--\a b c -> d is magic syntax for writing:
\a -> (\b -> (\c -> d))

--because people find this ugly.
--And since the parentheses are unneeded as well it could also be written as:
\a -> \b -> \c -> d


--chapter 31 lost me a little bit with partially applying add function (with type Natural -> Natural -> Natural) using map
--I believe the point being driven home is that the following:
[3, 4]
map (add 1) [2, 3]
map add_one [2, 3]


--all mean the same thing because map (add 1) [2, 3] would add on to each element in the list.
-- map :: (x->y) -> List x -> List y

-- I am hoping more examples will be clarifying for me. I need something meatier to sink my teeth into.
--Learn better by doing, usually.

--reminder that our type definition of map is:
map :: (x -> y) -> [x] -> [y]
--and we could make our expression:
map = \_ _ -> []

--in this case the input is discarded and a list [y] is returned. This passes the type checker definition.
--but this is not what we wanted the map to do, it is just passing type checking.
--we need more to ensure that our function works as we expect
--we need to convey that something can be 'mapped over' without actually saying what that something is

--typeclasses
--typeclasses represent a class of types that supports some particular operations.
--in the case of 'mapping'we will concern ourselves with the class of types in which we can find values of a particualr
--type that can be replaced by values of a potenitally different type.
--this is what our map funciton did, but by specifying lists with the brackets it was made more concrete, which we do not want (Why?)


--Haskell comes with a built in function called fmap that solves this problem:
fmap :: Functor f => (x -> y) -> f x -> f y
--when comparing fmap to our list map:

map ::               (x -> y) -> List X -> List y

--they are very similar. f has replaced all instances of List and we have a new keyword, 'Functor' which is describing something about 'f'

--the Functor to the far left of the => is 'constraining'
--Functor is the typeclass in question. Functor f is saying that the f type parameter appearing to the right of the
-- => symbol can be any type as long as it belongs to the class of types that implement the features required by the Functor typeclass.
--Functor is just the name we give to these things that can be mapped over, such as Lists.


--What is 'Functor'?
--Functor is a built in typeclass in Haskell. But reproducing it for learning:

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--This notation is creating a new typeclass, as indicated by the class keyword.
--f is a place holder for any type that implements what the Functor typeclass requires.
--What is the requirement for Functor?
-- a definition of fmap.


--the type of fmap is
Functor f => (a -> b) -> f a -> f b



--The class of types that can be mapped over are called the Functor class
--To be part of the Functor class, the type must implement the fmap method.
--When a function is declared as part of the type class it is called a method instead of a function


--Given all of the above, does a 'type f' belong to multiple class types? If we have a typeclass called:

class SillyFunction f where
    silly :: (a -> b) -> f a -> f b


--Can 'type f' also belong to SillyFunction class type if it implements both silly and fmap? I assume so from what
--I am reading. So types can belong to multiple classtypes that have different requirements for being part of the class type
--Do types opt into being part of that classtype or do they just become part of it once they meet the requirements of
--required implementations?
--This feels like it will have large implications for the structure of programs in Haskell that I am unable to grok at this juncture




instance Functor List where
    fmap = \g xs ->
        case xs of
            Nil -> Nil
            Cons x rest -> Cons (g x) (fmap g rest)

--The connection between typeclass 'Functor' and the List type we made, we create an 'instance'
--in this instance we have implemented fmap for the List type
--To be more precise, according to the book, we have implemented the instance of the Functor typeclass for the List type
--There can only be one instance of a particular typeclass for a particular type.
--e.g. we can't have two instances of Functor for List

--Ah, yes, what I wondered earlier has been stated explicilty.
--'f type' can have many instances of different type classes.
--So all typeclasses must be opted into by the instance keyword. Without implementing an instance of a typeclass
--a type is not part of that typeclass.



add_ones :: Functor f => f Natural -> Natural
add_ones = fmap (add 1)

instance Functor Maybe where
    fmap = \g ya ->
        case ya of
            Nothing -> Nothing
            Just a -> Just (g a)


--by focusing on what properties a type has and what the type is capable of is called
--parametricity
--parametricity says that no matter what unknown types might be in our polymorphic function
--the behavior of the function will always be the same

--add_ones adds one to all the elements of any functor
--id whatever is given to it
--this is true for all types we could choose to use add_ones and id with


--all of this does not prevent us from implementing fmap incorrectly ourself

instance Functor Maybe where
    fmap = \_ _ -> Nothing

--in this instance the type of fmap  method is
-- (a -> b) -> Maybe a -> Maybe b

--this broken fmap implementation follows the expectations of the type system but failed to satisyf
--the Functor laws

--the first Functor law, which is the functor identity law, says that mapping the identityfunction id over some
--functor X should return that same x unmodified

fmap id x == x



--function composition
foo :: Natural -> Natural
foo = \x -> (x * 10) + 1

--foo is the composition of multiplying by ten and adding one

foo :: Natural -> Natural
foo = compose add_one multiply_by_ten


multiple_by_ten :: Natural -> Natural
multiply_by_ten = \x -> x * 10

add_one :: Natural -> Natural
add_one :: \x -> x + 1

--the 'type' of compose
compose :: _ -> _ -> Natural -> Natural


compose
    :: (Natural -> Natural)
    -> (Natural -> Natural)
    -> (Natural -> Natural)

--remember that right most parentheses are redundant
--other parentheses are used to indicate an indvidual input parameter

--what this means is that for compose we are inputting (Natural -> Natural) as a single input.
--we have 2 inputs and an output and the 2 inputs are the Natural -> Natural in the ()s



--we are trying to achieve
foo = \x -> (x * 10) + 1

--we are applying compose to add_one and multiply_by_ten
--we want to do this to our input, one after another

compose
    :: (Natural -> Natural)
    -> (Natural -> Natural)
    -> (Natural -> Natural)

compose = \g f x -> g (f x)


--alternatively, without the unneded right most parentheses
compose
    :: (Natural -> Natural)
    -> (Natural -> Natural)
    -> Natural
    -> Natural

compose = \g f x -> g (f x)
--if we beta reduce this function:

\x -> add_one (multiply_by_ten x)
\x -> (x * 10) + 1

--but we want to get rid of the naturals in the compose arguments so that we can avoid
--functions that meet the type requirements but do nothing

compose :: (x -> x) -> (x -> x) -> x -> x


--but this would still let us do bad things, such as

compose = \_ _ a -> a

--because we're using the correct types but we're returning the wrong value

--so we need to go further and ensure that the application of the function is done in a specific order
--and we're back to parametric polymorphism

compose :: (b -> c) -> (a -> b) -> a -> c
compose = \g f a -> g (f a)

--since c is our output
--we have to go back and see that b is what produces c
--and a produces b
--and a produces a
--so the order has to be g (f a)
--because we must apply them in the order we need to get c, the right most output
--I think. I am trying to understand.

--learning about the second functor law

fmpap (compose g f) x == fmpag g (fmap f x)


--laws aren't a real thing in haskell
--i thought they might be but was skeptical how
--turns out I was right to be skeptical
--its just about comments


--this function performs the addition
--of its two input parameters
add :: Natural -> Natural -> Natural
add :: \x y -> x + y -- Here is another comment!

-- Functors are expected to satisfy the following laws:
--
-- Identity law:
--    fmap id == id
--
-- Composition law:
--    compose (fmap g) (fmap f) == fmap (compose g f)


--when saying something like
-- 2 * 3
--the * is a function
-- and since it appears BETWEEN the parameters instead of before them (eg * 2 3)
-- we call this an 'infix' function
-- it too has a type!
--for now we will say the type of * is Natural -> Natural -> Natural
-- but that is obviously simplified for now

--doesn't work
* 2 3

--does work
(*) 2 3

--any time an infix function is not placed BETWEEN the parameters it must be enclosed in parentheses
--when done this way it is in a 'prefix' position
--this term is not often used though because the default 'fixity' of a position is prefix when
--we define a new function or constructor
--this means we just assume functions are in the prefix condition by default unless told otherwise

--we're going to implement * as a function just for exercise

(*) :: Natural -> Natural -> Natural
(*) = \x y ->
    case y of
        0 -> 0
        _ -> x + x * (y - 1)

    infixl 7 *

--infixl means it associated to the left and has a precedence of 7
-- + would have a precedence of 6


-- : needs the value of a list element as its first parameter and a list as its second argument


--functor type class
class Functor f where
    fmap :: (a -> b) -> f a _> f b

--functions are functors
--then we must be able to write a functor instance where functions somehow are that f (functor)

--in Functor f, the f is a type constructor, not a type


--this is nonsense:
fmap :: (a -> b) -> Maybe Natural a -> Maybe Natural b
--because f must be a type, Maybe Natural is not a type because maybe type to maybe type isn't telling us anything

fmap :: (a -> b) -> Maybe a -> Maybe b
--this is correct, as maybe is a type


--however Either x is an acceptable type:
fmap :: (a -> b) -> Either x a -> Either x b

--this way x can be a type such as Natural, I think

instance Functor (Either x) where
    fmap = \g e ->
        case e of
            Left x -> Left x
            Right a -> Right (g a)

--in the above the left parameter in the () is left unchaged, we simply return the same value for it
--But I confess I am a little lost

--saying it out loud is
--given a value of type Either x a as input, fmap modifies the a somehow, if any, possibly change its type
--but leave the x untouched

--for example

fmap add_one (Left 2)

--results in the same Left 2
--but

fmap add_one (Right 2)

--results in Right 3

--this is the definition, again, of Either, which is comprised of
-- Left X or Right y
data Either x y = Left x | Right y

