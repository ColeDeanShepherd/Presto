# Presto
A programming language.

## Ideas
* Easy deep copy
* Macros for control flow (ex: do something, if error then return).
    * Can be done with monads (clunky).
* Discriminated unions
    * Source generators?
* Concurrency with green threads - solve function coloring problem.
* Less verbose mutable class definition syntax.
    * C# actually already has this - mutable records.
* Functions outside of classes.
    * can be done with "using static CLASS_NAME;"
* Type aliases
* Immutable freezing
* Immutable by default
* Exhaustive pattern matching
* Transparent optimization

## Example Programs
### Hello world

```
# Directly reference impl
main() = printLn("Hello, world!")

# Pass in impl
main(printLnImpl) = printLnImpl("Hello, world"!)
main(consoleImpl) = consoleImpl.printLn("Hello, world!")
main(consoleImpl) = printLn(consoleImpl, "Hello, world!")

# Environment
main() = withEnv(printLn = printLnImpl)
    printLn("")

OR

main(di) = di.console.printLn("Hello, world!")

OR

di.console = realConsole
main() = di.console.printLn("")

# Implicit Parameter

setimplicit printLn = realPrintLn
main(implicit printLn) = printLn("")

# Reader Monad

main(): Reader(printLn) = ask()("")

# Interpreter
main() = printLn("Hello, world"!)
runRealIO(main())
```

Notes:
* How to mock println? printLn refers to specific implementation, not interface. How can I fix that?
  I either need to:
    * Set the implementation in the environment (implicit param)
    * Pass in the implementation
    * run with interpreter


### Add Function
add(x, y) = x + y
x.add(y)
x `add` y

OR
add x y = x + y
OR
+ x y = x + y

Notes:
* Need a way to 

### Fibonnaci
f : Z -> Z
f(0) = 0
f(1) = 1
f(n) = f(n-2) + f(n-1)

OR

f(n: Z): Z = case n
    0 => 0
    1 => 1
    n => f(n-2) + f(n-1)

OR

f : Z -> Z
f(n) = n
    | 0 => 0
    | 1 => 1
    | n => f(n-2) + f(n-1)

OR

f 0 = 0
f 1 = 1
f n = f (n-2) + f (n-1)

OR



Notes:
* Operates on some kind of positive integer
* Can overflow with 32 bit integers

### "Recipe"
```
bakeCake(ingredients) = bake(mixIng(mixWetIng(mixDryIng())))

OR

bakeCake(ingredients) = mixDry |> mixWet |> mixIng |> bake

OR

bakeCake(ingredients) = {
    implicit state

    state = mixDry()
    state = mixWet()
    state = mixIng()
    state = bake()
}

OR

mix(ingredients) = ... # returns batter
bake(batter) = ... # returns cake
makeCake(ingredients) = mix |> bake
```

### Number guessing game

```
fn main() = {
    # generate random number
    ans = randInt(1, 10)

    # print rules
    printLn("The rule of the game is to guess the correct number between 1 and 10!")

    # while haven't guessed
    loop {
        ## prompt for guess
        printLn("What's your guess?")
        guess = readInt()

        ## output feedback
        case guess
            ans => break;
            guess < ans => printLn("Your guess is too low")
            guess > ans => printLn("Your guess is too high")
    }

    printLn("Correct! You win!")
}

# TODO: separate pure & impure code, use DI?, use recursion?

fn main() [impure] = {
    runGame(randImpl, consoleImpl)
}

fn runGame(rand, console) = {
    ans = rand.int(1, 10)
    console.printLn("rules")
    processGuesses()
    where
        fn processGuesses() = {
            console.printLn("guess?")
            guess = console.readInt()
            
            case guess
                ans => ();
                guess < ans => console.printLn("Your guess is too low"); processGuesses()
                guess > ans => console.printLn("Your guess is too high"); processGuesses()
        }
}

OR


```

### Pong

### Navier stokes simulator

### Static website
