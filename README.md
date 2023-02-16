# Presto
A programming language.

## Ideas
* Numbers are boundless by default. Bounded numbers are an optimization.
* Concurrency with green threads
    * async operations block the green thread
    * can explicitly launch a new green thread somehow
    * green threads can return a value?

## Questions
* Explicitly mark impure functions?
* Significant whitespace?
* Run on CLR? JVM? BEAM? AOT?
* How to handle logging
* How to use monadic error handling with an interface with multiple implementations?
* Interesting problems:
  * Updating one small piece of state in a large tree (AST transformations, game state transformations).
    * Concurrently!
  * High-volume RESTful server
  * Selecting partial data from SQL, operating on DTOs, etc.
  * Adding attribute to existing data type
  * Adding additional data to object identity (ECS type program)

## To-Do
* Implement comments
* Handle presence or absence of newline at end of source code
* Multi-file support
* Name shadowing?
* Better error handling than "incorrectly pushed scope"

## Example Programs

### Empty Program

```
# Every executable program should define a "main" function.

main = fn () {}
```

### "Hello, world!"

```
# The main function can take global implicit parameters as arguments.
# These implicit parameters will be the default real-world implementations.

# NOTE: writeLine can't fail, and console is implicitly passed in

main = fn (console: Console [implicit]) {
    console.writeLine("Hello, world!")
}
```

Implementation notes:

* The grammar requires some lookahead, need to rework parser
* Need to rework scopes & symbol tables in the AST builder.
* Need to add "Console" to the global scope
    * Console is a collection of function impls
* Need to add real impl type of "Console" to the global scope
* Need to add implicit console variable to the global scope
    * Need to figure out how we can manually do this too
* Need to allow function arg number mismatch (due to implicits)
* Need to prove presence of implicit at compile time

### Binary Search

```
# Operates on a sorted collection. Therefore, needs proof of sorted collection.
# Proof can be an instance of a data type (ex: SortedList)
# Proof can separate (ex: (List, ProofSorted))

binarySearch = fn (t: Type, c: Collection(t), p: ProofSorted) {
    # How to ensure that p is a proof for c?
    ...
}

OR

binarySearch = fn (t: Type, c: SortedCollection(t)) {
    ...
}

# Note that if c is mutable, proofs are invalid after mutation in general.
# Could force immutability.
# OR, could void proofs after mutation in language
```













### Print a Random Number

```
main = fn (console: Console [implicit], rand: Rand [implicit]) {
    num = rand.randInt()
    console.writeLine("The random number is: " + num)
}
```

### Number Guessing Game

```
main = fn (console: Console [implicit], rand: Rand [implicit]) {
    num = rand.randInt()

    repeatUntilEq(processGuess, num)
    
    processGuess = fn (console: Console [implicit], num: Int [implicit]) {
        console.writeLine("What's your guess?")
        guess = console.readInt()
        
        case guess of
            guess == num: console.writeLine("Correct!")
            guess < num: console.writeLine("Your guess is too low")
            guess > num: console.writeLine("Your guess is too high")
        
        guess
    }
}
```

OR

```
main = fn (io: IIO [implicit]) {
    num = io.rand.randInt()

    repeatUntilEq(processGuess, num)
    
    processGuess = fn (io: IIO [implicit]) {
        io.console.writeLine("What's your guess?")
        guess = io.console.readInt()
        
        case guess of
            guess == num: io.console.writeLine("Correct!")
            guess < num: io.console.writeLine("Your guess is too low")
            guess > num: io.console.writeLine("Your guess is too high")
        
        guess
    }
}
```

OR

```
main: IIO () = do
    num = randInt()
    repeatUntilEq processGuess num

processGuess: IIO () = do
    writeLine("What's your guess?")
    guess = readInt()
    
    case guess of
        guess == num: writeLine("Correct!")
        guess < num: writeLine("Your guess is too low")
        guess > num: writeLine("Your guess is too high")
    
    guess
```

### Fibonacci

```
fib = fn (n: Nat): Nat =>
    case n of
        0: 0
        1: 1
        _: fib(n-1) + fib(n+1)
```

### Array Indexing

```
at = fn (t: Type [implicit], n: Nat [implicit], arr: Array(t, n), i: BoundedNat(n)): t => arr[i]
```

### Circular References

```
# Assume record fields are stored as inline data. Then these circular references can't be represented.
A = record
    b: B

B = record
    a: A

# If we use references, then we can represent it. However, I'm not sure how we could initialize instances of the records.
A = record
    b: &B

B = record
    a: &A

# We could use nullable references. But this may not be ideal.
A = record
    b: Option(&B)

B = record
    a: Option(&A)

# If we're using the option type, then we don't even have to use references.
A = record
    b: Option(B)

B = record
    a: Option(A)

# One solution is to implicitly use references, and allow references to the parent value when constructing.
A = record
    b: B

B = record
    a: A

a = A { b = B { a = parent } }

# Another complication is compiling the types.
```



### Other
identity = fn (x: 'a) -> x

OR

identity = fn (t: type, x: t) -> x



in_range = fn (x: nat, min: nat, max: nat {max >= min}) -> ...

OR

in_range = fn (x: nat, min: nat, max: nat, min_max_proof: proof(max >= min)) -> ...

