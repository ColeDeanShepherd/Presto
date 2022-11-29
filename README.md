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
    writeLine("Hello, world!")
}
```

### Print a Random Number

```
main = fn (console: Console [implicit], rand: Rand [implicit]) {
    num = randInt()
    writeLine("The random number is: " + num)
}
```

### Number Guessing Game

```
main = fn (console: Console [implicit], rand: Rand [implicit]) {
    num = randInt()

    loop {
        writeLine("What's your guess?")
        guess = readInt()
        
        case guess of
            guess == num: {
                writeLine("Correct!")
                break
            }
            guess < num: writeLine("Your guess is too low")
            guess > num: writeLine("Your guess is too high")
    }
}
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
at = fn (t: Type [implicit], n: Nat, arr: Array(t, n), i: BoundedNat(n)): t => arr[i]
```