# rust-monkey-interpreter
A monkey interpreter written in rust - Based on the book "Writing An Interpreter In Go" by Thorsten Ball

This project is based on the book *Writing an Interpreter in Go* by Thorsten Ball. It implements an interpreter for a simple programming language called Monkey language.

This interpreter is written in Rust, a fast, memory-safe, and productive programming language.

## REPL
The REPL is started when running the repo:

`cargo run`


## Tests
All tests pass using:

`cargo test`


## Code example

```monkey
let name = "Monkey";
let age = 1;
let inspirations = ["Scheme", "Lisp", "JavaScript", "Clojure"];
let book = {
    "title": "Writing A Compiler In Go",
    "author": "Thorsten Ball",
    "prequel": "Writing An Interpreter In Go"
};

let printBookName = fn(book) {
    let title = book["title"];
    let author = book["author"];
    puts(author + " - " + title);
};
printBookName(book);
// => prints: "Thorsten Ball - Writing A Compiler In Go"


let fibonacci = fn(x) {
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    iter(arr, []);
};
let numbers = [1, 1 + 1, 4 - 1, 2 * 2, 2 + 3, 12 / 2];
map(numbers, fibonacci);
// => returns: [1, 1, 2, 3, 5, 8]
```


## Implemented Features
The interpreter supports:
* integers
* booleans
* strings
* arrays
* hashes
* prefix-, infix- and index operators
* conditionals
* global and local bindings
* first-class functions
* return statements
* closures