// Phase 1 + 2 + 3 + 4 + 5: Complete Compiler Test
// Testing: Control flow, Error handling, Destructuring, Spread, Rest, Classes, Interfaces, Generics, Async/Await, Getters/Setters, Modules

// === PHASE 1: Control Flow ===
for (let i = 0; i < 5; i++) {
    console.log(i);
}

let j = 0;
while (j < 3) {
    j++;
}

let k = 0;
do {
    k++;
} while (k < 3);

let arr = [1, 2, 3];
for (let num of arr) {
    console.log(num);
}

let obj = { a: 1, b: 2 };
for (let key in obj) {
    console.log(key);
}

let x = 10;
if (x > 5) {
    console.log("big");
} else {
    console.log("small");
}

let y = x > 5 ? "big" : "small";

for (let i = 0; i < 10; i++) {
    if (i === 5) {
        break;
    }
}

for (let i = 0; i < 5; i++) {
    if (i === 2) {
        continue;
    }
    console.log(i);
}

let day = 1;
switch (day) {
    case 1:
        console.log("Monday");
        break;
    case 2:
        console.log("Tuesday");
        break;
    default:
        console.log("Other");
}

// === PHASE 2: Error Handling & Types ===
try {
    throw new Error("test");
} catch (e) {
    console.log("caught");
} finally {
    console.log("finally");
}

function divide(a, b) {
    if (b === 0) {
        throw new Error("Division by zero");
    }
    return a / b;
}

enum Color {
    Red = "red",
    Green = "green",
    Blue = "blue"
}
let favoriteColor = Color.Green;

let someValue = "hello";
let strLength = (someValue as string).length;

// === PHASE 3: Advanced Patterns ===

// Array destructuring
let arr2 = [1, 2, 3];
let [a, b, c] = arr2;

// Array destructuring with rest
let [first, ...rest] = arr2;

// Object destructuring
let obj2 = { name: "John", age: 30 };
let { name, age } = obj2;
