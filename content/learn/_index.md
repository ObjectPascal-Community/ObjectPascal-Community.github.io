+++
title = 'Learn Pascal'
date = 2024-05-30T18:56:05+03:00
draft = false
layout = 'single-with-sidebar'
+++

# Get Started in 30 Minutes! 🚀

Welcome to Pascal! This guide will get you writing your first Pascal programs quickly using Free Pascal. Pascal is perfect for learning programming because it's clear, structured, and teaches good habits.

## Quick Setup

### Option 1: Try Online (No Installation) ⚡

Start coding immediately with these online compilers:

- **[FPC Playground](https://fpc-playground-app-mgeib.ondigitalocean.app/)** - Best for learning
- **[OneCompiler](https://onecompiler.com/pascal)** - Simple and fast
- **[OnlineGDB](https://www.onlinegdb.com/online_pascal_compiler)** - Has debugging features

### Option 2: Install on Your Computer 💻

| Option | Best For | Download |
|--------|----------|----------|
| **Lazarus IDE** | Complete beginners, GUI apps | [Download Lazarus](https://www.lazarus-ide.org/) |
| **Free Pascal** | Command line, learning fundamentals | [Download FPC](https://www.freepascal.org/) |

**Recommendation**: Start with Lazarus - it includes everything you need!


## Your First Program - Hello World! 👋

Let's write your first Pascal program. Create a file called `hello.pas`:

```objectpascal {class="highlight capsule-universal"}
program HelloWorld;
begin
    writeln('Hello, Pascal!');
    writeln('Welcome to programming!');
    readln; // Wait for Enter key
end.
```

**How to run it:**

- **Online**: Copy-paste into any online compiler and click "Run"
- **Lazarus**: Create new program, replace code, press F9
- **Command line**: `fpc hello.pas` then `./hello` (Linux/Mac) or `hello.exe` (Windows)


## Essential Concepts 🧠

### Variables - Storing Information

Think of variables as labeled boxes that hold information:

```objectpascal {class="highlight capsule-universal"}
program Variables;
var
    name: string;        // Text
    age: integer;        // Whole numbers
    height: real;        // Decimal numbers
    isStudent: boolean;  // True or false
begin
    name := 'Alice';
    age := 20;
    height := 5.6;
    isStudent := true;
    
    writeln('Name: ', name);
    writeln('Age: ', age);
    writeln('Height: ', height:0:1, ' feet');
    writeln('Student: ', isStudent);
    
    readln;
end.
```

**Key Points:**

- Declare variables with `var`
- Assign values with `:=` (not `=`)
- Always end statements with `;`

### Making Decisions - If Statements

Programs need to make choices based on conditions:

```objectpascal {class="highlight capsule-universal"}
program Decisions;
var
    age: integer;
begin
    write('Enter your age: ');
    readln(age);
    
    if age >= 18 then
        writeln('You can vote!')
    else
        writeln('Too young to vote.');
        
    // Multiple conditions
    if age < 13 then
        writeln('You are a child')
    else if age < 20 then
        writeln('You are a teenager')
    else
        writeln('You are an adult');
        
    readln;
end.
```

### Repeating Actions - Loops

Loops let you repeat code multiple times:

```objectpascal {class="highlight capsule-fpc capsule-delphi"}
program Loops;
var
    i: integer;
begin
    // Count from 1 to 5
    writeln('Counting up:');
    for i := 1 to 5 do
        writeln('Number: ', i);
    
    // Count down from 5 to 1
    writeln('Counting down:');
    for i := 5 downto 1 do
        writeln('Number: ', i);
    
    // While loop - keep going while condition is true
    i := 1;
    writeln('While loop:');
    while i <= 3 do
    begin
        writeln('Step: ', i);
        i := i + 1;
    end;
    
    readln;
end.
```

## Practice Time! 🏆

Let's build some real programs to practice what you've learned.

### Exercise 1: Simple Calculator

```objectpascal {class="highlight capsule-universal"}
program Calculator;
var
    num1, num2: real;
    operation: char;
begin
    writeln('=== Simple Calculator ===');
    write('Enter first number: ');
    readln(num1);
    write('Enter operation (+, -, *, /): ');
    readln(operation);
    write('Enter second number: ');
    readln(num2);
    
    case operation of
        '+': writeln('Result: ', num1 + num2:0:2);
        '-': writeln('Result: ', num1 - num2:0:2);
        '*': writeln('Result: ', num1 * num2:0:2);
        '/': if num2 <> 0 then
                 writeln('Result: ', num1 / num2:0:2)
             else
                 writeln('Error: Cannot divide by zero!');
        else writeln('Error: Unknown operation!');
    end;
    
    readln;
end.
```

### Exercise 2: Number Guessing Game

```objectpascal {class="highlight capsule-universal"}
program GuessingGame;
var
    secretNumber, guess, attempts: integer;
begin
    writeln('=== Number Guessing Game ===');
    writeln('I''m thinking of a number between 1 and 10!');
    
    secretNumber := 7; // In real game, this would be random
    attempts := 0;
    
    repeat
        write('Enter your guess: ');
        readln(guess);
        attempts := attempts + 1;
        
        if guess < secretNumber then
            writeln('Too low! Try again.')
        else if guess > secretNumber then
            writeln('Too high! Try again.')
        else
            writeln('Congratulations! You got it in ', attempts, ' attempts!');
            
    until guess = secretNumber;
    
    readln;
end.
```

## Basic Debugging 🔍

When your program doesn't work as expected, try these simple debugging techniques:

### 1. Use `writeln` to Check Values

```objectpascal {class="highlight capsule-universal"}
program Debug;
var
    x, y, result: integer;
begin
    x := 10;
    y := 5;
    
    writeln('Debug: x = ', x, ', y = ', y);  // Check values
    
    result := x + y;
    writeln('Debug: result = ', result);     // Check calculation
    
    if result > 20 then
        writeln('Result is big')
    else
        writeln('Result is small');
        
    readln;
end.
```

### 2. Common Mistakes to Watch For

- **Assignment vs Comparison**: Use `:=` for assignment, `=` for comparison
- **Missing semicolons**: Every statement needs `;` except before `else` and `end`
- **Case sensitivity**: Pascal is not case-sensitive, but be consistent
- **Parentheses**: Make sure they match `()` and `begin`/`end` pairs

### 3. Reading Error Messages

When you get an error:
1. Look at the **line number** - that's where the problem is (or nearby)
2. Read the error message carefully
3. Common errors:
   - "Identifier not found" = you misspelled a variable name
   - "Type mismatch" = you're mixing different data types
   - "Syntax error" = you have a typo or missing punctuation

## What's Next? 🚀

Congratulations! You now know the basics of Pascal programming. Here's your learning path:

### 🎯 **Ready for More?**

Head over to our **[Documentation Page](/docs/)** for:
- Advanced programming concepts (arrays, records, functions)
- Object-oriented programming with classes
- File handling and database programming
- GUI development with Lazarus
- Professional development practices
- Implementation-specific features (Free Pascal, Delphi, etc.)

### 📚 **Additional Resources**

- [Free Pascal Documentation](https://www.freepascal.org/docs.html) - Official FPC docs
- [Lazarus Wiki](https://wiki.lazarus.freepascal.org/) - GUI development guide
- [Pascal Community Forum](https://forum.lazarus.freepascal.org/) - Get help and share projects

### 💡 **Keep Practicing**
The best way to learn programming is by doing! Try modifying the examples above, create your own small projects, and don't be afraid to experiment.

---

**Happy coding! Welcome to the Pascal community! 🎉**