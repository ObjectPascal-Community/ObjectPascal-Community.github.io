+++
title = 'Learn Pascal'
date = 2024-05-30T18:56:05+03:00
draft = false
layout = 'single-with-sidebar'
+++

**Get Started in Quickly using Free Pascal!**

Welcome to Pascal! This guide will get you writing your first Pascal programs quickly using Free Pascal.

---

## Quick Setup

### Option 1: Try Online (No Installation)

Start coding immediately with these online compilers:

- **[OneCompiler](https://onecompiler.com/pascal)** - Simple and fast
- **[OnlineGDB](https://www.onlinegdb.com/online_pascal_compiler)** - Has debugging features
- **[FPC Playground](https://fpc-playground-app-mgeib.ondigitalocean.app/)** - Best for simple codes

### Option 2: Install on Your Computer

| Option | Best For | Download |
|--------|----------|----------|
| **Lazarus IDE** | Complete beginners, GUI apps | [Download Lazarus](https://www.lazarus-ide.org/) |
| **Free Pascal** | Command line, learning fundamentals | [Download FPC](https://www.freepascal.org/) |

**Recommendation**: Start with Lazarus - it includes everything you need!

---

## Your First Program - Hello World!

Let's write your first Pascal program. Create a file called `hello.pas`:

```objectpascal {class="highlight capsule-universal"}
program HelloWorld;

{$mode objfpc}{$H+}{$J-}

begin
    writeln('Hello, Pascal!');
    readln; // Wait for Enter key
end.
```

**How to run it:**

- **Online**: Copy-paste into any online compiler and click "Run"
- **Lazarus**: Create new program, replace code, press F9
- **Command line**: `fpc hello.pas` then `./hello` (Linux/Mac) or `hello.exe` (Windows)

---

## Essential Concepts

### Variables - Storing Information

Think of variables as labeled boxes that hold information:

```objectpascal {class="highlight capsule-fpc"}
program Variables;

{$mode objfpc}{$H+}{$J-}

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

- `{$mode objfpc}{$H+}{$J-}` enable modern Object Pascal features in Free Pascal
    - [`{$mode objfpc}`](https://wiki.freepascal.org/Mode_ObjFPC) - Enable exceptions, classes, interfaces, overloading, etc
    - [`{$H+}`](https://wiki.freepascal.org/$H) - Enable long string
    - [`{$J-}`](https://www.freepascal.org/docs-html/current/prog/progsu42.html) - Disable writing to constants
- Declare variables with `var`
- Assign values with `:=` (not `=`)
- Always end statements with `;`

### Making Decisions - If Statements

Programs need to make choices based on conditions:

```objectpascal {class="highlight capsule-fpc"}
program Decisions;

{$mode objfpc}{$H+}{$J-}

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

```objectpascal {class="highlight capsule-fpc"}
program Loops;

{$mode objfpc}{$H+}{$J-}

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

---


## Practice Time!

Let's build some real programs to practice what you've learned.

### Exercise 1: Simple Calculator

```objectpascal {class="highlight capsule-fpc"}
program Calculator;

{$mode objfpc}{$H+}{$J-}

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
    
end.
```

### Exercise 2: Number Guessing Game

```objectpascal {class="highlight capsule-fpc"}
program GuessingGame;

{$mode objfpc}{$H+}{$J-}

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
    
end.
```

---

## Basic Debugging

When your program doesn't work as expected, try these simple debugging techniques:

### 1. Use `writeln` to Check Values

```objectpascal {class="highlight capsule-fpc"}
program Debug;

{$mode objfpc}{$H+}{$J-}

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

---

## Working with Data Structures

As your programs grow, you'll need better ways to organize data. Pascal provides several powerful tools for this.

### Records - Grouping Related Data

Records let you group related information together:

```objectpascal {class="highlight capsule-fpc"}
program RecordsDemo;

{$mode objfpc}{$H+}{$J-}

type
    TPerson = record
        name: string;
        age: integer;
        email: string;
    end;

var
    student: TPerson;
begin
    // Set values
    student.name := 'Alice';
    student.age := 20;
    student.email := 'alice@example.com';
    
    // Display information
    writeln('Student Info:');
    writeln('Name: ', student.name);
    writeln('Age: ', student.age);
    writeln('Email: ', student.email);
end.
```

### Classes - Objects with Behavior

Classes combine data and methods (functions) that work on that data:

```objectpascal {class="highlight capsule-fpc"}
program ClassesDemo;

{$mode objfpc}{$H+}{$J-}

type
    TBankAccount = class
    private
        FBalance: real;
    public
        constructor Create(InitialBalance: real);
        procedure Deposit(Amount: real);
        procedure Withdraw(Amount: real);
        function GetBalance: real;
    end;

constructor TBankAccount.Create(InitialBalance: real);
begin
    FBalance := InitialBalance;
end;

procedure TBankAccount.Deposit(Amount: real);
begin
    if Amount > 0 then
        FBalance := FBalance + Amount;
end;

procedure TBankAccount.Withdraw(Amount: real);
begin
    if (Amount > 0) and (Amount <= FBalance) then
        FBalance := FBalance - Amount;
end;

function TBankAccount.GetBalance: real;
begin
    Result := FBalance;
end;

var
    account: TBankAccount;
begin
    account := TBankAccount.Create(100.0);
    try
        writeln('Initial balance: $', account.GetBalance:0:2);
        
        account.Deposit(50.0);
        writeln('After deposit: $', account.GetBalance:0:2);
        
        account.Withdraw(25.0);
        writeln('After withdrawal: $', account.GetBalance:0:2);
    finally
        account.Free;
    end;
end.
```

### Interfaces - Contracts for Classes

Interfaces define what methods a class must implement:

```objectpascal {class="highlight capsule-fpc"}
program InterfacesDemo;

{$mode objfpc}{$H+}{$J-}

type
    ISpeaker = interface
        procedure Speak;
    end;
    
    TPerson = class(TInterfacedObject, ISpeaker)
    private
        FName: string;
    public
        constructor Create(AName: string);
        procedure Speak;
    end;
    
    TRobot = class(TInterfacedObject, ISpeaker)
    public
        procedure Speak;
    end;

constructor TPerson.Create(AName: string);
begin
    FName := AName;
end;

procedure TPerson.Speak;
begin
    writeln('Hello, I am ', FName);
end;

procedure TRobot.Speak;
begin
    writeln('BEEP BOOP - ROBOT SPEAKING');
end;

var
    speakers: array[1..2] of ISpeaker;
begin
    speakers[1] := TPerson.Create('Alice');
    speakers[2] := TRobot.Create;
    
    speakers[1].Speak;  // Hello, I am Alice
    speakers[2].Speak;  // BEEP BOOP - ROBOT SPEAKING
end.
```

### Advanced Records - Records with Methods

Modern Pascal allows records to have methods like classes:

```objectpascal {class="highlight capsule-fpc"}
program AdvancedRecordsDemo;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

uses
  SysUtils;

type
    TPoint = record
        X, Y: real;
        procedure SetCoords(AX, AY: real);
        function DistanceFromOrigin: real;
        function ToString: string;
    end;

procedure TPoint.SetCoords(AX, AY: real);
begin
    X := AX;
    Y := AY;
end;

function TPoint.DistanceFromOrigin: real;
begin
    Result := sqrt(X * X + Y * Y);
end;

function TPoint.ToString: string;
begin
    Result := '(' + FloatToStr(X) + ', ' + FloatToStr(Y) + ')';
end;

var
    point1, point2: TPoint;
begin
    // Using methods on records
    point1.SetCoords(3.0, 4.0);
    writeln('Point 1: ', point1.ToString);
    writeln('Distance from origin: ', point1.DistanceFromOrigin:0:2);
    
    // Direct field access still works
    point2.X := 1.0;
    point2.Y := 2.0;
    writeln('Point 2: ', point2.ToString);
    writeln('Distance from origin: ', point2.DistanceFromOrigin:0:2);

end.
```

**Key Points:**

- **Records**: Simple data containers, good for grouping related values
- **Classes**: Full object-oriented programming with inheritance and polymorphism
- **Interfaces**: Define contracts that classes must follow
- [**Advanced Records**](https://wiki.freepascal.org/modeswitch): Combine the simplicity of records with some class features

---

## What's Next?

Congratulations! You now know the basics of Pascal programming. Here's your learning path:

### Ready for More?

Head over to our **[Resources Page](/resources/)** for:

- Advanced programming concepts (arrays, records, functions)
- Object-oriented programming with classes
- File handling and database programming
- GUI development with Lazarus
- Professional development practices
- Implementation-specific features (Free Pascal, Delphi, etc.)

### Additional Resources

- [Free Pascal Documentation](https://www.freepascal.org/docs.html) - Official FPC docs
- [Lazarus Wiki](https://wiki.lazarus.freepascal.org/) - GUI development guide
- [Pascal Community Forum](https://forum.lazarus.freepascal.org/) - Get help and share projects


---

**Happy coding! Welcome to the Pascal community! 🎉**