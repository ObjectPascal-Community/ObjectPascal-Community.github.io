+++
title = 'Learn Pascal'
date = 2025-08-22T08:50:00+10:00
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

```objectpascal {class="highlight capsule-fpc"}
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
program Controls;

{$mode objfpc}{$H+}{$J-}

var
    score: integer;
    grade: char;
begin
    write('Enter your score (0-100): ');
    readln(score);
    
    // If-then-else statement
    if score >= 90 then
        grade := 'A'
    else if score >= 80 then
        grade := 'B'
    else if score >= 70 then
        grade := 'C'
    else if score >= 60 then
        grade := 'D'
    else
        grade := 'F';
        
    writeln('Your grade is: ', grade);
    
    // Case statement for multiple choices
    case grade of
        'A': writeln('Excellent!');
        'B': writeln('Good job!');
        'C': writeln('Average');
        'D': writeln('Below average');
        'F': writeln('Failed');
    end;
end.
```

### Repeating Actions - Loops

Loops let you repeat code multiple times:

```objectpascal {class="highlight capsule-fpc"}
program LoopsDemo;

{$mode objfpc}{$H+}{$J-}

var
    i, sum: integer;
    numbers: array[1..5] of integer;
    num: integer;
begin
    // For loop - count from 1 to 5
    writeln('For loop:');
    for i := 1 to 5 do
        writeln('Number: ', i);
    
    // For-in loop with array
    numbers[1] := 10; numbers[2] := 20; numbers[3] := 30;
    numbers[4] := 40; numbers[5] := 50;
    
    writeln('For-in loop:');
    for num in numbers do
        writeln('Value: ', num);
    
    // While loop - sum numbers until we reach 100
    sum := 0;
    i := 1;
    writeln('While loop (sum until >= 100):');
    while sum < 100 do
    begin
        sum := sum + i;
        writeln('Adding ', i, ', sum = ', sum);
        i := i + 1;
    end;
    
    // Repeat-until loop - ask for positive number
    writeln('Repeat-until loop:');
    repeat
        write('Enter a positive number: ');
        readln(num);
        if num <= 0 then
            writeln('Please enter a positive number!');
    until num > 0;
    writeln('Thank you! You entered: ', num);
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

### Arrays - Static and Dynamic

Arrays store multiple values of the same type:

```objectpascal {class="highlight capsule-fpc"}
program ArraysDemo;

{$mode objfpc}{$H+}{$J-}

var
    // Static array - fixed size
    staticNumbers: array[1..5] of integer;
    
    // Dynamic array - size can change
    dynamicNumbers: array of integer;
    
    i: integer;
begin
    // Working with static arrays
    writeln('Static Array:');
    staticNumbers[1] := 10;
    staticNumbers[2] := 20;
    staticNumbers[3] := 30;
    staticNumbers[4] := 40;
    staticNumbers[5] := 50;
    
    for i := 1 to 5 do
        writeln('staticNumbers[', i, '] = ', staticNumbers[i]);
    
    // Working with dynamic arrays
    writeln('Dynamic Array:');
    SetLength(dynamicNumbers, 3);  // Set size to 3 elements
    
    dynamicNumbers[0] := 100;  // Dynamic arrays start at index 0
    dynamicNumbers[1] := 200;
    dynamicNumbers[2] := 300;
    
    for i := 0 to High(dynamicNumbers) do
        writeln('dynamicNumbers[', i, '] = ', dynamicNumbers[i]);
    
    // Resize dynamic array
    SetLength(dynamicNumbers, 5);
    dynamicNumbers[3] := 400;
    dynamicNumbers[4] := 500;
    
    writeln('After resize:');
    for i := 0 to High(dynamicNumbers) do
        writeln('dynamicNumbers[', i, '] = ', dynamicNumbers[i]);
end.
```

**Key Points:**

- Use `SetLength` to resize dynamic arrays
- Use `High` to get the highest index of an array

### Generics Collections

FPC 3.2.2 includes generic collections for flexible data storage:

```objectpascal {class="highlight capsule-fpc"}
program CollectionsDemo;

{$mode objfpc}{$H+}{$J-}

uses
    Generics.Collections;

type
    TStringList = specialize TList<string>;
    TIntList = specialize TList<integer>;
    TStrIntDictionary = specialize TDictionary<string, integer>;

var
    stringList: TStringList;
    numberList: TIntList;
    dictionary: TStrIntDictionary;
    i: integer;
    key: string;
begin
    // Generic List of strings
    stringList := TStringList.Create;
    try
        stringList.Add('Apple');
        stringList.Add('Banana');
        stringList.Add('Cherry');
        
        writeln('String List:');
        for i := 0 to stringList.Count - 1 do
            writeln(i + 1, '. ', stringList[i]);
    finally
        stringList.Free;
    end;
    
    // Generic List of integers
    numberList := TIntList.Create;
    try
        numberList.Add(10);
        numberList.Add(20);
        numberList.Add(30);
        
        writeln('Number List:');
        for i := 0 to numberList.Count - 1 do
            writeln('Item ', i, ': ', numberList[i]);
    finally
        numberList.Free;
    end;
    
    // Dictionary (key-value pairs)
    dictionary := TStrIntDictionary.Create;
    try
        dictionary.Add('Alice', 25);
        dictionary.Add('Bob', 30);
        dictionary.Add('Charlie', 35);
        
        writeln('Dictionary:');
        for key in dictionary.Keys do
            writeln(key, ' is ', dictionary[key], ' years old');
    finally
        dictionary.Free;
    end;
end.
```

**Key Points:**

- Pair `try`/`finally` to ensure resources are cleaned up
- Create objects using `Create`
- Use `Free` to release resources

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