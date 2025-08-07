+++
title = 'Learn Pascal'
date = 2024-05-30T18:56:05+03:00
draft = false
layout = 'single-with-sidebar'
+++

## Quick Start with Free Pascal Compiler (FPC)

This quick start guide uses the Free Pascal Compiler (FPC), a free, open-source Pascal compiler that works across multiple platforms. FPC is a great choice for learning Pascal as it's widely used and well-documented.

### Installation

Choose your Pascal development environment:

| Feature | Free Pascal Compiler (FPC) | Lazarus IDE |
|---------|----------------------------|-------------|
| **Type** | Command-line compiler | Visual development environment |
| **Interface** | Terminal/command-line | Graphical IDE with forms designer |
| **Best for** | Learning fundamentals, scripting | GUI applications, beginners |
| **Includes** | Pascal compiler + Text-based IDE | FPC + IDE + visual designer |
| **Platform** | Cross-platform | Cross-platform |
| **Download** | [Download FPC](https://www.freepascal.org/) | [Download Lazarus](https://www.lazarus-ide.org/) |  

### Try Online (No Installation Required)

If you want to try Pascal without installing anything, you can use these online compilers (note: they have limited functionality compared to a full installation):

- [FPC Playground](https://fpc-playground-app-mgeib.ondigitalocean.app/) - Online Free Pascal Compiler
- [OneCompiler](https://onecompiler.com/pascal) - Online Pascal compiler with basic features
- [OnlineGDB](https://www.onlinegdb.com/online_pascal_compiler) - Online Pascal compiler with debugging


## Compiling Hello World!

Create a new file called `hello.pas`:

```objectpascal {class="highlight capsule-fpc"}
{$mode objfpc}{$H+}{$J-}

program HelloWorld;
begin
    writeln('Hello, Pascal!');
end.
```

Compile and run:

```bash
fpc hello.pas
./hello
```


## Core Concepts

### Program Structure

Every Pascal program follows this basic structure:

```objectpascal {class="highlight capsule-fpc"} {wrapper="highlight capsule-fpc"}
program ProgramName;

{$mode objfpc}{$H+}{$J-} // use this for modern Pascal features

{ Constant declarations }
const
  MAX_ITEMS = 100;
  GREETING = 'Welcome to Pascal';
  PI = 3.14159;

{ Type declarations }
type
  TStudent = record
      name: string;
      age: integer;
  end;
  TScoreArray = array[1..10] of integer;

{ Variable declarations }
var
  message: string;
  count: integer;
  student: TStudent;

{ Procedure and function declarations }
procedure displayInfo;
begin
    writeln('Program: ', GREETING);
end;

{ Main program block }
begin
  message := GREETING;
  count := 42;
  writeln(message);
  writeln('Count: ', count);
  displayInfo;
end.
```

### Data Types

Pascal has strong typing with these fundamental and practical types:

```objectpascal {class="highlight capsule-fpc"}
var
  // Integer types
  age: integer;              // -2,147,483,648 to 2,147,483,647
  population: longint;       // Same as integer
  fileSize: int64;           // Large numbers: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
  count: cardinal;           // Unsigned: 0 to 4,294,967,295
  index: word;               // Small positive: 0 to 65,535
  flags: byte;               // Tiny values: 0 to 255
  
  // Real/floating point types
  temperature: real;         // General purpose floating point
  price: double;             // High precision: banking, scientific
  percentage: single;        // Lower precision, memory efficient
  saving: currency;          // Fixed-point for money calculations
  
  // Text and character types
  grade: char;               // Single character
  name: string;              // Dynamic string (unlimited length)
  shortName: string[50];     // Fixed-length string (50 chars max)
  unicodeStr: UnicodeString; // Unicode support for international text
  
  // Boolean and logical
  isActive: boolean;         // True or false
  hasPermission: boolean;
  
  // Date and time types
  birthDate: TDateTime;      // Date and time combined
  startTime: TTime;          // Time only
  eventDate: TDate;          // Date only
  
  // Pointer and reference types
  dataPtr: pointer;          // Generic pointer
  objRef: TObject;           // Object reference
  
  // Variant types (dynamic typing)
  dynamicValue: variant;     // Can hold any type
  
  // Set types (collections of values)
  weekDays: set of (Monday, Tuesday, Wednesday, Thursday, Friday);
  validChars: set of char;   // Set of characters
```

### Control Structures

**Conditional Statements:**

```objectpascal {class="highlight capsule-fpc"}
if age >= 18 then
  writeln('You are an adult')
else
  writeln('You are a minor');

case grade of
  'A': writeln('Excellent!');
  'B': writeln('Good job!');
  'C': writeln('Average');
  else writeln('Keep trying!');
end;
```

**Loops:**

```objectpascal {class="highlight capsule-fpc"}
{ For loop - counting up }
for i := 1 to 10 do
  writeln('Number: ', i);

{ For loop - counting down }
for i := 10 downto 1 do
  writeln('Countdown: ', i);

{ For loop with arrays }
var
  numbers: array[1..5] of integer = (10, 20, 30, 40, 50);
  i: integer;
begin
  for i := 1 to 5 do
    writeln('Array[', i, '] = ', numbers[i]);
end;

{ While loop - condition checked first }
var
  count: integer = 5;
begin
  while count > 0 do
  begin
    writeln('Count: ', count);
    count := count - 1;
  end;
end;

{ Repeat-until loop - condition checked last }
var
  number: integer;
begin
  repeat
    write('Enter a positive number: ');
    readln(number);
  until number > 0;
  writeln('You entered: ', number);
end;

{ For-in loop (modern Pascal) }
var
  names: array of string = ('Alice', 'Bob', 'Charlie');
  name: string;
begin
  for name in names do
    writeln('Hello, ', name);
end;

{ Nested loops - multiplication table }
var
  i, j: integer;
begin
  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
      write(i * j:4);  // Format with width 4
    writeln;  // New line after each row
  end;
end;

{ Loop control statements }
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    if i = 5 then
      continue;  // Skip iteration when i = 5
    if i = 8 then
      break;     // Exit loop when i = 8
    writeln(i);
  end;
end;
```

## Functions and Procedures

Functions return values, procedures don't:

```objectpascal {class="highlight capsule-fpc"}
// Function - returns a value
function Add(a, b: Integer): Integer;
begin
  Result := a + b;  // or Add := a + b;
end;

// Procedure - performs an action
procedure PrintGreeting(const name: string);
begin
  writeln('Hello, ', name, '!');
end;

// Using them
var
  sum: Integer;
begin
  sum := Add(5, 3);           // sum = 8
  PrintGreeting('Pascal');    // prints: Hello, Pascal!
end.
```

## Practice Exercises

Try these exercises to reinforce what you've learned:

### Exercise 1: Basic Calculator
```objectpascal {class="highlight capsule-fpc"}
program Calculator;

{$mode objfpc}{$H+}{$J-} 

var
  num1, num2: real;
  operation: char;
begin
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
           writeln('Error: Division by zero!');
    else
      writeln('Invalid operation!');
  end;
end.
```

### Exercise 2: Number Guessing Game
```objectpascal {class="highlight capsule-fpc"}
program GuessNumber;

{$mode objfpc}{$H+}{$J-} 
var
  secretNumber, guess, attempts: Integer;
begin
  Randomize;  // Initialize random number generator
  secretNumber := Random(100) + 1;  // 1-100
  attempts := 0;
  
  writeln('Guess the number between 1 and 100!');
  
  repeat
    write('Enter your guess: ');
    readln(guess);
    Inc(attempts);
    
    if guess < secretNumber then
      writeln('Too low!')
    else if guess > secretNumber then
      writeln('Too high!')
    else
      writeln('Congratulations! You got it in ', attempts, ' attempts!');
  until guess = secretNumber;
end.
```

## Next Steps

Once you're comfortable with the basics above:

1. **Practice More**: Write small programs using what you've learned
2. **Learn Arrays**: Work with collections of data
3. **Master Records**: Group related data together  
4. **Try File I/O**: Read and write files
5. **Explore GUI**: Use Lazarus to create visual applications
6. **Join the Community**: Ask questions and share your progress

## Resources

- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
- [Lazarus Wiki](https://wiki.lazarus.freepascal.org/)
- [Pascal Programming Examples](/docs/)
- [Community Forum](/community/)

---

## Advanced Topics

The following sections are for those ready to dive deeper into Pascal programming.


### Arrays and Data Structures

**Static Arrays:**
```objectpascal {class="highlight capsule-fpc"}
program ArrayExample;

{$mode objfpc}{$H+}{$J-} 

var
  scores: array[1..5] of integer;
  names: array[1..3] of string;
  i: integer;
begin
  { Initialize array }
  scores[1] := 95;
  scores[2] := 87;
  scores[3] := 92;
    
  names[1] := 'Alice';
  names[2] := 'Bob';
  names[3] := 'Charlie';
    
  { Display results }
  for i := 1 to 3 do
    writeln(names[i], ': ', scores[i]);
end.
```

**Records (Structures):**
```objectpascal {class="highlight capsule-fpc"}
program RecordExample;

{$mode objfpc}{$H+}{$J-} 

type
  TStudent = record
    name: string;
    age: integer;
    grade: char;
    gpa: real;
  end;

var
  student: TStudent;
begin
  { Initialize record }
  student.name := 'Emma Watson';
  student.age := 20;
  student.grade := 'A';
  student.gpa := 3.8;
    
  { Display student info }
  writeln('Student: ', student.name);
  writeln('Age: ', student.age);
  writeln('Grade: ', student.grade);
  writeln('GPA: ', student.gpa:0:1);
end.
```

### File Input/Output

#### Reading from Files

```objectpascal {class="highlight capsule-fpc"}
program ReadFile;

{$mode objfpc}{$H+}{$J-} 

uses
  Classes, SysUtils, streamex;

var
  reader: TStreamReader;
  fileStream: TFileStream;
  line, filename:string;
  i: integer;
begin
  // filename to read
  filename:= 'cake-ipsum-.txt';
  try
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      reader := TStreamReader.Create(fileStream);
      try
        // Set line counter to 1
        i := 1;
        while not reader.EOF do
        begin
          line := reader.ReadLine;
          WriteLn(Format('line %d is: %s', [i, line]));
          i := i + 1;
        end;
      finally
        reader.Free;
      end;
    finally
      fileStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;

  // Pause console
  ReadLn;
end.
```

#### Writing to Files
```objectpascal {class="highlight capsule-fpc"}
program AppendTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils;

var
  filename: string = 'log.txt';
  fileStream: TFileStream;
  size: int64;
  newText: string;

begin

  // First, does the file exist?
  if FileExists(fileName) then
    // If yes, open the file in append mode.
    fileStream := TFileStream.Create(filename, fmOpenWrite or fmShareDenyNone)
  else
    // If not, create a a new file.
    fileStream := TFileStream.Create(filename, fmCreate);

  // Next, start appending.
  try
    // set position at the end of the file
    fileStream.Seek(0, soFromEnd);
    // Write text into the file
    newText := LineEnding + 'A new line!';
    size := fileStream.Write(newText[1], Length(newText));
    // Show confirmation
    Writeln(Format('Appended %s. %d bytes written.', [filename, size]));
  finally
    // Free TFileStream object
    fileStream.Free;
  end;

  // Pause console
  WriteLn('Press Enter to quit.');
  ReadLn;
end.
```

---

## Debugging & Troubleshooting Guide

### Getting Started with Debugging

When your program doesn't work as expected, follow this step-by-step approach:

#### Step 1: Read the Error Message Carefully
- Compile errors usually tell you the exact line number and what's wrong
- Don't ignore warnings - they often point to real problems
- Take note of the error type (syntax error, runtime error, etc.)

#### Step 2: Use WriteLn to See What's Happening

The simplest and most effective debugging technique for beginners:

```objectpascal {class="highlight capsule-fpc"}
program DebugExample;

{$mode objfpc}{$H+}{$J-} 

var
  x, y, result: Integer;
begin
  x := 10;
  y := 5;
  
  writeln('Before calculation: x=', x, ', y=', y);
  result := x * y;
  writeln('After calculation: result=', result);
  
  if result > 40 then
    writeln('Result is greater than 40')
  else
    writeln('Result is 40 or less');
end.
```

#### Step 3: Check Your Assumptions

Always validate what you think should be happening:

```objectpascal {class="highlight capsule-fpc"}
// Always validate input
function SafeDivide(a, b: Real): Real;
begin
  writeln('Dividing ', a:0:2, ' by ', b:0:2);
  
  if b = 0 then
  begin
    writeln('Error: Cannot divide by zero!');
    Result := 0;
  end
  else
  begin
    Result := a / b;
    writeln('Result: ', Result:0:2);
  end;
end;
```

#### Step 4: Debugging Different Scenarios

**Array Operations:**
```objectpascal {class="highlight capsule-fpc"}
// Good - safe with bounds checking
if (i >= 0) and (i < Length(myArray)) then
begin
  writeln('Setting myArray[', i, '] = ', value);
  myArray[i] := value;
end
else
  writeln('Array index error: i = ', i, ', array length = ', Length(myArray));
```

**Debugging Loops:**
```objectpascal {class="highlight capsule-fpc"}
for i := 0 to High(myArray) do
begin
  writeln('Processing item ', i, ': ', myArray[i]);  // Shows progress
  // Your code here
end;
```

**File Operations:**
```objectpascal {class="highlight capsule-fpc"}
if not FileExists(filename) then
begin
  writeln('File not found: ', filename);
  Exit;
end;

writeln('Opening file: ', filename);  // Confirm file is being processed
// File operations here
```

### Common Beginner Mistakes & Solutions

Here are the most frequent issues newcomers encounter:

#### Missing Semicolons
```objectpascal {class="highlight capsule-fpc"}
// Problem: Missing semicolons
begin
  writeln('Hello')     // Need semicolon here
  writeln('World')     // And here
end.                   // Period ends the program

// Solution: Add semicolons after statements
begin
  writeln('Hello');    // Correct
  writeln('World');    // Correct
end.
```

#### Array Index Errors
```objectpascal {class="highlight capsule-fpc"}
// Problem: Wrong array bounds
var
  numbers: array[1..5] of Integer;  // Arrays are 1-indexed by default
  i: Integer;
begin
  for i := 0 to 5 do  // Wrong! Should start at 1, end at 5
    numbers[i] := i * 2;
end;

// Solution: Use correct bounds
var
  numbers: array[1..5] of Integer;
  i: Integer;
begin
  for i := 1 to 5 do  // Correct
    numbers[i] := i * 2;
  
  // Or use High() function for dynamic arrays
  for i := 0 to High(numbers) do
    numbers[i] := i * 2;
end;
```

#### Uninitialized Variables
```objectpascal {class="highlight capsule-fpc"}
// Problem: Forgetting to initialize variables
var
  count: Integer;
begin
  writeln('count: ', count); // Bad: count has undefined value
  // ... rest of code
end;

// Solution: Always initialize variables
var
  count: Integer;
begin
  count := 0;  // Good: explicitly set initial value
  writeln('count: ', count);
  // ... rest of code
end;
```

#### Case Sensitivity Confusion
Pascal isn't case-sensitive, but being consistent helps readability:
```objectpascal {class="highlight capsule-fpc"}
// These are all the same to Pascal:
WriteLn('Hello');
writeln('Hello');
WRITELN('Hello');

// But pick one style and stick with it
writeln('Hello');  // Most common convention
```

### Setting Up for Debugging

#### Compiler Flags for Debugging

Essential compiler flags when compiling from command line:

```bash
# Basic debugging
fpc -g myprogram.pas           # Add debug info
fpc -gl myprogram.pas          # Line info for backtraces
fpc -gh myprogram.pas          # Memory leak detection

# Runtime checks  
fpc -Cr myprogram.pas          # Range checking
fpc -Co myprogram.pas          # Object method checking

# Combine multiple flags
fpc -g -gl -Cr -Co myprogram.pas
```

#### Using Lazarus IDE

**For Lazarus Users**: When you create a project in Lazarus and use the default **Debug** build profile, Lazarus automatically includes the necessary debugging flags (`-g`, `-gl`, `-Cr`, etc.). You don't need to add them manually - just make sure you're using the **Debug** profile instead of **Release** when developing and testing your programs.

To switch build modes in Lazarus:
1. Go to **Project** → **Project Options**
2. Select **Compiler Options** → **Config and Target**
3. Choose **Debug** from the build mode dropdown

### Intermediate Debugging Techniques

#### Using Assertions

Assertions help catch problems early in development:

```objectpascal {class="highlight capsule-fpc"}
program AssertExample;

{$mode objfpc}{$H+}{$J-}
{$C+}  // Enable assertions

uses sysutils;

// Simple function that uses assertions
function SafeDivide(a, b: Real): Real;
begin
  Assert(b <> 0, 'Division by zero is not allowed!');
  Result := a / b;
end;

// Function with array bounds checking
function GetArrayElement(arr: array of Integer; index: Integer): Integer;
begin
  Assert((index >= 0) and (index <= High(arr)), 
    Format('Index %d is out of bounds (0-%d)', [index, High(arr)]));
  Result := arr[index];
end;

var
  numbers: array[0..4] of Integer = (10, 20, 30, 40, 50);
  result: Real;
begin
  writeln('Testing assertions...');
  
  // This will work fine
  result := SafeDivide(10, 2);
  writeln('10 / 2 = ', result:0:2);
  
  // This will work fine
  writeln('numbers[2] = ', GetArrayElement(numbers, 2));
  
  // Uncomment these lines to see assertion failures:
  // result := SafeDivide(10, 0);  // Will trigger assertion
  // writeln('numbers[10] = ', GetArrayElement(numbers, 10));  // Will trigger assertion
  
  writeln('All tests passed!');
end.
```

#### Conditional Compilation for Debugging

Use compiler directives to include debug code only when needed:

```objectpascal {class="highlight capsule-fpc"}
{$IFDEF DEBUG}
  // Debug-specific code
  {$DEFINE ENABLE_LOGGING}
  const DEBUG_MODE = True;
{$ELSE}
  // Release configuration
  const DEBUG_MODE = False;
{$ENDIF}

procedure DebugLog(const msg: string);
begin
  {$IFDEF ENABLE_LOGGING}
  writeln('[DEBUG] ', msg);
  {$ENDIF}
end;
```

#### Memory Leak Detection

Find memory leaks in your programs:

```bash
# Compile with heap tracing
fpc -gh myprogram.pas

# Run your program (creates heap.trc file)
./myprogram

# Check the heap.trc file for leaks
cat heap.trc
```

### Advanced Debugging

#### Exception Handling

For more robust error handling:

```objectpascal {class="highlight capsule-fpc"}
uses sysutils;

try
  // Potentially problematic code
  result := ProcessData(input);
except
  on E: EDivByZero do
    writeln('Division by zero error: ', E.Message);
    
  on E: EAccessViolation do
  begin
    writeln('Memory access violation:');
    DumpExceptionBackTrace(Output);
  end;
    
  on E: Exception do
    writeln('Error (', E.ClassName, '): ', E.Message);
    
  else
    writeln('Unknown error occurred');
end;
```

#### Using the Debugger

When WriteLn debugging isn't enough:

- **Set breakpoints** to pause execution at specific lines
- **Step through code** line by line to see exactly what happens
- **Watch variables** to see how values change over time
- **Examine the call stack** to understand how you got to the current point

#### Performance Debugging

For timing critical sections:
```objectpascal {class="highlight capsule-fpc"}
uses SysUtils;

var
  StartTime, EndTime: TDateTime;
begin
  StartTime := Now;
  // Your code here
  EndTime := Now;
  writeln('Execution time: ', FormatDateTime('hh:nn:ss.zzz', EndTime - StartTime));
end;
```



### Debugging Checklist & Getting Help

#### Quick Debugging Checklist

**For Beginners:**

1. [ ] **Read error messages carefully** - they usually point to the exact problem
2. [ ] **Add writeln statements** around problem areas to see what's happening
3. [ ] **Check common mistakes** - semicolons, array bounds, initialization
4. [ ] **Test with simple values** first, then try edge cases
5. [ ] **Ask for help** - community forums and Discord

**For Advanced Users:**

6. [ ] **Compile with debug flags**: `-g -gl -Cr` (or use Lazarus Debug profile)
7. [ ] **Use the debugger** to step through code line by line
8. [ ] **Check for memory leaks** with `-gh` flag and heap.trc file
9. [ ] **Write simple tests** for your functions
10. [ ] **Handle exceptions** properly with try/except blocks

#### Common Error Types & Quick Fixes

- **Compile errors**: Usually syntax problems - check semicolons, brackets, keywords
- **Runtime errors**: Use assertions and exception handling to catch issues early
- **Logic errors**: Add debug output and use the debugger to trace execution
- **Memory issues**: Compile with `-gh` and check heap.trc for leaks
- **Performance issues**: Time critical sections to identify bottlenecks

---

## Getting Help

When you're stuck, here's where to get help:

- **Community Forums**: [Free Pascal Forum](https://forum.lazarus.freepascal.org/)
- **Discord**: Join the [Unofficial Pascal Programming Discord Server](https://discord.com/channels/570025060312547359/570025060312547361)
- **Documentation**: [Free Pascal Docs](https://www.freepascal.org/docs.html)
- **Stack Overflow**: Tag your questions with `pascal`, `free-pascal` or `lazarus`

**When asking for help**, include:

- Your complete code (or minimal example that reproduces the problem)
- Exact error messages
- What you expected to happen vs. what actually happened
- Your Free Pascal/Lazarus version and operating system