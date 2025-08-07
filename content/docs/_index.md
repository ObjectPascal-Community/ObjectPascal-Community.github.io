+++
title = 'Documentation'
date = 2024-05-30T18:56:05+03:00
draft = false
layout = 'single-with-sidebar'
+++

Comprehensive guides and references for Pascal programming across all implementations.

## 1. Core Pascal Language (Universal)

These features work across all Pascal implementations including Free Pascal, Delphi, PascalABC.NET, and others.

### Basic Syntax and Structure

Pascal uses a clear, structured syntax that emphasizes readability:

```objectpascal {class="highlight capsule-universal"}
program HelloWorld;
begin
    writeln('Hello, Pascal World!');
end.
```

### Program Structure

Every Pascal program follows this basic structure:

```objectpascal {class="highlight capsule-universal"}
program ProgramName;

{ Optional: Uses clause for including units }
uses
    SysUtils;

{ Optional: Constant declarations }
const
    MAX_SIZE = 100;
    PI = 3.14159;

{ Optional: Type declarations }
type
    TIntArray = array[1..MAX_SIZE] of integer;
    TPoint = record
        x, y: real;
    end;

{ Optional: Variable declarations }
var
    numbers: TIntArray;
    point: TPoint;
    i: integer;

{ Main program block }
begin
    { Program statements go here }
    writeln('Program executed successfully');
end.
```

### Standard Data Types

**Basic Types (Universal):**
- `integer`: Whole numbers
- `real`: Floating-point numbers
- `char`: Single character
- `boolean`: True or false values
- `string`: Text strings (implementation may vary)

```objectpascal {class="highlight capsule-universal"}
var
    count: integer;
    temperature: real;
    initial: char;
    isValid: boolean;
    name: string;
begin
    count := 42;
    temperature := 98.6;
    initial := 'A';
    isValid := true;
    name := 'Pascal';
end.
```

**Structured Types:**

```objectpascal {class="highlight capsule-universal"}
{ Arrays }
type
    TScores = array[1..10] of integer;
    TMatrix = array[1..3, 1..3] of real;

{ Records }
type
    TPerson = record
        name: string;
        age: integer;
        salary: real;
    end;

{ Sets }
type
    TDigits = set of 0..9;
    TLetters = set of 'A'..'Z';
```

### Control Structures

**Conditional Statements:**

```objectpascal {class="highlight capsule-universal"}
program ConditionalExample;
var
    age: integer;
begin
    write('Enter your age: ');
    readln(age);
    
    if age >= 18 then
        writeln('You are an adult')
    else
        writeln('You are a minor');
    
    { Case statement }
    case age of
        0..12: writeln('Child');
        13..19: writeln('Teenager');
        20..64: writeln('Adult');
        else writeln('Senior');
    end;
end.
```

**Loops:**

```objectpascal {class="highlight capsule-universal"}
program LoopExamples;
var
    i, sum: integer;
begin
    { For loop }
    sum := 0;
    for i := 1 to 10 do
        sum := sum + i;
    writeln('Sum 1-10: ', sum);
    
    { While loop }
    i := 1;
    while i <= 5 do
    begin
        writeln('Count: ', i);
        i := i + 1;
    end;
    
    { Repeat-until loop }
    i := 1;
    repeat
        writeln('Repeat: ', i);
        i := i + 1;
    until i > 3;
end.
```

### Procedures and Functions

```objectpascal {class="highlight capsule-universal"}
program ProcedureFunctionExample;

{ Procedure - performs action, no return value }
procedure PrintMessage(msg: string);
begin
    writeln('Message: ', msg);
end;

{ Function - returns a value }
function Add(a, b: integer): integer;
begin
    Add := a + b;  { or Result := a + b; }
end;

{ Function with multiple parameters }
function CalculateArea(length, width: real): real;
begin
    CalculateArea := length * width;
end;

var
    result: integer;
    area: real;
begin
    PrintMessage('Hello from procedure!');
    
    result := Add(5, 3);
    writeln('5 + 3 = ', result);
    
    area := CalculateArea(10.5, 8.2);
    writeln('Area: ', area:0:2);
end.
```

## 2. Object Pascal Extensions (Widely Supported)

These features are supported by most modern Pascal implementations including Delphi, Free Pascal, and others.

### Classes and Objects

```objectpascal {class="highlight capsule-universal"}
program BasicOOP;

type
    { Simple class definition }
    TCounter = class
    private
        FValue: integer;
    public
        constructor Create;
        procedure Increment;
        procedure Decrement;
        function GetValue: integer;
    end;

{ Constructor }
constructor TCounter.Create;
begin
    FValue := 0;
end;

{ Methods }
procedure TCounter.Increment;
begin
    FValue := FValue + 1;
end;

procedure TCounter.Decrement;
begin
    FValue := FValue - 1;
end;

function TCounter.GetValue: integer;
begin
    GetValue := FValue;
end;

{ Main program }
var
    counter: TCounter;
begin
    counter := TCounter.Create;
    try
        counter.Increment;
        counter.Increment;
        writeln('Counter value: ', counter.GetValue);
    finally
        counter.Free;
    end;
end.
```

### Inheritance and Polymorphism

```objectpascal {class="highlight capsule-universal"}
program InheritanceExample;

type
    { Base class }
    TShape = class
    private
        FColor: string;
    public
        constructor Create(AColor: string);
        function GetArea: real; virtual; abstract;
        property Color: string read FColor write FColor;
    end;
    
    { Derived class }
    TCircle = class(TShape)
    private
        FRadius: real;
    public
        constructor Create(AColor: string; ARadius: real);
        function GetArea: real; override;
    end;

{ TShape implementation }
constructor TShape.Create(AColor: string);
begin
    FColor := AColor;
end;

{ TCircle implementation }
constructor TCircle.Create(AColor: string; ARadius: real);
begin
    inherited Create(AColor);
    FRadius := ARadius;
end;

function TCircle.GetArea: real;
begin
    GetArea := 3.14159 * FRadius * FRadius;
end;

{ Main program }
var
    circle: TCircle;
begin
    circle := TCircle.Create('Red', 5.0);
    try
        writeln('Circle color: ', circle.Color);
        writeln('Circle area: ', circle.GetArea:0:2);
    finally
        circle.Free;
    end;
end.
```

## 3. Implementation-Specific Features

### Free Pascal / Lazarus

**Generics in Free Pascal:**

```objectpascal {class="highlight capsule-fpc"}
program FPCGenericsDemo;

{$mode objfpc}{$H+}{$J-}

type
    { Generic class }
    generic TList<T> = class
    private
        FItems: array of T;
        FCount: integer;
    public
        procedure Add(const Item: T);
        function Get(Index: integer): T;
        property Count: integer read FCount;
    end;

procedure TList.Add(const Item: T);
begin
    SetLength(FItems, FCount + 1);
    FItems[FCount] := Item;
    Inc(FCount);
end;

function TList.Get(Index: integer): T;
begin
    if (Index >= 0) and (Index < FCount) then
        Result := FItems[Index];
end;

type
    TIntList = specialize TList<integer>;
    TStringList = specialize TList<string>;

var
    intList: TIntList;
    i: integer;
begin
    intList := TIntList.Create;
    try
        intList.Add(10);
        intList.Add(20);
        intList.Add(30);
        
        writeln('Integer list:');
        for i := 0 to intList.Count - 1 do
            writeln('  ', intList.Get(i));
    finally
        intList.Free;
    end;
end.
```

**Free Pascal Generics.Collections**

```objectpascal {class="highlight capsule-fpc"}
program SimpleIntegerList;

{$mode objfpc}{$H+}{$J-}

uses
  Math,
  Generics.Defaults,
  Generics.Collections;

type
  TIntegerList = specialize TList<integer>;

var
  myIntList: TIntegerList;
  i: integer;

begin
  // Create a new generic list
  myIntList := TIntegerList.Create;
  try
    // Add some elements to the list, use Add or AddRange (append)
    myIntList.Add(0);
    myIntList.Add(1);
    myIntList.AddRange([9, 8, 7, 6, 5]);

    // Access the n-th element, 0-indexed
    WriteLn('The 3rd item is: ', myIntList[2]);

    // Sorting it ascending
    myIntList.Sort;

    // Iterate through the list
    for i := 0 to myIntList.Count - 1 do
      Writeln(myIntList[i]);

    // Get the mean
    WriteLn('The mean is: ', Math.Mean(myIntList.ToArray): 0: 2);

    // Empty the list
    myIntList.Clear;

  finally
    // Free the memory used by the list
    myIntList.Free;
  end;

  // Pause console
  ReadLn;
end.
```

### Delphi

**Inline variable declaration:** 

Ref: https://docwiki.embarcadero.com/RADStudio/Sydney/en/Inline_Variable_Declaration

```objectpascal {class="highlight capsule-delphi"}
procedure Test;
begin
  var I: Integer;
  I := 22;
  ShowMessage (I.ToString);
end;

procedure Test2;
begin
  var I, K: Integer;
  I := 22;
  K := I + 10;
  ShowMessage (K.ToString);
end;
```

**Scoped inline variable:** 

Ref: https://docwiki.embarcadero.com/RADStudio/Sydney/en/Inline_Variable_Declaration

```objectpascal {class="highlight capsule-delphi"}
procedure Test; // declaration and initialization in a single statement
begin
  var I: Integer := 22;
  ShowMessage (I.ToString);
end;

procedure Test1; // multiple inline declarations (symbols declared when used)
begin
  var I: Integer := 22;
  var J: Integer;
  J := 22 + I;
  var K: Integer := I + J;
  ShowMessage (K.ToString);
end;

procedure Test2; // scope limited to local block
begin
  var I: Integer := 22;
  if I > 10 then
  begin
    var J: Integer := 3;
    ShowMessage (J.ToString);
  end
  else
  begin
    var K: Integer := 3;
    ShowMessage (J.ToString); // COMPILER ERROR: “Undeclared identifier: ‘J’”
  end;
end;
```

**Generics:**


```objectpascal {class="highlight capsule-delphi"}
program GenericClassExample;

{$APPTYPE CONSOLE}

type
  TMyGenericClass<T> = class
  private
    FValue: T;
  public
    constructor Create(AValue: T);
    function GetValue: T;
    procedure SetValue(AValue: T);
  end;

constructor TMyGenericClass<T>.Create(AValue: T);
begin
  FValue := AValue;
end;

function TMyGenericClass<T>.GetValue: T;
begin
  Result := FValue;
end;

procedure TMyGenericClass<T>.SetValue(AValue: T);
begin
  FValue := AValue;
end;

begin
  // Instantiate TMyGenericClass with Integer
  var IntInstance: TMyGenericClass<Integer>;
  IntInstance := TMyGenericClass<Integer>.Create(10);
  Writeln('Integer Value: ' + IntToStr(IntInstance.GetValue));
  IntInstance.SetValue(20);
  Writeln('New Integer Value: ' + IntToStr(IntInstance.GetValue));
  IntInstance.Free;

  // Instantiate TMyGenericClass with String
  var StringInstance: TMyGenericClass<string>;
  StringInstance := TMyGenericClass<string>.Create('Hello');
  Writeln('String Value: ' + StringInstance.GetValue);
  StringInstance.SetValue('World');
  Writeln('New String Value: ' + StringInstance.GetValue);
  StringInstance.Free;

  Readln;
end.
```

### Other Pascal Implementations

**PascalABC.NET:**
- .NET-based Pascal implementation
- Modern language features
- Integrated development environment
- Supports both console and Windows Forms applications


## 4. Cross-Platform Development

### Universal Approaches

**Console Applications:**

```objectpascal {class="highlight capsule-universal"}
program CrossPlatform;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils;

begin
  writeln('This program runs on:');
  {$IFDEF WINDOWS}
  writeln('- Windows');
  {$ENDIF}
  {$IFDEF LINUX}
  writeln('- Linux');
  {$ENDIF}
  {$IFDEF DARWIN}
  writeln('- macOS');
  {$ENDIF}
  
  writeln('Pascal version: ', {$I %FPCVERSION%});
  writeln('Target CPU: ', {$I %FPCTARGETCPU%});
  writeln('Target OS: ', {$I %FPCTARGETOS%});
end.
```

### Common Libraries and Frameworks

**For GUI Development:**

- **Lazarus LCL**: Cross-platform native widgets
- **fpGUI**: Lightweight, custom-drawn GUI
- **Delphi VCL & FireMonkey**: GUI Frameworks in Delphi

**For Web Development:**

- **Brook Framework**: Microframework for web applications
- **Horse**: Fast, minimalist web framework
- **Pas2JS**: Compile Pascal to JavaScript

**For Database Access:**

- **SQLDB**: Built-in database components
- **ZeosLib**: High-performance database connectivity
- **FireDAC**: Enterprise database access (Delphi)

### Best Practices for Cross-Platform Code

```objectpascal {class="highlight capsule-universal"}
program BestPractices;

uses
  SysUtils, Classes
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

function GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + '\MyApp\';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/.config/myapp/';
  {$ENDIF}
end;

procedure CreateDirectoryIfNotExists(const Path: string);
begin
  if not DirectoryExists(Path) then
    ForceDirectories(Path);
end;

var
  configPath: string;
begin
  configPath := GetConfigPath;
  CreateDirectoryIfNotExists(configPath);
  writeln('Config directory: ', configPath);
end.
```

## Getting Started

### Choose Your Pascal Implementation

**For Beginners:**
- **Free Pascal + Lazarus**: Free, cross-platform, excellent learning environment
- **PascalABC.NET**: Modern features, good for education

**For Professional Development:**
- **Delphi**: Commercial, extensive libraries, Windows-focused
- **Free Pascal**: Open source, highly compatible, cross-platform

**For Web Development:**
- **Pas2JS**: Compile Pascal to JavaScript
- **Brook Framework**: Web applications with Free Pascal

### Development Environment Setup

1. **Install a Pascal compiler** (Free Pascal recommended for beginners)
2. **Choose an IDE** (Lazarus for visual development, VS Code with Pascal extensions for text editing)
3. **Set up your first project** using the examples above
4. **Join the community** at [pascal-lang.org/community](/community/)

### Next Steps

- Explore [Learning Resources](/learn/) for tutorials and guides
- Check out [Community Projects](/community/) for real-world examples
- Contribute to open-source Pascal projects
- Share your Pascal knowledge with others

---

*This documentation covers Pascal features across multiple implementations. Code examples are marked with compatibility indicators: **UNIVERSAL** for standard Pascal features, **FPC** for Free Pascal specific features.*

## Standard Library Reference

### System Unit

The System unit is automatically included and provides core functionality:

**Memory Management:**
```objectpascal {class="highlight capsule-fpc"}
{ Dynamic memory allocation }
var
    ptr: ^integer;
begin
    New(ptr);           // Allocate memory
    ptr^ := 42;         // Assign value
    writeln(ptr^);      // Use value
    Dispose(ptr);       // Free memory
end;
```

**String Functions:**
```objectpascal {class="highlight capsule-fpc"}
program StringFunctions;
var
    text: string;
    position: integer;
begin
    text := 'Hello, Pascal World!';
    
    writeln('Length: ', Length(text));
    writeln('Uppercase: ', UpperCase(text));
    writeln('Lowercase: ', LowerCase(text));
    
    position := Pos('Pascal', text);
    writeln('Position of "Pascal": ', position);
    
    writeln('Substring: ', Copy(text, 8, 6)); // "Pascal"
end.
```

### SysUtils Unit

Provides extended system utilities:

```objectpascal {class="highlight capsule-fpc"}
uses SysUtils;

program SysUtilsExample;
var
    fileName: string;
    fileAge: integer;
begin
    fileName := 'example.txt';
    
    { File operations }
    if FileExists(fileName) then
    begin
        fileAge := FileAge(fileName);
        writeln('File exists, age: ', fileAge);
        writeln('File size: ', FileSize(fileName), ' bytes');
    end
    else
        writeln('File does not exist');
    
    { String formatting }
    writeln(Format('Today is %s', [DateToStr(Date)]));
    writeln(Format('Current time: %s', [TimeToStr(Time)]));
end.
```

### Math Unit

Mathematical functions and constants:

```objectpascal {class="highlight capsule-fpc"}
uses Math;

program MathExample;
var
    angle, result: double;
begin
    angle := Pi / 4;  // 45 degrees in radians
    
    writeln('Sin(45°): ', Sin(angle):0:4);
    writeln('Cos(45°): ', Cos(angle):0:4);
    writeln('Tan(45°): ', Tan(angle):0:4);
    
    writeln('Square root of 16: ', Sqrt(16):0:2);
    writeln('2 to the power of 8: ', Power(2, 8):0:0);
    writeln('Natural log of e: ', Ln(Exp(1)):0:4);
    
    { Random numbers }
    Randomize;
    writeln('Random number (0-99): ', Random(100));
end.
```

## Advanced Programming Patterns

### Generic Programming

```objectpascal {class="highlight capsule-fpc"}
program GenericExample;

{$mode objfpc}
{$H+}

type
    generic TList<T> = class
    private
        FItems: array of T;
        FCount: integer;
    public
        procedure Add(const Item: T);
        function Get(Index: integer): T;
        property Count: integer read FCount;
    end;

procedure TList.Add(const Item: T);
begin
    SetLength(FItems, FCount + 1);
    FItems[FCount] := Item;
    Inc(FCount);
end;

function TList.Get(Index: integer): T;
begin
    if (Index >= 0) and (Index < FCount) then
        Result := FItems[Index]
    else
        raise Exception.Create('Index out of bounds');
end;

type
    TIntegerList = specialize TList<integer>;
    TStringList = specialize TList<string>;

var
    intList: TIntegerList;
    strList: TStringList;
    i: integer;
begin
    intList := TIntegerList.Create;
    strList := TStringList.Create;
    
    try
        { Add integers }
        for i := 1 to 5 do
            intList.Add(i * 10);
        
        { Add strings }
        strList.Add('Apple');
        strList.Add('Banana');
        strList.Add('Cherry');
        
        { Display results }
        writeln('Integers:');
        for i := 0 to intList.Count - 1 do
            writeln('  ', intList.Get(i));
        
        writeln('Strings:');
        for i := 0 to strList.Count - 1 do
            writeln('  ', strList.Get(i));
    finally
        intList.Free;
        strList.Free;
    end;
end.
```

### Interface Programming

```objectpascal {class="highlight capsule-fpc"}
program InterfaceExample;

{$mode objfpc}
{$interfaces corba}

type
    { Interface definition }
    IDrawable = interface
        procedure Draw;
        function GetArea: double;
    end;
    
    { Circle implementation }
    TCircle = class(TInterfacedObject, IDrawable)
    private
        FRadius: double;
    public
        constructor Create(ARadius: double);
        procedure Draw;
        function GetArea: double;
    end;
    
    { Rectangle implementation }
    TRectangle = class(TInterfacedObject, IDrawable)
    private
        FWidth, FHeight: double;
    public
        constructor Create(AWidth, AHeight: double);
        procedure Draw;
        function GetArea: double;
    end;

{ TCircle implementation }
constructor TCircle.Create(ARadius: double);
begin
    inherited Create;
    FRadius := ARadius;
end;

procedure TCircle.Draw;
begin
    writeln('Drawing a circle with radius: ', FRadius:0:2);
end;

function TCircle.GetArea: double;
begin
    Result := Pi * FRadius * FRadius;
end;

{ TRectangle implementation }
constructor TRectangle.Create(AWidth, AHeight: double);
begin
    inherited Create;
    FWidth := AWidth;
    FHeight := AHeight;
end;

procedure TRectangle.Draw;
begin
    writeln('Drawing a rectangle: ', FWidth:0:2, ' x ', FHeight:0:2);
end;

function TRectangle.GetArea: double;
begin
    Result := FWidth * FHeight;
end;

{ Main program }
var
    shapes: array[0..1] of IDrawable;
    shape: IDrawable;
begin
    { Create shapes }
    shapes[0] := TCircle.Create(5.0);
    shapes[1] := TRectangle.Create(4.0, 6.0);
    
    { Use polymorphism }
    for shape in shapes do
    begin
        shape.Draw;
        writeln('Area: ', shape.GetArea:0:2);
        writeln;
    end;
end.
```

### Multi-threading

```objectpascal {class="highlight capsule-fpc"}
program ThreadExample;

{$mode objfpc}
{$H+}

uses
    Classes, SysUtils;

type
    TWorkerThread = class(TThread)
    private
        FThreadID: integer;
    protected
        procedure Execute; override;
    public
        constructor Create(AThreadID: integer);
    end;

constructor TWorkerThread.Create(AThreadID: integer);
begin
    FThreadID := AThreadID;
    inherited Create(False); // Start immediately
end;

procedure TWorkerThread.Execute;
var
    i: integer;
begin
    for i := 1 to 5 do
    begin
        writeln('Thread ', FThreadID, ' - Step ', i);
        Sleep(1000); // Wait 1 second
    end;
    writeln('Thread ', FThreadID, ' completed');
end;

var
    threads: array[1..3] of TWorkerThread;
    i: integer;
begin
    writeln('Starting multiple threads...');
    
    { Create and start threads }
    for i := 1 to 3 do
        threads[i] := TWorkerThread.Create(i);
    
    { Wait for all threads to complete }
    for i := 1 to 3 do
    begin
        threads[i].WaitFor;
        threads[i].Free;
    end;
    
    writeln('All threads completed');
end.
```

## Database Programming

### SQLite Integration

```objectpascal {class="highlight capsule-fpc"}
program DatabaseExample;

{$mode objfpc}
{$H+}

uses
    Classes, SysUtils, sqldb, sqlite3conn;

var
    connection: TSQLite3Connection;
    transaction: TSQLTransaction;
    query: TSQLQuery;
begin
    { Create database components }
    connection := TSQLite3Connection.Create(nil);
    transaction := TSQLTransaction.Create(nil);
    query := TSQLQuery.Create(nil);
    
    try
        { Setup connection }
        connection.DatabaseName := 'example.db';
        connection.Transaction := transaction;
        transaction.DataBase := connection;
        query.DataBase := connection;
        
        { Open connection }
        connection.Open;
        
        { Create table }
        query.SQL.Text := 'CREATE TABLE IF NOT EXISTS users (' +
                         'id INTEGER PRIMARY KEY, ' +
                         'name TEXT NOT NULL, ' +
                         'email TEXT UNIQUE)';
        query.ExecSQL;
        transaction.Commit;
        
        { Insert data }
        query.SQL.Text := 'INSERT INTO users (name, email) VALUES (?, ?)';
        query.Params[0].AsString := 'John Doe';
        query.Params[1].AsString := 'john@example.com';
        query.ExecSQL;
        transaction.Commit;
        
        { Query data }
        query.SQL.Text := 'SELECT * FROM users';
        query.Open;
        
        while not query.EOF do
        begin
            writeln('ID: ', query.FieldByName('id').AsInteger);
            writeln('Name: ', query.FieldByName('name').AsString);
            writeln('Email: ', query.FieldByName('email').AsString);
            writeln('---');
            query.Next;
        end;
        
    finally
        query.Free;
        transaction.Free;
        connection.Free;
    end;
    
    writeln('Database operations completed');
end.
```

## GUI Development with Lazarus

### Basic Form Application

```objectpascal {class="highlight capsule-fpc"}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
    TForm1 = class(TForm)
        Button1: TButton;
        Edit1: TEdit;
        Label1: TLabel;
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
    public
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
    Caption := 'Pascal GUI Example';
    Label1.Caption := 'Enter your name:';
    Button1.Caption := 'Greet';
    Edit1.Text := '';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    userName: string;
begin
    userName := Trim(Edit1.Text);
    if userName <> '' then
        ShowMessage('Hello, ' + userName + '!')
    else
        ShowMessage('Please enter your name first.');
end;

end.
```

## Performance Optimization

### Memory Management Tips

```objectpascal {class="highlight capsule-fpc"}
program PerformanceExample;

type
    PIntegerArray = ^TIntegerArray;
    TIntegerArray = array[0..0] of integer;

var
    data: PIntegerArray;
    size, i: integer;
begin
    size := 1000000; // One million integers
    
    { Allocate memory efficiently }
    GetMem(data, size * SizeOf(integer));
    
    try
        { Initialize data }
        for i := 0 to size - 1 do
            data^[i] := i;
        
        { Process data efficiently }
        writeln('Processing ', size, ' integers...');
        
        { Example: Find sum }
        var sum: int64 := 0;
        for i := 0 to size - 1 do
            sum := sum + data^[i];
        
        writeln('Sum: ', sum);
        
    finally
        { Always free allocated memory }
        FreeMem(data);
    end;
end.
```

### Compiler Directives

```objectpascal {class="highlight capsule-fpc"}
program CompilerDirectives;

{$mode objfpc}          // Object Pascal mode
{$H+}                   // Long strings
{$R+}                   // Range checking (debug)
{$I+}                   // I/O checking
{$Q+}                   // Overflow checking
{$optimization level3}   // High optimization

{ Conditional compilation }
{$IFDEF DEBUG}
    {$DEFINE VERBOSE}
{$ENDIF}

{$IFDEF WINDOWS}
    {$DEFINE PLATFORM_SPECIFIC}
{$ENDIF}

begin
    writeln('Pascal program with compiler directives');
    
    {$IFDEF VERBOSE}
    writeln('Debug mode: Verbose output enabled');
    {$ENDIF}
    
    {$IFDEF PLATFORM_SPECIFIC}
    writeln('Windows-specific code would go here');
    {$ENDIF}
end.
```

## Testing and Quality Assurance

### Unit Testing Framework

```objectpascal {class="highlight capsule-fpc"}
program TestExample;

{$mode objfpc}
{$H+}

uses
    Classes, SysUtils;

type
    TTestCase = class
    private
        FTestName: string;
        FPassed: boolean;
    public
        constructor Create(const ATestName: string);
        procedure AssertEquals(Expected, Actual: integer; const Message: string = '');
        procedure AssertTrue(Condition: boolean; const Message: string = '');
        property TestName: string read FTestName;
        property Passed: boolean read FPassed;
    end;

constructor TTestCase.Create(const ATestName: string);
begin
    FTestName := ATestName;
    FPassed := True;
end;

procedure TTestCase.AssertEquals(Expected, Actual: integer; const Message: string);
begin
    if Expected <> Actual then
    begin
        FPassed := False;
        writeln('FAIL: ', FTestName);
        writeln('  Expected: ', Expected, ', Actual: ', Actual);
        if Message <> '' then
            writeln('  Message: ', Message);
    end
    else
        writeln('PASS: ', FTestName);
end;

procedure TTestCase.AssertTrue(Condition: boolean; const Message: string);
begin
    if not Condition then
    begin
        FPassed := False;
        writeln('FAIL: ', FTestName);
        if Message <> '' then
            writeln('  Message: ', Message);
    end
    else
        writeln('PASS: ', FTestName);
end;

{ Example function to test }
function Add(a, b: integer): integer;
begin
    Result := a + b;
end;

function IsEven(n: integer): boolean;
begin
    Result := (n mod 2) = 0;
end;

{ Test cases }
var
    test: TTestCase;
begin
    writeln('Running Pascal Unit Tests');
    writeln('========================');
    
    { Test addition }
    test := TTestCase.Create('Add function test');
    test.AssertEquals(5, Add(2, 3), 'Basic addition');
    test.Free;
    
    test := TTestCase.Create('Add negative numbers');
    test.AssertEquals(-1, Add(-3, 2), 'Adding negative numbers');
    test.Free;
    
    { Test even number detection }
    test := TTestCase.Create('Even number test');
    test.AssertTrue(IsEven(4), '4 should be even');
    test.Free;
    
    test := TTestCase.Create('Odd number test');
    test.AssertTrue(not IsEven(3), '3 should be odd');
    test.Free;
    
    writeln('Tests completed');
end.
```

## Deployment and Distribution

### Cross-Platform Compilation

```bash
# Compile for Windows (64-bit)
fpc -Twin64 -O3 program.pas

# Compile for Linux (64-bit)
fpc -Tlinux -O3 program.pas

# Compile for macOS
fpc -Tdarwin -O3 program.pas

# Create optimized release build
fpc -O3 -Xs -XX -CX program.pas
```

### Build Scripts

**Windows Batch File (build.bat):**
```batch
@echo off
echo Building Pascal project...
fpc -O3 -Xs -XX -CX src\main.pas
if %errorlevel% == 0 (
    echo Build successful!
    copy main.exe bin\
) else (
    echo Build failed!
)
pause
```

**Linux Shell Script (build.sh):**
```bash
#!/bin/bash
echo "Building Pascal project..."
fpc -O3 -Xs -XX -CX src/main.pas
if [ $? -eq 0 ]; then
    echo "Build successful!"
    cp main bin/
else
    echo "Build failed!"
fi
```
