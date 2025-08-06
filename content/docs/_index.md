+++
title = 'Documentation'
date = 2024-05-30T18:56:05+03:00
draft = false
layout = 'single-with-sidebar'
+++


Comprehensive guides and references for Pascal programming.

## Language Reference

### Syntax Guide

Pascal uses a clear, structured syntax that emphasizes readability:

```objectpascal {class="highlight capsule-fpc"}
program ExampleProgram;

const
    MAX_SIZE = 100;
    PI = 3.14159;

type
    TIntArray = array[1..MAX_SIZE] of integer;
    TPoint = record
        x, y: real;
    end;

var
    numbers: TIntArray;
    point: TPoint;
    i: integer;

begin
    { Initialize array }
    for i := 1 to 10 do
        numbers[i] := i * i;
    
    { Initialize record }
    point.x := 10.5;
    point.y := 20.3;
    
    { Output results }
    writeln('First 10 squares:');
    for i := 1 to 10 do
        writeln(i, '^2 = ', numbers[i]);
    
    writeln('Point: (', point.x:0:1, ', ', point.y:0:1, ')');
end.
```

### Data Types

**Basic Types:**
- `integer`: Whole numbers (-32768 to 32767)
- `longint`: Extended integers (-2147483648 to 2147483647)
- `real`: Floating-point numbers
- `double`: Double-precision floating-point
- `char`: Single character
- `string`: Text strings
- `boolean`: True or false values

**Structured Types:**

```objectpascal {class="highlight capsule-fpc"}
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

## Code Examples

### File Operations

```objectpascal {class="highlight capsule-fpc"}
program FileExample;
var
    inputFile, outputFile: text;
    line: string;
begin
    assign(inputFile, 'input.txt');
    assign(outputFile, 'output.txt');
    
    reset(inputFile);
    rewrite(outputFile);
    
    while not eof(inputFile) do
    begin
        readln(inputFile, line);
        writeln(outputFile, 'Processed: ', line);
    end;
    
    close(inputFile);
    close(outputFile);
    
    writeln('File processing complete.');
end.
```

### Dynamic Arrays

```objectpascal {class="highlight capsule-fpc"}
program DynamicArrayExample;
type
    TIntArray = array of integer;

var
    numbers: TIntArray;
    i, size: integer;

begin
    write('Enter array size: ');
    readln(size);
    
    { Allocate memory }
    SetLength(numbers, size);
    
    { Fill array }
    for i := 0 to size - 1 do
        numbers[i] := Random(100);
    
    { Display array }
    writeln('Random numbers:');
    for i := 0 to size - 1 do
        write(numbers[i], ' ');
    writeln;
    
    { Free memory automatically when program ends }
end.
```

### Object-Oriented Programming

```objectpascal {class="highlight capsule-fpc"}
program OOPExample;

type
    { Base class }
    TShape = class
    private
        FColor: string;
    public
        constructor Create(AColor: string);
        procedure SetColor(AColor: string);
        function GetColor: string;
        function GetArea: real; virtual; abstract;
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

procedure TShape.SetColor(AColor: string);
begin
    FColor := AColor;
end;

function TShape.GetColor: string;
begin
    Result := FColor;
end;

{ TCircle implementation }
constructor TCircle.Create(AColor: string; ARadius: real);
begin
    inherited Create(AColor);
    FRadius := ARadius;
end;

function TCircle.GetArea: real;
begin
    Result := 3.14159 * FRadius * FRadius;
end;

{ Main program }
var
    circle: TCircle;
begin
    circle := TCircle.Create('Red', 5.0);
    writeln('Circle color: ', circle.GetColor);
    writeln('Circle area: ', circle.GetArea:0:2);
    circle.Free;
end.
```

## Best Practices

### Code Style

1. **Use meaningful names:**
   ```objectpascal {class="highlight capsule-fpc"}
   var
       studentCount: integer;    // Good
       sc: integer;              // Avoid
   ```

2. **Consistent indentation:**
   ```objectpascal {class="highlight capsule-fpc"}
   if condition then
   begin
       statement1;
       statement2;
   end;
   ```

3. **Comment your code:**
   ```objectpascal {class="highlight capsule-fpc"}
   { Calculate compound interest }
   function CompoundInterest(principal, rate: real; years: integer): real;
   begin
       Result := principal * Power(1 + rate, years);
   end;
   ```

### Error Handling

```objectpascal {class="highlight capsule-fpc"}
program ErrorHandlingExample;
var
    number, divisor: integer;
    result: real;
begin
    try
        write('Enter a number: ');
        readln(number);
        write('Enter divisor: ');
        readln(divisor);
        
        if divisor = 0 then
            raise Exception.Create('Division by zero not allowed');
        
        result := number / divisor;
        writeln('Result: ', result:0:2);
        
    except
        on E: Exception do
            writeln('Error: ', E.Message);
    end;
end.
```

## Tools and Libraries

### Free Pascal Compiler
- Cross-platform compilation
- Extensive standard library
- Compatible with Turbo Pascal and Delphi

### Lazarus IDE
- Visual form designer
- Integrated debugger
- Component library (LCL)
- Cross-platform GUI development

### Popular Libraries
- **LCL**: Lazarus Component Library for GUI
- **FCL**: Free Component Library for utilities
- **Synapse**: Network and internet protocols
- **ZeosDBO**: Database connectivity

## Further Reading

- [Free Pascal Reference Guide](https://www.freepascal.org/docs-html/ref/ref.html)
- [Lazarus Documentation](https://wiki.lazarus.freepascal.org/)
- [Pascal Programming Tutorials](/learn/)
- [Community Examples and Projects](/community/)

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
