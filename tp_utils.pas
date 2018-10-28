unit tp_utils;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function tp_is_alpha(c: Char): Boolean;
function tp_is_numeric(c: Char): Boolean;
function tp_is_operator(c: Char): Boolean;
function tp_is_whitespace(c: Char): Boolean;

function isIdentifier(op: Byte): Boolean;
function isLogicOperation(op: Byte): Boolean;
function isMathOperation(op: Byte): Boolean;
function isBinaryOperation(op: Byte): Boolean;

function getSysCallId(s: String): Integer;
function isSysCall(s: String): Boolean;

function tokentostr(id: Byte): String;
function operationtostr(op: Byte): String;

implementation

uses tp_constants;

function tp_is_alpha(c: Char): Boolean;
begin
  Result := c in ['A' .. 'Z', 'a' .. 'z'];
end;

function tp_is_numeric(c: Char): Boolean;
begin
  Result := c in ['0' .. '9'];
end;

function tp_is_operator(c: Char): Boolean;
begin
  Result := c in [':', '=', '<', '>', '+', '-', '*', '/'];
end;

function tp_is_whitespace(c: Char): Boolean;
begin
  Result := c in [#9, #10, #13, #32]; // Tab, Line Feed, Carriage Return, Space
end;

function isIdentifier(op: Byte): Boolean;
begin
  case op of
    CopVariable,
    CopString,
    CopNumber:
      Result := true;

    else Result := false;
  end;
end;

function isLogicOperation(op: Byte): Boolean;
begin
  case op of
    CopCompareEqual,
    CopCompareUnequal,
    CopCompareGreater,
    CopCompareSmaller,
    CopCompareGreaterOrEqual,
    CopCompareSmallerOrEqual,
    CopLogicTrue,
    CopLogicFalse,
    CopLogicAnd,
    CopLogicOr,
    CopLogicNot:
      Result := true;

    else Result := false;
  end;
end;

function isMathOperation(op: Byte): Boolean;
begin
  case op of
    CopMathAdd,
    CopMathSub,
    CopMathMul,
    CopMathDiv,
    CopMathDivInt,
    CopMathMod:
      Result := true;

    else Result := false;
  end;
end;

// everything with exactly two sides linked by an operation
function isBinaryOperation(op: Byte): Boolean;
begin
  case op of
    CopAssign,
    CopLogicOr,
    CopLogicAnd,
    CopCompareEqual,
    CopCompareUnequal,
    CopCompareGreater,
    CopCompareSmaller,
    CopCompareGreaterOrEqual,
    CopCompareSmallerOrEqual,
    CopMathAdd,
    CopMathSub,
    CopMathMul,
    CopMathDiv,
    CopMathDivInt,
    CopMathMod:
      Result := true;

    else Result := false;
  end;
end;

// check known system calls to distinguish them from variables
// a function call has to look like this foo(); per definition, but pascal allows foo; aswell
// this code is needed to deal with this exception
function getSysCallId(s: String): Integer;
var i: Integer;
begin
  Result := -1;
  s := LowerCase(s);

  for i := 0 to CSysCallCount - 1 do
  if CSysCalls[i].s = s then
  begin
    Result := i;
    Break;
  end;
end;

function isSysCall(s: String): Boolean;
begin
  Result := getSysCallId(s) <> -1;
end;

function tokentostr(id: Byte): String;
begin
  case id of
    CtokenIdentifier: Result := 'Identifier';
    CtokenOperator: Result := 'Operator';
    CtokenString: Result := 'String';
    CtokenNumber: Result := 'Number';
    CtokenComment: Result := 'Comment';
    CtokenSingle: Result := 'Single';
    else Result := '<Unknown Token ID ' + IntToStr(id) + '!>';
  end;
end;

function operationtostr(op: Byte): String;
begin
  case op of
    CopNOP: Result := 'NOP';
    CopAssign: Result := ':=';
    CopCommandBlock: Result := 'begin-end';
    CopCommandBlockMarker: Result := '#begin_marker';
    CopCondition: Result := 'if-then';
    CopCase: Result := 'case-of';
    CopCaseMarker: Result := '#case_marker';
    CopLoopWhile: Result := 'while-do';
    CopLoopRepeatUntil: Result := 'repeat-until';
    CopLoopRepeatMarker: Result := '#repeat_marker';
    CopLoopForTo: Result := 'for-to';
    CopLoopForDownTo: Result := 'for-downto';
    CopLogicTrue: Result := 'true';
    CopLogicFalse: Result := 'false';
    CopLogicOr: Result := 'or';
    CopLogicAnd: Result := 'and';
    CopLogicNot: Result := 'not';
    CopCompareEqual: Result := '=';
    CopCompareUnequal: Result := '<>';
    CopCompareGreater: Result := '>';
    CopCompareSmaller: Result := '<';
    CopCompareGreaterOrEqual: Result := '>=';
    CopCompareSmallerOrEqual: Result := '<=';
    CopMathAdd: Result := '+';
    CopMathSub: Result := '-';
    CopMathMul: Result := '*';
    CopMathDiv: Result := '/';
    CopMathDivInt: Result := 'div';
    CopMathMod: Result := 'mod';
    CopFunctionCall: Result := 'func-call';
    CopFunctionArg: Result := 'func-arg';
    CopArrayIndex: Result := '[]';
    CopVariable: Result := 'var';
    CopString: Result := 'string';
    CopNumber: Result := 'number';
    else Result := '<Unknown Operation ID ' + IntToStr(op) + '!>';
  end;
end;

end.

