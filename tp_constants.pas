unit tp_constants;

{$mode objfpc}{$H+}

interface

type TSysCallEntry = record
       s: String;
       id: Integer;
     end;

const
  CtpDebugMode: Boolean = false;
  CtpInterpreterMaxLoopCycles: Integer = 100123; // how many cycles each loop is allowed to run...

  CtokenIdentifier = 1;
  CtokenOperator = 2;
  CtokenString = 3;
  CtokenNumber = 4;
  CtokenComment = 5;
  CtokenSingle = 6;

  CopNOP = 0;
  CopAssign = 1;
  CopCommandBlock = 2;
  CopCommandBlockMarker = 3;
  CopCondition = 4;
  CopCase = 5;
  CopCaseMarker = 6;
  CopLoopWhile = 7;
  CopLoopRepeatUntil = 8;
  CopLoopRepeatMarker = 9;
  CopLoopForTo = 10;
  CopLoopForDownTo = 11;
  CopLogicTrue = 12;
  CopLogicFalse = 13;
  CopLogicOr = 14;
  CopLogicAnd = 15;
  CopLogicNot = 16;
  CopCompareEqual = 17;
  CopCompareUnequal = 18;
  CopCompareGreater = 19;
  CopCompareSmaller = 20;
  CopCompareGreaterOrEqual = 21;
  CopCompareSmallerOrEqual = 22;
  CopMathAdd = 23;
  CopMathSub = 24;
  CopMathMul = 25;
  CopMathDiv = 26;
  CopMathDivInt = 27;
  CopMathMod = 28;
  CopFunctionCall = 29;
  CopFunctionArg = 30;
  CopArrayIndex = 31;
  CopVariable = 32;
  CopString = 33;
  CopNumber = 34;

  CSysCallCount = 17;

  CSysIdImport = 0;
  CSysIdExport = 1;
  CSysIdUserFunction = 2;
  CSysIdWrite = 3;
  CSysIdWriteLn = 4;
  CSysIdRead = 5;
  CSysIdReadLn = 6;
  CSysIdHalt = 7;
  CSysIdBreak = 8;
  CSysIdContinue = 9;
  CSysIdInc = 10;
  CSysIdDec = 11;
  CSysIdOrd = 12;
  CSysIdChr = 13;
  CSysIdLength = 14;
  CSysIdConcat = 15;
  CSysIdRandom = 16;

  CSysCalls: array[0..CSysCallCount - 1] of TSysCallEntry =
    (
      (s: 'import';               id: CSysIdImport),
      (s: 'export';               id: CSysIdExport),
      (s: 'user';                 id: CSysIdUserFunction),
      (s: 'write';                id: CSysIdWrite),
      (s: 'writeln';              id: CSysIdWriteLn),
      (s: 'read';                 id: CSysIdRead),
      (s: 'readln';               id: CSysIdReadLn),
      (s: 'halt';                 id: CSysIdHalt),
      (s: 'break';                id: CSysIdBreak),
      (s: 'continue';             id: CSysIdContinue),
      (s: 'inc';                  id: CSysIdInc),
      (s: 'dec';                  id: CSysIdDec),
      (s: 'ord';                  id: CSysIdOrd),
      (s: 'chr';                  id: CSysIdChr),
      (s: 'length';               id: CSysIdLength),
      (s: 'concat';               id: CSysIdConcat),
      (s: 'random';               id: CSysIdRandom)
    );

  CReservedIdentifier = '*var*function*procedure*nil*begin*end*if*then*else*label*while*do*repeat*until*for*to*downto*true*false*or*and*not*div*mod*break*continue*exit*'; // search string will be '*' + s + '*'

implementation

end.

