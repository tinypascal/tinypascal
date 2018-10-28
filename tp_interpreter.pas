unit tp_interpreter;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_syntax_tree;

type TFuncImport = Function(keyword: String): String;       // imports a value from an outer world variable, identified by a keyword
     TProcExport = Procedure(keyword, content: String);     // exports the content to an outer world variable, identified by a keyword
     TProcUser = Procedure(paramList: TStringList);         // executes a user defined procedure with the given parameter

procedure tp_interpret(rootNode: TTreeNode);

// these are set externally to communicate with the outer world
var tp_import_func: TFuncImport = nil;
    tp_export_proc: TProcExport = nil;
    tp_user_proc: TProcUser = nil;

implementation

uses tp_utils, tp_constants, tp_error_reporting, math;
{

TODO: add error recovery! global error string list with an abort bool var
if set all functions abort and return nil, also delete all other datastructures correctly

check for nil after execution and if so, read error strings from list
}

procedure tp_interpret(rootNode: TTreeNode);
var variables: TStringList;
    FormatSettings: TFormatSettings;
    BreakFlag, ContinueFlag, HaltFlag: Boolean;

function getFloat(s: String): Extended;
begin
  Result := StrToFloatDef(s, 0, FormatSettings);
end;

function getInt(s: String): Integer;
begin
  Result := StrToIntDef(s, 0);
end;

function getString(f: Extended): String;
begin
  // FloatToStr is a very expensive function, this check is overal faster!
  if (abs(frac(f)) > 0) then Result := FloatToStr(f, FormatSettings)
                        else Result := IntToStr(trunc(f));
end;

function limit(x, x_min, x_max: Integer): Integer;
begin
  Result := min(max(x, x_min), x_max);
end;

procedure create_vars(index: Integer);
begin
  while (variables.Count - 1 < index) do variables.Add('');
end;

function isVariable(node: TTreeNode): Boolean;
begin
  Result := node.operation = CopVariable;
end;

// quick and dirty, dont need exceptions...
function StringToBool(s: String): Boolean;
begin
  Result := (s = 'T');
end;

function BoolToString(b: Boolean): String;
begin
  if b then Result := 'T'
       else Result := 'F';
end;

function interpret(node: TTreeNode): String;
var float_temp: Extended;
    i, loopCycles, for_from, for_to, int_temp: Integer;
    s1, s2 : String;
    nodeVariable: TTreeNode;
    userParams: TStringList;
begin
  if (node = nil) or (HaltFlag) or (BreakFlag) or (ContinueFlag) or (global_error) then Exit('@error');

  case node.operation of
    CopNOP: begin {No Operation...} end;

    CopVariable:
      begin
        create_vars(node.symbol_index);
        Result := variables[node.symbol_index];
      end;

    CopNumber, CopString:
      Result := node.s;

    CopArrayIndex:
    begin
      i := getInt(interpret(node.secondChild));
      s1 := interpret(node.firstChild);
      if (i < 1) or (i > length(s1)) then report_error('String Index out of Bounds: ' + IntToStr(i), node.line, CopArrayIndex, C_Warning)
                                     else Result := s1[i];
    end;

    CopAssign:
      begin
        // indexed assignment like a[2] := 'b';
        if node.firstChild.operation = CopArrayIndex then
        begin
          nodeVariable := node.firstChild.firstChild;
          if isVariable(nodeVariable) then
          begin
            create_vars(nodeVariable.symbol_index);
            s1 := variables.Strings[nodeVariable.symbol_index];
            s2 := interpret(node.secondChild) + #0; // + #0 in case string is empty
            i := getInt(interpret(node.firstChild.secondChild));

            if (i < 1) or (i > length(s1)) then report_error('String Index out of Bounds: ' + IntToStr(i), node.firstChild.secondChild.line, CopArrayIndex, C_Warning)
                                           else s1[i] := s2[1];

            variables.Strings[nodeVariable.symbol_index] := s1;
          end
          else report_error('Variable expected, found ' + operationtostr(nodeVariable.operation), nodeVariable.line, CopAssign, C_Warning);
        end
        else
        // direct assignment like a := 'Hello';
        if isVariable(node.firstChild) then
        begin
          create_vars(node.firstChild.symbol_index);
          variables.Strings[node.firstChild.symbol_index] := interpret(node.secondChild)
        end
        else report_error('Variable expected, found ' + operationtostr(node.firstChild.operation), node.firstChild.line, CopAssign, C_Warning);
      end;

    CopCompareEqual:
      Result := BoolToString(interpret(node.firstChild) = interpret(node.secondChild));

    CopCompareUnequal:
      Result := BoolToString(interpret(node.firstChild) <> interpret(node.secondChild));

    CopCompareGreater:
        Result := BoolToString(getFloat(interpret(node.firstChild)) > getFloat(interpret(node.secondChild)));

    CopCompareSmaller:
      Result := BoolToString(getFloat(interpret(node.firstChild)) < getFloat(interpret(node.secondChild)));

    CopCompareGreaterOrEqual:
      Result := BoolToString(getFloat(interpret(node.firstChild)) >= getFloat(interpret(node.secondChild)));

    CopCompareSmallerOrEqual:
      Result := BoolToString(getFloat(interpret(node.firstChild)) <= getFloat(interpret(node.secondChild)));

    CopLogicTrue:
      Result := BoolToString(true);

    CopLogicFalse:
      Result := BoolToString(false);

    CopLogicAnd:
      Result := BoolToString(StringToBool(interpret(node.firstChild)) and StringToBool(interpret(node.secondChild)));

    CopLogicOr:
      Result := BoolToString(StringToBool(interpret(node.firstChild)) or StringToBool(interpret(node.secondChild)));

    CopLogicNot:
      Result := BoolToString(not StringToBool(interpret(node.firstChild)));

    CopMathAdd:
      begin
        s1 := interpret(node.firstChild);
        s2 := interpret(node.secondChild);

        // is any of the parameters a string or the conversion fails -> handle as strings
        if (node.firstChild.operation = CopString)
        or (node.secondChild.operation = CopString)
        or (not TryStrToFloat(s1, float_temp, FormatSettings))
        or (not TryStrToFloat(s2, float_temp, FormatSettings)) then Result := s1 + s2
                                                               else Result := getString(getFloat(s1) + getFloat(s2));
      end;

    CopMathSub:
      Result := getString(getFloat(interpret(node.firstChild)) - getFloat(interpret(node.secondChild)));

    CopMathMul:
      Result := getString(getFloat(interpret(node.firstChild)) * getFloat(interpret(node.secondChild)));

    CopMathDiv:
      begin
        float_temp := getFloat(interpret(node.secondChild));
        if float_temp = 0 then report_error('Division by Zero.', node.line, node.operation, C_Warning)
                          else Result := getString(getFloat(interpret(node.firstChild)) / float_temp);
      end;

      CopMathDivInt:
      begin
        int_temp := getInt(interpret(node.secondChild));
        if int_temp = 0 then report_error('Division by Zero.', node.line, node.operation, C_Warning)
                        else Result := getString(getInt(interpret(node.firstChild)) div int_temp);
      end;

    CopMathMod:
      begin
        int_temp := getInt(interpret(node.secondChild));
        if int_temp <= 0 then report_error('Modulo by Zero.', node.line, node.operation, C_Warning)
                         else Result := getString(getInt(interpret(node.firstChild)) mod int_temp);
      end;

    CopCommandBlock:
        for i := 0 to node.children.Count - 1 do interpret(node.getChild(i));

    CopCondition:
      begin
        if (StringToBool(interpret(node.firstChild))) then interpret(node.secondChild)
                                                      else interpret(node.thirdChild);
      end;

    // i, 4, command, command <else> --> isidentifier nehmen für variable, zahl, string!
    CopCase:
      begin
        s1 := interpret(node.firstChild); // i
        i := 1;
        while (isIdentifier(node.getChild(i).operation) or isMathOperation(node.getChild(i).operation) or isLogicOperation(node.getChild(i).operation)) do
        begin
          if s1 = interpret(node.getChild(i)) then // if argument matches the case
          begin
            inc(i);
            break;
          end
          else inc(i, 2);
        end;
        interpret(node.getChild(i)); // either interprets the correct case or the last statement which is else / NOP
      end;

    CopLoopWhile:
      begin
        loopCycles := 0;
        while (StringToBool(interpret(node.firstChild)) and (loopCycles < CtpInterpreterMaxLoopCycles) and (not global_error)) do
        begin
          interpret(node.secondChild);
          ContinueFlag := false; // reset flag for next cycle
          if BreakFlag or HaltFlag then Break;
          inc(loopCycles);
        end;
        if (loopCycles >= CtpInterpreterMaxLoopCycles) then report_error('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(CtpInterpreterMaxLoopCycles), node.line, CopLoopWhile, C_Notice);

        BreakFlag := false; // reset flag
      end;

    CopLoopRepeatUntil:
      begin
        loopCycles := 0;
        repeat
          // Count - 2 because last Child = condition!
          for i := 0 to node.children.Count - 2 do interpret(node.getChild(i));

          ContinueFlag := false; // reset flag for next cycle
          if BreakFlag or HaltFlag then Break;
          inc(loopCycles);
        until (StringToBool(interpret(node.lastChild))) or (loopCycles >= CtpInterpreterMaxLoopCycles) or (global_error);
        if (loopCycles >= CtpInterpreterMaxLoopCycles) then report_error('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(CtpInterpreterMaxLoopCycles), node.line, CopLoopRepeatUntil, C_Notice);

        BreakFlag := false; // reset flag
      end;

    CopLoopForTo:
      begin
        for_from := getInt(interpret(node.secondChild));
        for_to := getInt(interpret(node.thirdChild));
        if ((for_to - for_from) >= CtpInterpreterMaxLoopCycles) then report_error('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(CtpInterpreterMaxLoopCycles), node.line, CopLoopForTo, C_Notice);

        create_vars(node.firstChild.symbol_index);
        for i := for_from to for_to do
        begin
          variables.Strings[node.firstChild.symbol_index] := getString(i);
          interpret(node.fourthChild);
          ContinueFlag := false; // reset flag for next cycle
          if BreakFlag or HaltFlag or global_error then Break;
        end;

        BreakFlag := false; // reset flag
      end;

    CopLoopForDownTo:
      begin
        for_from := getInt(interpret(node.secondChild));
        for_to := getInt(interpret(node.thirdChild));
        if ((for_from - for_to) >= CtpInterpreterMaxLoopCycles) then report_error('Possible infinite loop. Maximum allowed Cycles: ' + IntToStr(CtpInterpreterMaxLoopCycles), node.line, CopLoopForTo, C_Notice);

        create_vars(node.firstChild.symbol_index);
        for i := for_from downto for_to do
        begin
          variables.Strings[node.firstChild.symbol_index] := getString(i);
          interpret(node.fourthChild);
          ContinueFlag := false; // reset flag for next cycle
          if BreakFlag or HaltFlag or global_error then Break;
        end;

        BreakFlag := false; // reset flag
      end;

    CopFunctionCall:
      begin
        // no User defined functions supported yet, every function must be a known SysCall with an id <> -1
        if node.symbol_index = 255 then report_error('Call to unknown Function: ' + node.firstChild.s, node.line, CopFunctionCall, C_Error);

        case node.symbol_index of
          CSysIdImport: if (tp_import_func <> nil) and (node.secondChild <> nil) then Result := tp_import_func(interpret(node.secondChild))
                                                                                 else report_error('Import() not defined or missing arguments.', node.line, CopFunctionCall, C_Warning);

          CSysIdExport: if (tp_export_proc <> nil) and (node.children.Count >= 3) then tp_export_proc(interpret(node.secondChild), interpret(node.thirdChild))
                                                                                  else report_error('Export() not defined or missing arguments.', node.line, CopFunctionCall, C_Warning);

          CSysIdUserFunction:
            begin
              userParams := TStringList.Create;

              // evaluate user params
              for i := 1 to node.children.Count - 1 do
              userParams.Add(interpret(node.getChild(i)));

              if tp_user_proc <> nil then tp_user_proc(userParams)
                                     else report_error('User() not defined or missing arguments.', node.line, CopFunctionCall, C_Warning);
              FreeAndNil(userParams);
            end;

          CSysIdWrite:
            for i := 1 to node.children.Count - 1 do write(interpret(node.getChild(i)));

          CSysIdWriteLn:
            begin
              for i := 1 to node.children.Count - 1 do write(interpret(node.getChild(i)));
              writeln;
            end;

          CSysIdHalt:
            HaltFlag := true;

          CSysIdBreak:
            BreakFlag := true;

          CSysIdContinue:
            ContinueFlag := true;

          CSysIdInc, CSysIdDec:
            begin
              nodeVariable := node.secondChild.firstChild; // func-arg first one is var
              create_vars(nodeVariable.symbol_index);

              if nodeVariable.operation = CopVariable then
              begin
                int_temp := 1;
                if node.thirdChild <> nil then int_temp := getInt(interpret(node.thirdChild));

                if node.symbol_index = CSysIdInc then variables.Strings[nodeVariable.symbol_index] := getString(getInt(variables.Strings[nodeVariable.symbol_index]) + int_temp)  // inc
                                                 else variables.Strings[nodeVariable.symbol_index] := getString(getInt(variables.Strings[nodeVariable.symbol_index]) - int_temp); // dec
              end
              else report_error('Variable expected, found ' + operationtostr(nodeVariable.operation), nodeVariable.line, CopFunctionCall, C_Warning);
            end;

          CSysIdOrd:
            begin
              s1 := interpret(node.secondChild) + #0; {in case the string is empty}
              Result := getString(ord(s1[1]));
            end;

          CSysIdChr:
            Result := chr(limit(getInt(interpret(node.secondChild)), 0, 255));

          CSysIdLength:
            Result := getString(Length(interpret(node.secondChild)));

          CSysIdConcat:
            begin
              Result := '';
              for i := 1 to node.children.Count - 1 do
              Result := Result + interpret(node.getChild(i));
            end;

          CSysIdRandom:
            if node.secondChild = nil then // 2 variants of random!
              Result :=  getString(Random)
            else
              Result := getString(Random(getInt(interpret(node.secondChild))));

          else
            report_error('Unknown Function ' + node.firstChild.s, node.line, CopFunctionCall, C_Error);
        end; // case
      end; // begin

    CopFunctionArg:
      Result := interpret(node.firstChild);

    else report_error('Unknown Operation ' + operationtostr(node.operation), node.line, node.operation, C_Error);
  end;
end;

var i: Integer;
begin
  if CtpDebugMode then
  begin
    writeln;
    writeln('*** Interpreter ***');
  end;

  Randomize;

  variables := TStringList.Create;
  FormatSettings := DefaultFormatSettings;
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator := '.';
  BreakFlag := false;
  ContinueFlag := false;
  HaltFlag := false;

  interpret(rootNode);

  if CtpDebugMode then
  begin
    writeln;
    writeln('# Variable State Dump');
    for i := 0 to variables.Count - 1 do
    writeln('Var_' + IntToStr(i) + ' value: ' + variables.Strings[i]);

    writeln;
    writeln('# Execution halted.');
  end;

  FreeAndNil(rootNode);
  FreeAndNil(variables);
end;

end.

