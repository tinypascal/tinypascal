unit tp_syntax_tree;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_tinylist;

type
    TTreeNode = class(TObject)
                  operation: Integer;
                  s: String; // all strings like identifier, constants etc
                  symbol_index: Integer; // assigns a unique index to every variable (original name in s)
                  line: Integer; // original line in source
                  children: TTinyList;

                  constructor Create(param_operation: Integer; param_s: String; param_line: Integer);
                  function getChild(index: Integer): TTreeNode;
                  function firstChild: TTreeNode;
                  function secondChild: TTreeNode;
                  function thirdChild: TTreeNode;
                  function fourthChild: TTreeNode;
                  function lastChild: TTreeNode;
                  procedure debug(current_depth: integer; symbol_table: TStringList);
                  destructor Destroy; override;
                end;

function create_node(operation: Integer; s: String; line: Integer): TTreeNode;
function tp_build_syntax_tree(stack: TTinyList): TTreeNode; // stack of TTreeNodes, Result is the final tree

implementation

uses tp_utils, tp_constants, tp_error_reporting;

function create_node(operation: Integer; s: String; line: Integer): TTreeNode;
begin
  Result := TTreeNode.Create(operation, s, line);
end;

constructor TTreeNode.Create(param_operation: Integer; param_s: String; param_line: Integer);
begin
  operation := param_operation;
  s := param_s;
  symbol_index := 255;
  line := param_line;
  children := TTinyList.Create;
end;

function TTreeNode.getChild(index: Integer): TTreeNode;
begin
  Result := TTreeNode(children.Items(index));
end;

// we are so lazy....
function TTreeNode.firstChild: TTreeNode;
begin
  Result := TTreeNode(children.GetFirst);
end;

function TTreeNode.secondChild: TTreeNode;
begin
  Result := getChild(1);
end;

function TTreeNode.thirdChild: TTreeNode;
begin
  Result := getChild(2);
end;

function TTreeNode.fourthChild: TTreeNode;
begin
  Result := getChild(3);
end;

function TTreeNode.lastChild: TTreeNode;
begin
  Result := TTreeNode(children.Get);
end;

procedure TTreeNode.debug(current_depth: integer; symbol_table: TStringList);
var i: Integer;
begin
  for i := 1 to current_depth do write('  ');     // indentation

  if children.Count > 0 then write('+ ')
                        else write('| ');

  write(operationtostr(operation));

  if operation = CopFunctionCall then write('(sys-call) ', CSysCalls[symbol_index].s, ' (id ', symbol_index, ')');
  if operation = CopVariable then write(' ',symbol_table.Strings[symbol_index], ' (index ', symbol_index, ')');
  if s <> '' then write(' ',s); // string, number etc

//  if children.Count = 1 then write(' (1 child)');
//  if children.Count > 1 then write(' (', children.Count, ' children)');

  writeln;

  for i := 0 to children.Count - 1 do getChild(i).debug(current_depth + 1, symbol_table);

  dec(current_depth);
end;

// deleting the master node will delete the whole tree
destructor TTreeNode.Destroy;
var node: TTreeNode;
begin
  while children.Count > 0 do
  begin
    node := TTreeNode(children.Items(0));
    FreeAndNil(node);
    children.RemoveFirst;
  end;

  FreeAndNil(children);

  inherited;
end;

function StackPeek(list: TTinyList): TTreeNode;
begin
  if list.Count < 1 then Result := nil
                    else Result := TTreeNode(list.GetFirst);
end;

function StackPop(list: TTinyList): TTreeNode;
begin
  Result := nil;
  if StackPeek(list) = nil then report_error('Stack Pop, unexpected end of Stack', 0, -1, C_Error)
  else
  begin
    Result := StackPeek(list);
    list.RemoveFirst;
  end;
end;

function StackPeekAtTop(list: TTinyList): TTreeNode;
begin
  if list.Count < 1 then Result := nil
                    else Result := TTreeNode(list.Get);
end;

function StackGetAndRemoveFromTop(list: TTinyList): TTreeNode;
begin
  Result := nil;
  if StackPeek(list) = nil then report_error('Stack Pop, unexpected end of Stack', 0, -1, C_Error)
  else
  begin
    Result := TTreeNode(list.Get);
    list.Remove;
  end;
end;

function tp_build_syntax_tree(stack: TTinyList): TTreeNode; // stack of TTreeNodes, Result is the final tree
var symbol_table: TStringList;

// warning! maximum of 255 symbols....
function get_symbol_index(s: String): Word;
var i, index: Word;
begin
  s := LowerCase(s);
  i := 0;
  index := 255;

  while ((i < symbol_table.Count) and (index = 255)) do
  begin
    if symbol_table.Strings[i] = s then index := i;
    inc(i);
  end;

  if index = 255 then index := symbol_table.Add(s);

  Result := index;
end;

var workNode, tempNode: TTreeNode;
    workstack: TTinyList;
    i, j: Integer;
begin
  if CtpDebugMode then
  begin
    writeln;
    writeln('*** Syntax Tree ***');
  end;

  workstack := TTinyList.Create;
  symbol_table := TStringList.Create;

  if CtpDebugMode then writeln('# Workstack:');
  while (stack.Count > 0) and (not global_error) do
  begin
    workNode := StackPop(stack);

    writeln;
    writeln('Current Operation: ' + operationtostr(workNode.operation) + ' ' + workNode.s);
    write('Current Workstack: ');

    // debug workstack
    if CtpDebugMode then
    begin
      for i := 0 to workstack.Count - 1 do
      begin
        write(operationtostr(TTreeNode(workstack.Items(i)).operation));
        // display children, if any
        if TTreeNode(workstack.Items(i)).children.Count > 0 then
        begin
          write('{');
          for j := 0 to TTreeNode(workstack.Items(i)).children.Count - 1 do
          begin
            if j > 0 then write('; ');
            write(operationtostr(TTreeNode(workstack.Items(i)).getChild(j).operation));
          end;
          write('}');
        end;

        write(' ');
      end;
      writeln;
    end;

    // cache symbols...
    if workNode.operation = CopVariable then
    begin
      workNode.symbol_index := get_symbol_index(workNode.s);
      workNode.s := '';
    end;

    if isIdentifier(workNode.operation) // = var, string, number
    or (workNode.operation in [CopNOP, CopLogicTrue, CopLogicFalse, CopLoopRepeatMarker, CopCommandBlockMarker, CopCaseMarker]) then workstack.Add(workNode)
    else
    if workNode.operation = CopLogicNot then
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // get the command to negate...
      workstack.Add(workNode);
    end
     else
    if isBinaryOperation(workNode.operation) then
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack));
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack));
      workstack.Add(workNode);
    end
    else
    if workNode.operation = CopCommandBlock then
    begin
      while (StackPeekAtTop(workstack) <> nil) do
      begin
        // collect primary statements: begin bla; bla; blabla; end;
        if StackPeekAtTop(workstack).operation = CopCommandBlockMarker then
        begin
          tempNode := StackGetAndRemoveFromTop(workstack); // remove the marker
          FreeAndNil(tempNode);
          Break; // quit while loop...
        end
        else workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack));
      end;
      workstack.Add(workNode); // add the block itself...
    end
    else
    if (workNode.operation = CopArrayIndex) then // children: variable, index
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // read index
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // read variable
      workstack.Add(workNode);
    end
    else
    if (workNode.operation = CopFunctionArg) then
    begin
      workNode.children.Add(StackGetAndRemoveFromTop(workstack)); // read arg and push as child
      workstack.Add(workNode);
    end
    else
    if (workNode.operation = CopFunctionCall) then // children: <args>, 'writeln'
    begin
      // check for arguments
      while (StackPeekAtTop(workstack).operation = CopFunctionArg) do
      begin
        workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // add args to children...
      end;
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // read name ..., check on execution if children[0] is func-arg

      // add SysCall Index (all functions are Sys Calls for now, user defined would have an index of -1)
      workNode.symbol_index := getSysCallId(workNode.getChild(0).s);
      // clear string
      workNode.getChild(0).s := '';
      // add the node
      workstack.Add(workNode);
    end
    else
    if workNode.operation = CopCondition then // children: a = b, if command, <else command>
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // erster Befehl, muß immer da sein
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // zweiter befehl, im Zweifel ein NOP

      if not (isLogicOperation(StackPeekAtTop(workstack).operation) or (StackPeekAtTop(workstack).operation in [CopFunctionCall, CopVariable])) then
        report_error('Logic Operation expected for If!', workNode.line, CopCondition, C_Error);

      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // logic operation like 3 = 4 or function call
      workstack.Add(workNode);
    end
    else
    if workNode.operation = CopCase then // children: <number, command> (multiple) <closing command> (for else)
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // read the else command

      while (StackPeekAtTop(workstack) <> nil) do
      begin
        // check for marker, otherwise read number / function call pairs
        if StackPeekAtTop(workstack).operation = CopCaseMarker then
        begin
          tempNode := StackGetAndRemoveFromTop(workstack); // remove the marker
          FreeAndNil(tempNode);
          Break; // quit while loop...
        end
        else
        begin
          workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // statement
          workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // number
        end;
      end;

      // end of statements, read the variable / expression / function call etc
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // variable or expression
      workstack.Add(workNode);
    end
    else
    if workNode.operation = CopLoopWhile then // children: a > 0, command
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // Command

      if not (isLogicOperation(StackPeekAtTop(workstack).operation) or (StackPeekAtTop(workstack).operation in [CopFunctionCall, CopVariable])) then
        report_error('Logic Operation expected, found: ' + operationtostr(StackPeekAtTop(workstack).operation) + '!', workNode.line, CopLoopWhile, C_Error);

      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // logic operation like 3 = 4 or function call
      workstack.Add(workNode);
    end
    else
    if workNode.operation = CopLoopRepeatUntil then // children: command a <= 0
    begin
      if not (isLogicOperation(StackPeekAtTop(workstack).operation) or (StackPeekAtTop(workstack).operation in [CopFunctionCall, CopVariable])) then
        report_error('Logic Operation expected, found: ' + operationtostr(StackPeekAtTop(workstack).operation) + '!', workNode.line, CopLoopWhile, C_Error);

      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // logic operation like 3 = 4 or function call

      while (StackPeekAtTop(workstack) <> nil) do
      begin
        if StackPeekAtTop(workstack).operation = CopLoopRepeatMarker then
        begin
          tempNode := StackGetAndRemoveFromTop(workstack); // remove #repeat_marker
          FreeAndNil(tempNode);
          Break; // quit repeat loop...
        end
        else workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack));
      end;

      workstack.Add(workNode);
    end
    else
    if (workNode.operation = CopLoopForTo) or (workNode.operation = CopLoopForDownTo) then // children: var number number command
    begin
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // Command
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // to-number
      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // from-number

      if not (StackPeekAtTop(workstack).operation = CopVariable) then
        report_error('Count Variable expected, found: ' + operationtostr(StackPeekAtTop(workstack).operation) + '!', workNode.line, CopLoopForTo, C_Error);

      workNode.children.AddFirst(StackGetAndRemoveFromTop(workstack)); // variable

      workstack.Add(workNode);
    end
    else report_error('Unknown Operator!', workNode.line, workNode.operation, C_Error);
  end;

  // Check if the stack contains our final begin-end block and nothing else!
  if workstack.Count > 1 then
  begin
    tempNode := StackGetAndRemoveFromTop(workstack); // remove the valid begin-end block
    FreeAndNil(tempNode);
    report_error('Unknown Identifier! Unresolved Nodes: ' + IntToStr(workstack.Count)+ ', First Node: ' + StackPeekAtTop(workstack).s, StackPeekAtTop(workstack).line, StackPeekAtTop(workstack).operation, C_Error);
  end;

  if CtpDebugMode then
  begin
    writeln;
    writeln('# Resulting Tree:');
    workNode.debug(0, symbol_table); // final workNode = tree root!

    writeln;
    // symbol table could be saved and added to binary / interpreter for debugging purpose...
    writeln('# Resulting Symbol Table:');
    for i := 0 to symbol_table.Count - 1 do
    writeln('Var_' + IntToStr(i) + ' = ' + symbol_table.Strings[i]);

    if symbol_table.Count < 1 then writeln('No Symbols.');
    writeln;
  end;

  FreeAndNil(stack);
  FreeAndNil(workstack);
  FreeAndNil(symbol_table);

  Result := workNode;
end;

end.

