unit tp_parser;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_tinylist;

function tp_parse(tokenlist: TTinyList): TTinyList; // Stack of TTreeNodes

implementation

uses tp_constants, tp_lexer, tp_utils, tp_syntax_tree, tp_error_reporting;

{
input:  a list of token from the lexer
output: a linear list of nodes containing the following

operation        Index of the Operation to be performed. For example Variable, Condition, WhileLoop, Assignment
s                Content of the Operation. Used for primary expressions like numbers, variables, function calls e.g. 100 or writeln
symbol_index     Cache for all variables. Their identifier is reduced to an index for direct access
line             Source Line where this Operation was originally found
children         List of Sub Nodes. If has for example 3 Sub Nodes. <Condition> <Code, if cond. is true> <Code, for else> --> a = 5, writeln('correct!'), writeln('wrong')
}
function tp_parse(tokenlist: TTinyList): TTinyList;
var stack: TTinyList;
    error_token: TToken;

// creates a new node element and pushes it to the main stack of nodes
procedure add_node(operation: Byte; s: String; line: Integer);
begin
  stack.Add(create_node(operation, s, line));
end;

// reads a token from the input
function preview_token: TToken;
begin
  Result := nil;

  if tokenlist.Count < 1 then
  begin
    report_error('Unexpected End of Program', 0, -1, C_Error);
    Result := error_token;
  end
  else Result := TToken(tokenlist.GetFirst);
end;

// reads a token from the input AND deletes it (pop)
procedure read_token;
var token: TToken;
begin
  if global_error then Exit;

  token := preview_token;
  tokenlist.RemoveFirst;
  FreeAndNil(token);
end;

// compares the string with the token, peek() does NOT read the token!
function peek(s: String): Boolean;
begin
  if tokenlist.Count < 1 then Result := false
  else                        Result := (UpperCase(s) = UpperCase(preview_token.s));
end;

// compares the string and reads (accepts) the token on a match
function accept(s: String): Boolean;
begin
  Result := false;
  if peek(s) then
  begin
//    if CtpDebugMode then writeln('ACCEPTED ' + preview_token.s);
    read_token;
    Result := true;
  end;
end;

// the token is read and the string must match
procedure expect(s: String);
begin
//  if CtpDebugMode then writeln('EXPECT ' + s);
  if not accept(s) then report_error(s + ' expected, but ' + preview_token.s + ' found', preview_token.line, -1, C_Error);
end;

// needed for primary expressions to read complex, recursive constructs -> go back to the beginning of parsing
procedure expr; forward;

{
Every pascal statement (like conditions and loops) consists of expressions. The following code tries to
match the expressions, starting with primary expressions (such as numbers and variables) down to
more complex ones (like math operations, logic).

Every expression procedure first calls the procedure one level above itself and tries to find a match for the following token.
After the procedure returns, the next match is attempted. Anything matching inside the levels above the current procedure
was already parsed and is out of the way in the token list. For example does a for statement parse <for> ... <:=> ... <to> ... <do>
but it doesn't parse the expressions like variables or math equations. It is calling the according procedures above it instead,
like prim_expr (for variables) and add_expr (for math expressions).

A more detailed breakdown of "for i := 1 to 2+3 do foobar;"

First, <for> is recognized as statement. The statement procedure calls the procedure to parse primary expressions to parse
the variable. The check if it really was a variable is done later, when building the syntax tree. It matches CtokenIdentifier <i>
and the code goes back to the statement. The next token is expected to be <:=>, no exceptions. Luckily it is there and the parser
is fetching the beginning of the loop range, which is the primary expression CtokenNumber <1>.

The code goes back to the statement and has the choice between <to> and <downto> as token, of which <to> is there. The final
expression is the end of the loop range, which is at first a simple CtokenNumber <2>, but it matches add_expr; on the way back and
recognizes the <+>. The add_expr keeps looking for additional + or - and calls the next higher procedure to check for expressions
like 4*5 or (4/2) etc. However the next expression is again a simple CtokenNumber <3> (and no + or -). The code returns and
expects <do> as token, which is there.

Finally the next statement is read within the for statement, which could be a begin / end block, a procedure call <foobar> or another loop.
If the statement was read successfully, the code returns to the main loop and tries to read the next statement.
}

// primary expressions
procedure prim_expr;
var token: TToken;
    sys_token: Boolean;
begin
  if global_error then Exit;

  token := preview_token;
  if token.id = CtokenNumber then
  begin
    add_node(CopNumber, token.s, token.line); // Number 0123.4
    read_token;
  end
  else
  if token.id = CtokenString then
  begin
    add_node(CopString, token.s, token.line); // String ''
    read_token;
  end
  else
  if accept('not') then
  begin
    expr;
    add_node(CopLogicNot, '', preview_token.line); // not
  end
  else
  if accept('true') then
  begin
    add_node(CopLogicTrue, '', preview_token.line); // True
  end
  else
  if accept('false') then
  begin
    add_node(CopLogicFalse, '', preview_token.line); // False
  end
  else
  if token.id = CtokenIdentifier then // variable or procedure without a parameter!
  begin
    sys_token := isSysCall(token.s); // check here for sys calls, the token is lost after it was read, but we need this information

    // if its a syscall, it gets added as string literal
    if sys_token then add_node(CopString, token.s, token.line)
                 else add_node(CopVariable, token.s, token.line);

    // check for reserved identifiers!
    if (not sys_token) and (Pos('*' + LowerCase(token.s) + '*', CReservedIdentifier) > 0) then report_error('Reserved identifier used! (' + token.s + ')', token.line, -1, C_Error);

    // read and consume token, so peek() in the next line works and looks at the next token which is now the first
    read_token;

    // if the token was recognized as sys call like writeln; it is added, if it is writeln('hi'); it will be recognized later in postfix_expr
    if not peek('(') and sys_token then add_node(CopFunctionCall, '', token.line);
  end
  else
  if accept('(') then
  begin
    expr;
    expect(')');
    // we don't need to add any braket elements, the structure already takes care of the right execution order
  end
  else report_error('Unexpected primary token ''' + token.s + '''', token.line, -1, C_Error);
end;

// function calls and array / string index
procedure postfix_expr;
var line: Integer;
begin
  if global_error then Exit;
  prim_expr;
  if global_error then Exit;

  line := preview_token.line;

  if accept('[') then // arrays, strings etc.
  begin
    expr;
    expect(']');
    add_node(CopArrayIndex, '', line); // []
  end
  else
  if accept('(') then // function, procedures, normal brakets like (1+5)*3 are recognized before as PRIMARY expressions!
  begin
    if not accept(')') then // allows for function()
    begin
      expr;
      add_node(CopFunctionArg, '', line); // function argument - a function with NO arguments is recognized before, as PRIMARY expression!
      while accept(',') and (not global_error) do
      begin
        expr;
        add_node(CopFunctionArg, '', line); // look for more function arguments
      end;
      expect(')');
    end;

    add_node(CopFunctionCall, '', line); // function
  end;
end;

// order of math operations is set by the order of procedure calls
// calling the next higher one as first line in the lower one recognizes them before
procedure mul_expr;
var line: Integer;
begin
  if global_error then Exit;
  postfix_expr;
  if global_error then Exit;

  line := preview_token.line;
  while (peek('*') or peek('/') or peek('mod') or peek ('div')) and (not global_error) do
  begin
    if accept('*') then
    begin
      postfix_expr;
      add_node(CopMathMul, '', line); // *
    end
    else
    if accept('/') then
    begin
      postfix_expr;
      add_node(CopMathDiv, '', line); // /
    end
    else
    if accept('mod') then
    begin
      postfix_expr;
      add_node(CopMathMod, '', line); // mod
    end
    else
    if accept('div') then
    begin
      postfix_expr;
      add_node(CopMathDivInt, '', line); // div
    end;
  end;
end;

// lower math expressions
procedure add_expr;
var line: Integer;
begin
  if global_error then Exit;
  mul_expr;
  if global_error then Exit;

  line := preview_token.line;
  while (peek('+') or peek('-')) and (not global_error) do
  begin
    if accept('+') then
    begin
      mul_expr; // checks for higher expressions like the 2*3 in 1+2*3
      add_node(CopMathAdd, '', line); // +
    end
    else
    if accept('-') then
    begin
      mul_expr;
      add_node(CopMathSub, '', line); // -
    end;
  end;
end;

// compare operators
procedure eq_expr;
var line: Integer;
begin
  if global_error then Exit;
  add_expr;
  if global_error then Exit;

  line := preview_token.line;
  if accept('=') then
  begin
    add_expr;
    add_node(CopCompareEqual, '', line); // =
  end
  else
  if accept('<>') then
  begin
    add_expr;
    add_node(CopCompareUnequal, '', line); // <>
  end
  else
  if accept('>') then
  begin
    add_expr;
    add_node(CopCompareGreater, '', line); // >
  end
  else
  if accept('<') then
  begin
    add_expr;
    add_node(CopCompareSmaller, '', line); // <
  end
  else
  if accept('>=') then
  begin
    add_expr;
    add_node(CopCompareGreaterOrEqual, '', line); // >=
  end
  else
  if accept('<=') then
  begin
    add_expr;
    add_node(CopCompareSmallerOrEqual, '', line); // <=
  end;
end;

// logic
procedure logic_expr;
var line: Integer;
begin
  if global_error then Exit;
  eq_expr;
  if global_error then Exit;

  line := preview_token.line;
  while (peek('and') or peek('or')) and (not global_error) do
  begin
    if accept('and') then
    begin
      eq_expr;
      add_node(CopLogicAnd, '', line); // and
    end
    else
    if accept('or') then
    begin
      eq_expr;
      add_node(CopLogicOr, '', line); // or
    end;
  end;
end;

// assignment
procedure expr;
var line: Integer;
begin
  if global_error then Exit;
  logic_expr;
  if global_error then Exit;

  line := preview_token.line;

  if accept(':=') then
  begin
    expr; // get right side of assignment...
    add_node(CopAssign, '', line); // :=
  end
end;

// recognizes a statement like begin end block or a for to do loop
procedure statement;
var line: Integer;
begin
  if global_error then Exit;

  line := preview_token.line;

  if accept('begin') then
  begin
    add_node(CopCommandBlockMarker, '', line); /// marking the start of the statement

    while not accept('end') and (not global_error) do statement;

    if not peek('else') then expect(';'); // exception for else, no ; before it! otherwise expect end;

    add_node(CopCommandBlock, '', line); // begin-end
  end
  else
  if accept('if') then
  begin
    logic_expr; // condition
    expect('then');
    statement; // statement
    if accept('else') then statement // statement for else
    else
    add_node(CopNOP, '', line); // else statement has to exist for easier parsing, add No OPeration if missing

    add_node(CopCondition, '', line); // if
  end
  else
  if accept('case') then
  begin
    logic_expr; // expression to switch cases for
    expect('of');

    add_node(CopCaseMarker, '', line); // needed, so the end of statements can be detected

    while not (peek('else') or peek('end')) and (not global_error) do
    begin
      add_expr; // 4
      expect(':');
      statement; // statement
    end;

    if accept('else') then statement
    else
    add_node(CopNOP, '', line); // else statement has to exist for easier parsing, add No OPeration if missing

    expect('end');
    expect(';');

    add_node(CopCase, '', line); // case
  end
  else
  if accept('while') then
  begin
    logic_expr; // condition
    expect('do');
    statement; // statement
    add_node(CopLoopWhile, '', line); // while
  end
  else
  if accept('repeat') then
  begin
    add_node(CopLoopRepeatMarker, '', line); // marking the start of the statement

    while not accept('until') and (not global_error) do statement;

    logic_expr; // read the condition
    expect(';');
    add_node(CopLoopRepeatUntil, '', line); // repeat
  end
  else
  if accept('for') then // for i := 3 to 5 do
  begin
    prim_expr; // variable i
    expect(':=');
    add_expr; // 3
    if accept('to') then
    begin
      add_expr; // 5
      expect('do');
      statement; // looped statement
      add_node(CopLoopForTo, '', line); // for to do
    end
    else
    begin
      expect('downto');
      add_expr; // 5
      expect('do');
      statement; // looped statement
      add_node(CopLoopForDownTo, '', line); // for downto do
    end
  end
  else
  begin
    expr;
    // exception for everything not needing a ; after it like if a = 5 then, for i := 1 to 5 do etc.
    // else and then statement are needed here aswell, because conditions can contain statements like if isNumber(x) then
    // and after the statement a ; is required, or in this case not required...
    if not (peek('then') or peek('else') or peek('do') or peek('to') or peek('downto')) then expect(';');
  end;
end;

var token: TToken;
    i: Integer;
    node: TTreeNode;
begin
  if tokenlist = nil then Exit;

  if CtpDebugMode then
  begin
    writeln;
    writeln('*** Parser ***');
  end;

  stack := TTinyList.Create; // create a new stack...

  // create dummy token for easier error handling...
  error_token := TToken.Create;
  error_token.id := 255; // unknown token
  error_token.s := '<EOF>'; // unknown string
  error_token.line := -1; // unknown line

  add_node(CopCommandBlockMarker, '', 0); // begin marker

  // accept our language....
  while (tokenlist.Count > 0) and (not global_error) do statement;

  // add a begin end block as top node, so every statement will be parsed one after another
  add_node(CopCommandBlock, '', 0); // begin-end

  // the syntax of the language is coded into the statement and expression procedures,
  // the parser is basically a state machine and accepts any valid program in the given language,
  // if the program is not valid, an error along the way is printed out
  if CtpDebugMode then
  begin
    writeln;
    writeln('# Program accepted');
  end;

  // the result of this operation is a huge stack in RPN (reverse polish notation) form of arguments
  // 1 + 2 becomes 1 2 +, which is very easy to parse for e.g. a stack based machine
  if CtpDebugMode then
  begin
    for i := 0 to stack.Count - 1 do
    begin
      node := TTreeNode(stack.Items(i));
      write(operationtostr(node.operation));
      if node.s <> '' then write('(' + node.s + ') ')
                      else write(' ');
    end;
    writeln;
  end;

  // finally delete the token list...
  for i := 0 to tokenlist.Count - 1 do
  begin
    token := TToken(tokenlist.Items(i));
    FreeAndNil(token);
  end;

  FreeAndNil(tokenlist);
  if error_token <> nil then FreeAndNil(error_token); // it is nil, if it was used and freed before!

  Result := stack;
end;

end.
