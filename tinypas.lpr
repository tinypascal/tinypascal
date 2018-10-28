program tinypas;

{$mode objfpc}{$H+}

uses Classes, sysutils, tp_lexer, tp_utils, tp_parser, tp_syntax_tree,
  tp_constants, tp_interpreter, dateutils, tp_tinylist, tp_error_reporting;

// imports a value from an outer world variable, identified by a keyword
function tp_import(keyword: String): String;
begin
  if keyword = 'max_hp' then Result := '100';
end;

// exports the content to an outer world variable, identified by a keyword
procedure tp_export(keyword, content: String);
begin
  writeln('Exporting ', keyword,' = ', content);
end;

// executes a user defined procedure with the given parameter
procedure tp_user(paramList: TStringList);
var i: Integer;
begin
  write('User Procedure ');

  for i := 0 to paramList.Count - 1 do
  write('''', paramList.Strings[i], ''' ');

  writeln;
end;

var tokenlist, stacklist: TTinyList;
    rootnode: TTreeNode;
    time0, time1: TDateTime;

label L_ERROR, L_DONE;

begin
  tp_import_func := @tp_import;
  tp_export_proc := @tp_export;
  tp_user_proc := @tp_user;

  CtpDebugMode := true; // set this to false if you want to use the project inside another!

  time0 := Now;
  time1 := time0;
  writeln('--- Start        ', TimeToStr(time1));

  writeln('Lexer start');

  // there are two ways to start the interpreter, using a filename or by passing a string and the parameter true

  tokenlist := tp_lex_file('a := 5; if a > 3 then writeln(2*a);', true);
  //tokenlist := tp_lex_file('test.tp');

  if global_error then goto L_ERROR;
  writeln('Lexer done       ', TimeToStr(Now), ' (', SecondsBetween(time1, Now), 's)');
  time1 := Now;


  writeln('Parser start');
  stacklist := tp_parse(tokenlist);
  if global_error then goto L_ERROR;
  writeln('Parser done      ', TimeToStr(Now), ' (', SecondsBetween(time1, Now), 's)');
  time1 := Now;

  writeln('Syntax Tree start');
  rootnode := tp_build_syntax_tree(stacklist);
  if global_error then goto L_ERROR;
  writeln('Syntax Tree done ', TimeToStr(Now), ' (', SecondsBetween(time1, Now), 's)');

  time1 := Now;

  writeln('Interpreter start');
  tp_interpret(rootnode);
  if global_error then goto L_ERROR;
  writeln('Interpreter done ', TimeToStr(Now), ' (', SecondsBetween(time1, Now), 's)');
  time1 := Now;

  goto L_DONE;

  L_ERROR:
  writeln('An Error occured!');

  L_DONE:
  writeln('--- Ready        ', TimeToStr(Now), ' (', SecondsBetween(Time0, Now), 's)');

  readln;
end.

