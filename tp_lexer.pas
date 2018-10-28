unit tp_lexer;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_tinylist;

type
  TToken = class(TObject)
             id: Byte;
             s: String;
             line: Integer;
           end;

function tp_lex_file(fname: String; stringmode: Boolean = false): TTinyList;

implementation

uses tp_constants, tp_utils, tp_error_reporting;

{
input:  a file or a string with the commands
output: a list of recognized token of the following types

CtokenIdentifier    variables or function calls, also keywords like begin end
CtokenOperator      : = < > + - * /
CtokenString        'anything like this' '' is also supported
CtokenNumber        100 100.34 -100 is NOT a vaid number it is replaced with 0 - 100 and added as 100
CtokenComment       // and {}, they will be discarded
CtokenSingle        any single character left unrecognized, such as ; , ( ) [ ]
}
function tp_lex_file(fname: String; stringmode: Boolean = false): TTinyList;
var f: Text;
    c: Char;
    line: Integer;
    tokenlist: TTinyList;

// preview next char
function next_ch: Char;
begin
  Result := c;
end;

// return buffer and fill it with the next char from file
function get_ch: Char;
begin
  if c = #10 then inc(line); // detect line feeds in Unix AND Windows

  Result := c;

  if stringmode then // stringmode reads the command from the fname string directly
  begin
    if fname = '' then c := #0
    else
    begin
      c := fname[1];
      Delete(fname, 1, 1);
    end;
  end
  else
  begin
    if EOF(f) then c := #0 // we won't allow #0 in source code, so it can be used...
              else Read(f, c);
  end;
end;

procedure skip_whitespace;
begin
  while (tp_is_whitespace(next_ch)) do get_ch;
end;

// recognizes and reads (= deletes from input stream) a token
procedure tp_lex_read_token;
var s: String;
    id: Byte;
    token: TToken;
    nest_depth, current_line: Integer;
begin
  s := get_ch;

  if tp_is_alpha(s[1]) then // Identifier: keywords, variables, functions
  begin
    id := CtokenIdentifier;
    while ((tp_is_alpha(next_ch)) or (tp_is_numeric(next_ch)) or (next_ch = '_')) do s := s + get_ch;
  end
  else
  if (s[1] = '/') and (next_ch = '/') then // single line comment: // example comment
  begin
    id := CtokenComment;
    get_ch; // skip second /
    current_line := line;
    // skip until next line or end of file
    while (line = current_line) and (next_ch <> #0) do get_ch;
  end
  else
  if tp_is_operator(s[1]) then // Operator: + - * / > < = :
  begin
    // exception for negative number or additional + ...
    if ((s[1] = '-') or (s[1] = '+')) and (tp_is_numeric(next_ch)) then
    begin
      id := CtokenNumber;
      while (tp_is_numeric(next_ch) or (next_ch = '.')) do s := s + get_ch;
    end
    else
    begin
      id := CtokenOperator;
      while (tp_is_operator(next_ch)) do s := s + get_ch;
    end
  end
  else
  if s[1] = '''' then // String: 'It''s working!'
  begin
    id := CtokenString;
    s := ''; // clear string, we don't want the leading ' char in our internal string

    while ((next_ch <> #0)) do
    begin
      case next_ch of
        '''': begin
                get_ch; // skip the closing ' and break unless we have the '' exception to continue
                if next_ch <> '''' then Break;
              end;
        #10: report_error('String not terminated', line, -1, C_Warning);
      end;

      s := s + get_ch;
    end;
  end
  else
  if tp_is_numeric(s[1]) then // Number: 78 or 0.234 + and - are ignored and evaluated later
  begin
    id := CtokenNumber;
    while (tp_is_numeric(next_ch) or (next_ch = '.')) do s := s + get_ch;
  end
  else
  if s[1] = '{' then // multi line comment: {just like this...}, does not (yet) handle this exception { writeln('}'); }
  begin
    id := CtokenComment;
    nest_depth := 1;

    while ((nest_depth > 0) and (next_ch <> #0)) do
    begin
      get_ch;
      if next_ch = '{' then inc(nest_depth);
      if next_ch = '}' then dec(nest_depth);
    end;

    get_ch; // get ending }
  end
  else id := CtokenSingle; // single token: ( ) ;

  // ignore comments, add everything else
  if id <> CtokenComment then
  begin
    token := TToken.Create;
    token.id := id;
    token.s := s;
    token.line := line;

    tokenlist.Add(token);
  end;
  skip_whitespace; // skips any whitespace between two commands, before or after a linebreak doesn't matter
end;

var i: Integer;
begin
  Result := nil;

  if not stringmode then
  if not FileExists(fname) then Exit;

  if CtpDebugMode then writeln('*** Lexer ***');

  tokenlist := TTinyList.Create;
  line := 1; // start at line 1 of file or string

  if not stringmode then
  begin
    AssignFile(f, fname);
    Reset(f);
  end;

  get_ch; // fill "next_ch" buffer for first time use

  skip_whitespace; // skip the initial whitespace in the file, if any
  while (next_ch <> #0) and (not global_error) do tp_lex_read_token; // #0 is our internal symbol for end of file / input, it is NOT allowed as character in the sourcecode!

  if not stringmode then CloseFile(f);

  // Debug: output our list of token...
  if CtpDebugMode then
  for i := 0 to tokenlist.Count - 1 do
  begin
    with TToken(tokenlist.Items(i)) do
    WriteLn(IntToStr(line) + ' ' + tokentostr(id) + ': ' + s);
  end;

  Result := tokenlist;
end;

end.

