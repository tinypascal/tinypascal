unit tp_error_reporting;

{$mode objfpc}

interface

uses Classes, SysUtils;

const C_Error = 0;
      C_Warning = 1;
      C_Notice = 2;

procedure report_error(s: String; line, operation, errortype: Integer);

var global_error: Boolean;

implementation

uses tp_utils;

procedure report_error(s: String; line, operation, errortype: Integer);
var prefix: String;
begin
//  if global_error then Exit; // uncomment to only report the first error...

  case errortype of
    C_Error:     prefix := 'Error! ';
    C_Warning:   prefix := 'Warning! ';
    C_Notice:    prefix := 'Notice! ';
    else         prefix := '';
  end;

  if operation <> -1 then
    WriteLn(prefix + 'Operation ' + operationtostr(operation) + ': ' + s + ' at Line ' + IntToStr(line) + '. (press Enter)')
  else
    WriteLn(prefix + s + ' at Line ' + IntToStr(line) + '. (press Enter)');

  if errortype = C_Error then global_error := true;
end;


begin
  global_error := false;
end.

