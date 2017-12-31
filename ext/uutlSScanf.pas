unit uutlSScanf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TutlSScanfOption = (soRepeatableWhitespace);
  TutlSScanfOptions = set of TutlSScanfOption;

function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; Options: TutlSScanfOptions = []): Integer;
function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; const FormatSettings: TFormatSettings; Options: TutlSScanfOptions = []): Integer;

implementation

function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; Options: TutlSScanfOptions): Integer;
begin
  Result:= utlSScanf(s, fmt, Pointers, DefaultFormatSettings, Options);
end;

{
   0 on success (perfect match)
  +N if fmt was consumed without error, but there are characters left in s. First unparsed character is N
  -N if there was an error consuming pattern character N

  %s: string, must be terminated by a fixed string in fmt
  %d: integer
  %D: int64
  %f: double, supports scientific notation
  %P: hex PtrUInt (fixed length)
  %x: hex integer
  %X: hex int64

  Options:
     soRepeatableWhitespace:    Treat any run of (the same) whitespace in fmt as a single character, which matches any whitespace of that type
}
function utlSScanf(const s: string; const fmt: string; const Pointers: array of Pointer; const FormatSettings: TFormatSettings; Options: TutlSScanfOptions): Integer;
var
  pt, fpos, tokfpos, spos:  integer;
  token, limit: string;
  limitChar: Char;
  success: Boolean;

  function nextToken(advance: boolean): string;
  var
    oldfpos: integer;
  begin
    Result:= '';
    oldfpos:= fpos;
    try
      if fpos > Length(fmt) then
        Exit;

      Result:= fmt[fpos];
      inc(fpos);
      if Result = '%' then begin
        if fpos > Length(fmt) then
          Exit;                    // incomplete token, treat as literal %

        Result += fmt[fpos];
        inc(fpos);

        if Result = '%%' then
          Result:= '%';
      end;
      if (Result <= ' ') and (soRepeatableWhitespace in Options) then begin
        while (fpos <= Length(fmt)) and (fmt[fpos] = Result) do
          inc(fpos);
      end;
    finally
      if not advance then
        fpos:= oldfpos
      else
        tokfpos:= oldfpos;
    end;
  end;

  function GetString: boolean;
  var
    tmp: string;
  begin
    tmp:= '';
    Result:= false;
    if limitChar = #0 then begin
      tmp:= Copy(s, spos, MaxInt);
      inc(spos, length(tmp));
      Result:= true;
    end else begin
      while spos <= Length(s) do begin
        if s[spos] = limitChar then begin
          dec(spos); // consume again
          Result:= true;
          break;
        end;
        tmp:= tmp + s[spos];
        inc(spos);
      end;
    end;
    PString(Pointers[pt])^:= tmp;
  end;

  function GetInteger(i64: boolean): Boolean;
  var
    tmp: string;
    v: int64;
  begin
    tmp:= '';
    Result:= false;
    while spos <= Length(s) do begin
      if s[spos] = limitChar then begin
        dec(spos); // consume again
        break;
      end;
      tmp:= tmp + s[spos];
      if not TryStrToInt64(tmp, v) then begin
        dec(spos);
        SetLength(tmp, Length(tmp) - 1);
        Break;
      end;
      inc(spos);
    end;
    if TryStrToInt64(tmp, v) then begin
      if i64 then
        PInt64(Pointers[pt])^:= v
      else
        PInteger(Pointers[pt])^:= v;
      Result:= true;
    end;
  end;

  function GetFloat: Boolean;
  var
    tmp: string;
    v: double;
    lastChar: char;
    firstFailure, fflen: Integer;
  begin
    tmp:= '';
    Result:= false;
    firstFailure:= 0;
    while spos <= Length(s) do begin
      if s[spos] = limitChar then begin
        dec(spos); // consume again
        break;
      end;
      tmp:= tmp + s[spos];
      if not TryStrToFloat(tmp, v, FormatSettings) then begin
        lastChar:= tmp[length(tmp)];
        if (firstFailure = 0) and
           ((lastChar = FormatSettings.ThousandSeparator) or (lastChar in ['-','E'])) then begin
          firstFailure:= spos;
          fflen:= Length(tmp);
        end else begin
          Break;
        end;
      end else
        firstFailure:= 0;
      inc(spos);
    end;
    if firstFailure > 0 then begin
      spos:= firstFailure;
      dec(spos);
      SetLength(tmp, fflen - 1);
    end;

    if TryStrToFloat(tmp, v, FormatSettings) then begin
      PDouble(Pointers[pt])^:= v;
      Result:= true;
    end;
  end;

  function GetHex(mode: byte): Boolean;
  const
    ptrlen =  Sizeof(PtrUInt) * 2;
  var
    tmp: string;
    v: int64;
  begin
    tmp:= '$';
    Result:= false;
    if mode = 0 then begin
      tmp += Copy(s, spos, ptrlen);
      inc(spos, length(tmp));
      if Length(tmp) <> ptrlen+1 then
        Exit;
    end else
      while spos <= Length(s) do begin
        if s[spos] = limitChar then begin
          dec(spos); // consume again
          break;
        end;
        tmp:= tmp + s[spos];
        if not TryStrToInt64(tmp, v) then begin
          dec(spos);
          SetLength(tmp, Length(tmp) - 1);
          Break;
        end;
        inc(spos);
      end;
    if TryStrToInt64(tmp, v) then begin
      case mode of
        0: PPtrUInt(Pointers[pt])^:= v;
        1: PInteger(Pointers[pt])^:= v;
        2: PInt64(Pointers[pt])^:= v;
      end;
      Result:= true;
    end;
  end;

begin
  pt:= 0;
  fpos:= 1;
  spos:= 1;
  while fpos <= length(fmt) do begin
    if spos > Length(s) then
      Exit(-fpos);

    token:= nextToken(true);

    if token = '' then
      Exit(-tokfpos);

    if Length(token) = 1 then begin
      if token <> s[spos] then
        Exit(-tokfpos);
      if (s[spos] <= ' ') and (soRepeatableWhitespace in Options) then begin
        while (spos<Length(s)) and (s[spos+1]=token) do
          inc(spos);
      end;
    end else begin
      limit:= nextToken(false);
      if Length(limit) = 1 then
        limitChar:= limit[1]
      else
        limitChar:= #0;

      case token[2] of
        's': success:= GetString;
        'd': success:= GetInteger(false);
        'D': success:= GetInteger(True);
        'f': success:= GetFloat;
        'P': success:= GetHex(0);
        'x': success:= GetHex(1);
        'X': success:= GetHex(2);
      else
        Exit(-tokfpos);
      end;
      if not success then
        Exit(-tokfpos);

      inc(pt);
    end;
    inc(spos);
  end;
  if spos <= Length(s) then
    Exit(spos)
  else
    Exit(0);
end;

end.

