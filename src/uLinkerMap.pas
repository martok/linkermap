unit uLinkerMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fgl, Math, Graphics, uutlSScanf;

type
  TEntry = class
    Start, Len: Int64;
    src: string;
  end;
  TLinkEntries = specialize TFPGObjectList<TEntry>;


procedure mapLoadFile(fname: string; Entries: TLinkEntries; out ImageSize, ImageBase: int64);
procedure mapPaintToBitmap(Entries:TLinkEntries; ImageSize: Int64; Bitmap: TBitmap);

function ColorOfEntry(Entry: TEntry): TColor;   

var
  PIXEL_ALIGN: integer         = 2;

implementation

uses
  sha1;


procedure mapLoadFile(fname: string; Entries: TLinkEntries; out ImageSize,
  ImageBase: int64);
var
  mapf: TextFile;
  mapformat: (mfUnknown, mfFPCInternal, mfGNULd);
  line: string;
  a, offset, size: Int64;
  objfile, sdummy: String;
  e: TEntry;
begin
  AssignFile(mapf, fname);
  Reset(mapf);
  try
    mapformat:= mfUnknown;
    repeat
      ReadLn(mapf, line);
      line:= trim(line);
      if 0=utlSScanf(line, 'Memory map  (ImageBase=0x%X)', [@Imagebase], [soRepeatableWhitespace]) then begin
        mapFormat:= mfFPCInternal;
        break;
      end;
      if 0=utlSScanf(line, '0x%X   ___ImageBase = 0x%X', [@a, @Imagebase], [soRepeatableWhitespace]) then begin
        mapFormat:= mfGNULd;
        break;
      end;
    until eof(mapf);
    if mapformat = mfUnknown then begin
      Writeln('ERROR: Unknown Map Format');
      Exit;
    end;
    // filter for object entry lines
    Entries.Clear;
    ImageSize:= 0;
    repeat
      ReadLn(mapf, line);
      line:= Trim(line);
      if (0=utlSScanf(line, '0x%X 0x%X %s', [@offset, @size, @objfile], [soRepeatableWhitespace])) or
         (0=utlSScanf(line, '%s 0x%X 0x%X %s', [@sdummy, @offset, @size, @objfile], [soRepeatableWhitespace])) then begin
        e:= TEntry.Create;
        e.Start:= offset - ImageBase;
        e.Len:= Size;
        ImageSize:= Max(ImageSize, e.Start + e.Len);
        e.src:= objfile;
        Entries.Add(e);
      end;
    until eof(mapf);
  finally
    CloseFile(mapf);
  end;
end;

function ColorOfEntry(Entry: TEntry): TColor;
var
  Digest: TSHA1Digest;
  A: array[0..4] of Cardinal absolute Digest;
begin
  Digest:= SHA1String(UpperCase(ExtractFileName(Entry.src)));
  Result:= A[1] and $FFFFFF;
end;

procedure mapPaintToBitmap(Entries:TLinkEntries; ImageSize: Int64; Bitmap: TBitmap);
var
  C: TCanvas;
  w,h: integer;
  pc: TColor;
  Entry: TEntry;

  procedure DrawDots(First, Last: integer; Col: TColor);
  var
    i, y, x: Integer;
  begin
    for i:= First to Last do begin
      y:= i div w;
      x:= i - w*y;
      C.Pixels[X,Y]:= Col;
    end;
  end;

begin
  c:= Bitmap.Canvas;
  w:= Bitmap.Width;
  h:= Bitmap.Height;
  C.Brush.Color:= clBlack;
  C.FillRect(0, 0, w, h);
  DrawDots(ImageSize div PIXEL_ALIGN+1, w*h, clWhite);
  for Entry in Entries do begin
    pc:=ColorOfEntry(Entry);
    DrawDots(Entry.Start div PIXEL_ALIGN, Max(1, (Entry.Start + Entry.Len) div PIXEL_ALIGN), pc);
  end;
end;

end.

