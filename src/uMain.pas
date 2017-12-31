unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, StdCtrls, ExtCtrls,
  uLinkerMap;

type
  TForm1 = class(TForm)
    clbLegend: TColorListBox;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public
    fEntries: TLinkEntries;
    fImageName: string;
    fImageWordWidth: integer;
    fImageSize, fImageBase: int64;
    fMap: TBitmap;
    procedure LoadFile(fname: string);
  end;

var
  Form1: TForm1;

implementation

uses
  FileCtrl;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fEntries:= TLinkEntries.Create();
  fMap:= TBitmap.Create;
  LoadFile(ParamStr(1));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fMap);
  FreeAndNil(fEntries);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  case UpCase(Key) of
    'R': LoadFile(fImageName);       
    'L': clbLegend.Visible:= not clbLegend.Visible;   
    '1','2','4','8': begin
      PIXEL_ALIGN:= StrToInt(Key);
      mapPaintToBitmap(fEntries, fImageSize, fMap);
      Invalidate;
    end;
  end;
end;

procedure TForm1.LoadFile(fname: string);
var
  Entry: TEntry;
  pc: TColor;
  f: String;
begin
  mapLoadFile(ParamStr(1), fEntries, fImageSize, fImageBase);
  if fImageBase > $ffffffff then
    fImageWordWidth:= 8
  else
    fImageWordWidth:= 4;
  PIXEL_ALIGN:= fImageWordWidth div 2;
  fImageName:= fname;
  Caption:= Format('%s Base %0.*x Size %.*x', [ExtractFileName(ParamStr(1)), fImageWordWidth*2, fImageBase, fImageWordWidth*2, fImageSize]);
  Invalidate;
  clbLegend.Clear;
  for Entry in fEntries do begin
    f:= ExtractFileName(Entry.src);
    if clbLegend.Items.IndexOf(f) < 0 then begin
      pc:= ColorOfEntry(Entry);
      clbLegend.Items.AddObject(f, TObject(PtrInt(pc)));
    end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0,0, fMap);
end;

procedure TForm1.PaintBox1Resize(Sender: TObject);
begin
  if not Assigned(fEntries) then Exit;
  if not Assigned(fMap) then Exit;
  fMap.PixelFormat:= pf32bit;
  fMap.SetSize(PaintBox1.ClientWidth, PaintBox1.ClientHeight);
  mapPaintToBitmap(fEntries, fImageSize, fMap);
  Invalidate;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pc: TColor;
  i: Integer;
begin
  clbLegend.MultiSelect:= True;
  clbLegend.ClearSelection;
  pc:= fMap.Canvas.Pixels[X, Y];
  for i:= 0 to clbLegend.Items.Count-1 do
    TCustomListBox(clbLegend).Selected[i]:= clbLegend.Colors[i] = pc;
end;

end.

