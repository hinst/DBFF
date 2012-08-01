unit ZenGLFCLGraphics;

{$mode objfpc}{$H+}

interface

uses
  FPimage,
  SysUtils,

  zgl_textures;

type

  { TMultiTexture }

  TMultiTexture = object
  public
    constructor Init;
  private
    fTexture: zglPTexture;
    fCount: integer;
    fWidth: integer;
    fHeight: integer;
    procedure Clean;
    procedure Allocate(const aWidth, aHeight: integer);
  public
    property Texture: zglPTexture read fTexture;
    property Count: integer read fCount write fCount;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    procedure Add(const aTexture: zglPTexture);
  end;

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;

implementation

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;
begin
  result :=0;
  result += (aColor.red shr 8) * 256 * 256;
  result += (aColor.green shr 8) * 256;
  result += (aColor.blue shr 8) * 1;
end;

{ TMultiTexture }

constructor TMultiTexture.Init;
begin
  Clean;
end;

procedure TMultiTexture.Clean;
begin
  fTexture := nil;
  fCount := -1;
  fWidth := 0;
  fHeight := 0;
end;

procedure TMultiTexture.Allocate(const aWidth, aHeight: integer);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  if Count = -1 then
    raise Exception.Create('Multi texture count property unassigned');
  fTexture := tex_CreateZero(Width * Count, Height);
end;

procedure TMultiTexture.Add(const aTexture: zglPTexture);
begin
  if Width = 0 then
  begin
    Allocate(aTexture^.Width, aTexture^.Height);
  end;
end;

end.

