unit ZenGLFCLGraphics;

{$mode objfpc}{$H+}

interface

uses
  FPimage,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,
  zgl_render_target;

type

  { TMultiTexture }

  TMultiTexture = object
  public
    constructor Init;
  private
    fArea: zglPRenderTarget;
    fCount: integer;
    fWidth: integer;
    fHeight: integer;
    procedure Clean;
    procedure Allocate(const aWidth, aHeight: integer);
  public
    property Area: zglPRenderTarget read fArea;
    property Count: integer read fCount write fCount;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    procedure Add(const aTexture: zglPTexture);
    procedure FinishArea;
    procedure ReleaseArea;
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
  fArea := nil;
  fCount := 0;
  fWidth := 0;
  fHeight := 0;
end;

procedure TMultiTexture.Allocate(const aWidth, aHeight: integer);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  if Count = 0 then
    raise Exception.Create('Multi texture count property unassigned');
  fArea := rtarget_Add(tex_CreateZero(Width * Count, Height), RT_DEFAULT);
end;

procedure TMultiTexture.Add(const aTexture: zglPTexture);
var
  x: integer;
begin
  if Width = 0 then
  begin
    Allocate(aTexture^.Width, aTexture^.Height);
    fCount := 0;
  end;
  rtarget_Set(Area);
  ssprite2d_Draw(aTexture, Count * Width, 0, Width, Height, 0);
  rtarget_Set(nil);
  inc(fCount);
end;

procedure TMultiTexture.FinishArea;
begin
  tex_SetFrameSize(Area^.Surface, Width, Height);
end;

procedure TMultiTexture.ReleaseArea;
begin
  rtarget_Del(fArea);
end;

end.

