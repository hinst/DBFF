unit MaskGeneration;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}

interface

uses
  BGRABitmap, BGRABitmapTypes;

procedure DrawDebris(const aImage: TBGRABitmap);

implementation

procedure DrawDebris(const aImage: TBGRABitmap);
const
{$IFDEF DEBUG}
  color: integer = $000000;
{$ELSE}
  color: integer = $FFFFFF;
{$ENDIF}
  deltaC: real = 0.1;
var
  i, x, y, w, h, d, delta: integer;
  pixel: PBGRAPixel;
  alpha: byte;
begin
  w := aImage.Width;
  h := aImage.Height;
  aImage.Data;
  pixel := aImage.Data;
  for i := aImage.NbPixels - 1 downto 0 do
  begin
    pixel^.alpha := 255 div 2;
    pixel^.red := 0;
    pixel^.red := 0;
    pixel^.red := 0;
    inc(pixel);
  end;
  aImage.AlphaPixel(0, 0, 255 div 2);
  aImage.AlphaPixel(w - 1, 0, 255 div 2);
  d := 1;
  delta := round(h * deltaC);
  for y := 1 to h - 2 do
  begin
    d += random(delta) * 2 - delta;
    if d > w then
      d := w;
    for x := 0 to d - 1 do
    begin
      alpha := round((d - y) / d * 255);
      aImage.AlphaPixel(x, y, alpha);
    end;
  end;
end;

end.

