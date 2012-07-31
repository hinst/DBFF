unit MaskGeneration;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}

interface

uses
  BGRABitmap;

procedure DrawDebris(const aImage: TBGRABitmap);

implementation

procedure DrawDebris(const aImage: TBGRABitmap);
const
{$IFDEF DEBUG}
  color = $000000;
{$ELSE}
  color = $FFFFFF;
{$ENDIF}
var
  x, y, w, h: integer;
begin
  w := aImage.Width;
  h := aImage.Height;
  for x := 0 to w - 1 do
    for y := 0 to h - 1 do
    begin
      aImage.AlphaPixel(x, y, 0);
      aImage.Pixels[x, y] := color;
    end;
  aImage.AlphaPixel(0, h - 1, 255 div 2);
  aImage.AlphaPixel(w - 1, h - 1, 255 div 2);
end;

end.

