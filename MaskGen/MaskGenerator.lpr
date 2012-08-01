program MaskGenerator;

{$mode objfpc}{$H+}

uses
  Interfaces,
  MaskGeneration,
  BGRABitmap;

var
  b: TBGRABitmap;

begin
  b := TBGRABitmap.Create(64, 64);
  randomize;
  DrawDebris(b);
  b.SaveToFile('image.png');
  b.Free;
end.

