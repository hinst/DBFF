program MaskGenerator;

{$mode objfpc}{$H+}

uses
  interfaces,
  MaskGeneration,
  BGRABitmap;

var
  b: TBGRABitmap;

begin
  b := TBGRABitmap.Create(64, 64);
  randomize;
  DrawDebris(b);
  b.SaveToFile('mask.png');
  b.Free;
end.

