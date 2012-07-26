unit ZenGL_FCL_Graphics;

{$mode objfpc}{$H+}

interface

uses
  FPimage;

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;

implementation

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;
begin
  result :=0;
  result += aColor.red * 256 * 256;
  result += aColor.green * 256;
  result += aColor.blue;
end;

end.

