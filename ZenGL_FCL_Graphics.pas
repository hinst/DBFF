unit ZenGL_FCL_Graphics;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FPimage;

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;

implementation

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;
begin
  result :=0;
  result += (aColor.red shr 8) * 256 * 256;
  result += (aColor.green shr 8) * 256;
  result += (aColor.blue shr 8) * 1;
end;

end.

