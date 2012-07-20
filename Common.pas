unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { ApplicationTitle considered to be unchangeable }
  ApplicationTitle = 'D:BFF';

{
  Вывести указанный текст на стандартный вывод в случае если приложение консольное,
    иначе не делать ничего.
}
procedure WriteLine(const aText: string);

implementation

procedure WriteLine(const aText: string);
begin
  if IsConsole then
    WriteLN(aText);
end;

end.

