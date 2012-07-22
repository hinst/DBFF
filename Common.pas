unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  LogManager;

const
  ApplicationTitle = 'D:BFF'; // Application title
  ApplicationName = 'DBFF'; // Application name should be a valid file name
  VendorName = 'EpicDevS'; // Vendor name should be a valid file name
  TextLogFileName = 'log.txt';
  EngineConfigFileName = 'EngineConfig.ini';

var
  GlobalConfigPath: string;
  GlobalLogManager: TLogManager;

{
  Вывести указанный текст на стандартный вывод в случае если имеется консоль,
    иначе не делать ничего.
}
procedure WriteLine(const aText: string); inline;

// Возвращает имя поставщика программного обеспечения
function DoOnGetVendorName: string;

// Возвращает имя приложения
function DoOnGetApplicationName: string;

function GlobalConfigDir: string;

implementation

procedure WriteLine(const aText: string);
begin
  if IsConsole then
    WriteLN(aText);
end;

function DoOnGetVendorName: string;
begin
  result := VendorName;
end;

function DoOnGetApplicationName: string;
begin
  result := ApplicationName;
end;

function GlobalConfigDir: string;
begin
  result := ExcludeTrailingPathDelimiter(GlobalConfigPath);
end;

end.

