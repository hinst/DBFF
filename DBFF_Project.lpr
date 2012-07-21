program DBFF_Project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads, // Unix - Create Threads
  {$ENDIF}

  {$REGION Standard FPC units}
  Classes,
  SysUtils,
  CustApp,
  {$ENDREGION}

  {$REGION Log units}
  LogManager,
  ConsoleLogWriter,
  SimpleLogTextFormat,
  {$ENDREGION}

  {$REGION Custom units}
  Common
  {$ENDREGION}
  ;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  public
    constructor Create(TheOwner: TComponent); override;
  protected
    fLogManager: TLogManager;
    procedure DoRun; override;
    function Execute: boolean;
    function PrepareConfig: boolean;
    function PrepareLog: boolean;
    function CheckCommandLineOptions: boolean;
  public
    property LogManager: TLogManager read fLogManager;
    function WriteHelp: boolean;
    destructor Destroy; override;
  end;

{ TMainApplication }

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TApplication.DoRun;
var
  result: boolean;
begin
  result := Execute;
  WriteLine('GLOBAL EXECUTION RESULT IS: ' + BoolToStr(result, true));
  Terminate;
end;

function TApplication.Execute: boolean;
begin
  result := PrepareConfig;
  if not result then exit;

  result := CheckCommandLineOptions;
  if not result then exit;

  result := PrepareLog;
  if not result then exit;

  if HasOption('help') then
    exit(WriteHelp);

  result := true;
end;

function TApplication.PrepareConfig: boolean;
begin
  OnGetApplicationName := @DoOnGetApplicationName;
  OnGetVendorName := @DoOnGetVendorName;
  GlobalConfigPath := GetAppConfigDir(false);
  WriteLine('ConfigPath: "' + GlobalConfigPath + '"');
  result := ForceDirectories(GlobalConfigPath);
  if not result then
    WriteLine('Fatal Error: could not create config path.');
end;

function TApplication.PrepareLog: boolean;
var
  consoleLogger: TConsoleLogWriter;
  consoleLogFormat: TSimpleTextLogFormat;
begin
  fLogManager := TLogManager.Create(self);
  consoleLogger := TConsoleLogWriter.Create(LogManager);
  consoleLogFormat := TSimpleTextLogFormat.Create(LogManager);
  consoleLogFormat.FormatStr := '[TAG] OBJECT: TEXT';
  result := true;
end;

function TApplication.CheckCommandLineOptions: boolean;
var
  validOptions: TStrings;
  errorMessage: string;
begin
  validOptions := TStringList.Create;
  validOptions.Add('help');
  errorMessage := CheckOptions('',validOptions);
  result := errorMessage = '';
  if not result then
  begin
    WriteLine('Fatal Error: Invalid command line options specified');
    WriteLine('Error is: "' + errorMessage + '"');
    WriteLine('The application would be no longer executed.');
  end;
  validOptions.Free;
end;

function TApplication.WriteHelp: boolean;
begin
  WriteLine('See the documentation.');
  WriteLine('Contact the developers at eds1491.forumer.com.');
  result := true;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TApplication;

begin
  WriteLine('GLOBAL EXECUTION START');

  Application := TApplication.Create(nil);
  Application.Title := ApplicationTitle;
  WriteLine('Now starting application: "' + Application.Title + '"...');
  Application.Run;
  Application.Free;

  WriteLine('GLOBAL EXECUTION END');
end.

