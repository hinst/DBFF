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
  LogEntity,
  LogItem,
  LogWriter,
  LogObjectEnhancer,
  ConsoleLogWriter,
  TextFileLogWriter,
  SimpleLogTextFormat,
  {$ENDREGION}

  {$REGION Custom units}
  Common, EngineManager
  {$ENDREGION}
  ;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  public
    constructor Create(TheOwner: TComponent); override;
  protected
    fLogManager: TLogManager;
    fLog: ILog;
    procedure DoRun; override;
    {$REGION Startup block}
    function CheckCommandLineOptions: boolean;
    function StartupConfig: boolean;
    function StartupLog: boolean;
    {$ENDREGION}
    function Execute: boolean;
    {$REGION Shutdown block}
    function ShutdownLog: boolean;
    {$ENDREGION}
    function GetTextLogFilePath: string;
  public
    property LogManager: TLogManager read fLogManager;
    property Log: ILog read fLog;
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
  result: boolean = false;
begin
  Terminate; // prevent loop of this method
  try
    result := Execute;
  except
    on E: Exception do
    begin
      WriteLine('FATAL ERROR: Global exception occured.');
      WriteLine('Application will be no longer executed.');
      DumpExceptionBackTrace(output);
    end;
  end;
  WriteLine('GLOBAL EXECUTION RESULT IS: ' + BoolToStr(result, true));
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
    WriteLine('FATAL ERROR: Invalid command line options specified.');
    WriteLine('Error is: "' + errorMessage + '"');
    WriteLine('The application would be no longer executed.');
  end;
  validOptions.Free;
end;

function TApplication.StartupConfig: boolean;
begin
  OnGetApplicationName := @DoOnGetApplicationName;
  OnGetVendorName := @DoOnGetVendorName;
  GlobalConfigPath := GetAppConfigDir(false);
  WriteLine('ConfigPath: "' + GlobalConfigPath + '"');
  result := ForceDirectories(GlobalConfigPath);
  if not result then
    WriteLine('FATAL ERROR: Could not create config path.');
end;

function TApplication.StartupLog: boolean;
  function AddConsoleLogger(const aManager: TLogManager): TConsoleLogWriter;
  var
    LogFormat: TSimpleTextLogFormat;
  begin
    result := TConsoleLogWriter.Create(aManager);
    LogFormat := TSimpleTextLogFormat.Create(aManager);
    LogFormat.FormatStr := '[TAG] OBJECT: TEXT';
    result.Format := LogFormat;
    aManager.ImmediateWriters.Add(result as ILogWriter);
  end;

  function AddFileLogger(const aManager: TLogManager): TTextFileLogWriter;
  var
    LogFormat: TSimpleTextLogFormat;
  begin
    result := TTextFileLogWriter.Create(aManager, GetTextLogFilePath);
    LogFormat := TSimpleTextLogFormat.Create(aManager);
    LogFormat.FormatStr := '#NUMBER# TIME [TAG]OBJECT: TEXT';
    result.Format := LogFormat;
    aManager.DeferredWriters.Add(result as ILogWriter);
  end;

begin
  fLogManager := TLogManager.Create(self);
  LogManager.StandardLogTagToString := TStandardLogTagToString.Create;
  AddConsoleLogger(LogManager);
  AddFileLogger(LogManager);
  GlobalLogManager := LogManager;
  fLog := TLog.Create(LogManager, 'App');
  Log.Write(logTagStartup, 'Log system started');
  result := true;
end;

function TApplication.Execute: boolean;
begin
  result := StartupConfig;

  result := CheckCommandLineOptions;
  if not result then exit;

  result := StartupLog;
  if not result then exit;

  if HasOption('help') then
    exit(WriteHelp);

  result := ShutdownLog;

  result := true;
end;

function TApplication.ShutdownLog: boolean;
begin
  Log.Write(logTagEnd, 'Log system shutdown...');
  Log.Free;
  fLog := nil;
  LogManager.Free;
  result := true;
end;

function TApplication.GetTextLogFilePath: string;
begin
  result := GlobalConfigPath + TextLogFileName;
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
  WriteLine('***GLOBAL EXECUTION START***');

  Application := TApplication.Create(nil);
  Application.Title := ApplicationTitle;
  WriteLine('Now starting application: "' + Application.Title + '"...');
  Application.Run;
  Application.Free;

  WriteLine('***GLOBAL EXECUTION END***');
end.

