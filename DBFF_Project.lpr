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
    procedure PrepareConfig;
    function StartupConfig: boolean;
    function StartupLog: boolean;
    {$ENDREGION}
    function Execute: boolean;
    {$REGION Shutdown block}
    procedure ShutdownLog;
    {$ENDREGION}
    function GetTextLogFilePath: string;
  public type
    TOptionName = object // Command line options
      const
        Help = 'help';
        CreateConfigDir = 'createConfigDir';
        ResetEngineConfig = 'resetEngineConfig';
    end;
  public
    property LogManager: TLogManager read fLogManager;
    property Log: ILog read fLog;
    function WriteHelp: boolean;
    function CreateConfigDir: boolean;
    function ResetEngineConfig: boolean;
    procedure Finalize;
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
  if not result then
    WriteLine('***GLOBAL ERROR OCCURED***');
end;

function TApplication.CheckCommandLineOptions: boolean;
var
  validOptions: TStrings;
  errorMessage: string;
begin
  validOptions := TStringList.Create;
  validOptions.Add(TOptionName.Help);
  validOptions.Add(TOptionName.CreateConfigDir);
  validOptions.Add(TOptionName.ResetEngineConfig);
  errorMessage := CheckOptions('',validOptions);
  result := errorMessage = '';
  if not result then
  begin
    WriteLine('FATAL ERROR: Invalid command line options specified.');
    WriteLine('Error is: "' + errorMessage + '"');
    WriteLine('The application would be no longer executed.');
    WriteLine('Consider starting the application with --help parameter.');
  end;
  validOptions.Free;
end;

procedure TApplication.PrepareConfig;
begin
  OnGetApplicationName := @DoOnGetApplicationName;
  OnGetVendorName := @DoOnGetVendorName;
  GlobalConfigPath := GetAppConfigDir(false);
  WriteLine('ConfigPath: "' + GlobalConfigPath + '"');
end;

function TApplication.StartupConfig: boolean;
begin
  result := DirectoryExists(
    ExcludeTrailingPathDelimiter(GlobalConfigPath)
  );
  if not result then
  begin
    WriteLine('FATAL ERROR: Config path does not exists.');
    WriteLine('Consider running application with --' + TOptionName.CreateConfigDir
      + ' parameter specified.');
  end;
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
  result := CheckCommandLineOptions;
  if not result then exit;

  PrepareConfig;

  if HasOption(TOptionName.Help) then
    exit(WriteHelp);

  if HasOption(TOptionName.CreateConfigDir) then
    exit(CreateConfigDir);

  result := StartupConfig;
  if not result then exit;

  result := StartupLog;
  if not result then exit;

  if HasOption(TOptionName.ResetEngineConfig) then
    exit(ResetEngineConfig);
end;

procedure TApplication.ShutdownLog;
begin
  if Assigned(Log) then
  begin
    Log.Write(logTagEnd, 'Log system shutdown...');
    Log.Free;
    fLog := nil;
  end;
  if Assigned(LogManager) then
  begin
    LogManager.Free;
  end;
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

function TApplication.CreateConfigDir: boolean;
begin
  WriteLine('Creation of the config dir requested ("' + GlobalConfigDir + '")');
  if DirectoryExists(GlobalConfigDir) then
  begin
    WriteLine('Config dir already exists.');
    exit(true);
  end;
  result := ForceDirectories(GlobalConfigDir);
  WriteLine('Operation result: ' + BoolToStr(result, true) + '.');
end;

function TApplication.ResetEngineConfig: boolean;
var
  config: TEngineConfig;
begin
  config.SetDefaults;
end;

procedure TApplication.Finalize;
begin
  ShutdownLog;
end;

destructor TApplication.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

var
  Application: TApplication;

begin
  WriteLine('');
  WriteLine('***GLOBAL EXECUTION START***');

  Application := TApplication.Create(nil);
  Application.Title := ApplicationTitle;
  WriteLine('Now starting application: "' + Application.Title + '"...');
  Application.Run;
  Application.Free;

  WriteLine('***GLOBAL EXECUTION END***');
  WriteLine(''); // to separate memory leak info which is being written below
end.



