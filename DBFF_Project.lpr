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

  {$REGION CUSTOM UNITS}
  Common
  {$ENDREGION}
  ;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function Execute: boolean;
    procedure CheckCommandLineOptions;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function WriteHelp: boolean;
  end;

{ TMainApplication }

procedure TApplication.DoRun;
begin
  Execute;
  Terminate;
end;

function TApplication.Execute: boolean;
begin
  try
    CheckCommandLineOptions;
  except
    on e: Exception do
    begin
      WriteLine('Invalid command line options specified');
      WriteLine('Error is: "' + e.Message + '"');
      exit(false);
    end;
  end;

  if HasOption('help') then
    exit(WriteHelp);

  result := true;
end;

procedure TApplication.CheckCommandLineOptions;
var
  validOptions: TStrings;
  errorMessage: string;
  result: boolean;
begin
  validOptions := TStringList.Create;
  validOptions.Add('help');
  errorMessage := CheckOptions('',validOptions);
  result := errorMessage = '';
  if not result then
  begin
    WriteLine('Invalid command line options specified');
    WriteLine('Error is: "' + errorMessage + '"');
    raise Exception.Create(errorMessage);
  end;
  validOptions.Free;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

function TApplication.WriteHelp: boolean;
begin
  WriteLine('See the documentation.');
  WriteLine('Contact the developers at eds1491.forumer.com.');
  result := true;
end;

var
  Application: TApplication;

begin
  WriteLine('GLOBAL EXECUTION START');

  Application := TApplication.Create(nil);
  Application.Title := ApplicationTitle;
  WriteLine('Now starting application: "' + ApplicationTitle + '"...');
  Application.Run;
  Application.Free;

  WriteLine('GLOBAL EXECUTION END');
end.

