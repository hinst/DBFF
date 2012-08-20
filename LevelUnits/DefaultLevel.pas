unit DefaultLevel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,

  NiceExceptions,
  LogEntityFace,
  NoLogEntity,

  UnitManagerFace;

type

  { TDefaultLevel }

  TDefaultLevel = class
  public
    constructor Create;
  private
    fLog: ILog;
    procedure SetLog(const aLog: ILog);
    procedure Initialize;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    destructor Destroy; override;
  end;

implementation

constructor TDefaultLevel.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TDefaultLevel.SetLog(const aLog: ILog);
begin
  FreeLog(fLog);
  fLog := aLog;
end;

procedure TDefaultLevel.Initialize;
begin
  fLog := TNoLog.Create;
end;

procedure TDefaultLevel.Finalize;
begin
  FreeLog(fLog);
end;

destructor TDefaultLevel.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

