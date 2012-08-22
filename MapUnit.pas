unit MapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  LogEntityFace,
  LogEntity,

  Common,
  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type


  { TMapUnit }

  TMapUnit = class(IMapUnit)
  public
    constructor Create;
  protected
    fLog: ILog;
    function GetOccupatedCells: TCellNumbers; virtual; abstract;
    procedure Initialize;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property OccupatedCells: TCellNumbers read GetOccupatedCells;
    procedure Draw(const aScroll: TMapScrollManager); virtual; abstract;
    procedure Update(const aTime: double); virtual; abstract;
    destructor Destroy; override;
  end;

implementation

{ TMapUnit }

constructor TMapUnit.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TMapUnit.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, self.ClassName);
end;

procedure TMapUnit.Finalize;
begin
  FreeLog(fLog);
end;

destructor TMapUnit.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

