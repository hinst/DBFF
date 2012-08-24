unit BuildingUnit;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG_LOG_VISIBILITY}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  NiceExceptions,

  Common,
  BuildingUnitFaceA,
  EngineManagerFace,
  MapUnit,
  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type

  { TBuildingType }

  TBuildingType = class(IAbstractBuildingType)
  protected
    fTexture: zglPTexture;
    function GetStandardUnitsPath: string;
    function GetEngine: IEngineManager;
    procedure Finalize;
  public
    property Texture: zglPTexture read fTexture;
    property StandardUnitsPath: string read GetStandardUnitsPath;
    property Engine: IEngineManager read GetEngine;
    destructor Destroy; override;
  end;

  TBuildingTypeClass = class of TBuildingType;

  TVehicleFactory = class(TBuildingType)
  end;

  { TBuilding }

  TBuilding = class(TMapUnit)
  public
    constructor Create(const aType: TBuildingType); virtual;
  protected
    fBuildingType: TBuildingType;
    fLastTimeVisible: boolean;
    procedure Initialize(const aType: TBuildingType);
    procedure SureDraw(const aScroll: TMapScrollManager); virtual; abstract;
  public
    property BuildingType: TBuildingType read fBuildingType;
    property LastTimeVisible: boolean read fLastTimeVisible;
    procedure Draw(const aScroll: TMapScrollManager);
  end;

  TBuildingClass = class of TBuilding;

implementation

{ TBuildingType }

function TBuildingType.GetStandardUnitsPath: string;
begin
  result := GlobalApplicationPath + StandardUnitsRelativePath;
end;

function TBuildingType.GetEngine: IEngineManager;
begin
  result := GlobalGameManager.Engine;
end;

procedure TBuildingType.Finalize;
begin
  Engine.DisposeTexture(fTexture);
end;

destructor TBuildingType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{ TBuilding }

constructor TBuilding.Create(const aType: TBuildingType);
begin
  inherited Create;
  Initialize(aType);
end;

procedure TBuilding.Initialize(const aType: TBuildingType);
begin
  fBuildingType := aType;
end;

procedure TBuilding.Draw(const aScroll: TMapScrollManager);
var
  cell: TCellNumber;
  cells: TCellNumbers;
  visible: boolean;
begin
  AssertAssigned(BuildingType, 'BuildingType');
  if self.ClassType = TBuilding then
    raise Exception.Create('Can not draw abstract building.');
  cells := (self as IMapUnit).OccupatedCells;
  visible := false;
  for cell in cells do
    if aScroll.CellVisible[cell.X, cell.Y] then
    begin
      visible := true;
      break;
    end;
  if visible then
    SureDraw(aScroll);
  {$IFDEF DEBUG_LOG_VISIBILITY}
  if visible and not LastTimeVisible then
    Log.Write('Unit is now visible');
  if not visible and LastTimeVisible then
    Log.Write('Unit is now not visible');
  {$ENDIF}
  fLastTimeVisible := visible;
end;

end.

