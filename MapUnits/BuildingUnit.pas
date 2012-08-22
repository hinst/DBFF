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
  MapUnit,
  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type

  { TBuildingType }

  TBuildingType = class(IAbstractBuildingType)
  protected
    fTexture: zglPTexture;
    procedure Finalize;
  public
    property Texture: zglPTexture read fTexture;
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
    fLeftTopCell: TCellNumber;
    fBuildingType: TBuildingType;
    fLastTimeVisible: boolean;
    function GetLeftTopCell: PCellNumber; inline;
    procedure Initialize(const aType: TBuildingType);
    procedure SureDraw(const aScroll: TMapScrollManager); virtual; abstract;
  public
    property LeftTopCell: PCellNumber read GetLeftTopCell;
    property BuildingType: TBuildingType read fBuildingType;
    property LastTimeVisible: boolean read fLastTimeVisible;
    procedure Draw(const aScroll: TMapScrollManager); override;
  end;

implementation

{ TBuildingType }

procedure TBuildingType.Finalize;
begin
  GlobalGameManager.Engine.DisposeTexture(fTexture);
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

function TBuilding.GetLeftTopCell: PCellNumber;
begin
  result := @fLeftTopCell;
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
  cells := OccupatedCells;
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
