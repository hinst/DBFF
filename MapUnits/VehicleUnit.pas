unit VehicleUnit;

{$Define DEBUG_LOG_VEHICLE_NAVIGATION}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  LogEntityFace,
  LogEntity,
  DeltaApproachUnit,
  Angle360,

  Common,
  ZenGLFCLGraphics,
  MapScrollManagerFace,
  TerrainManagerFaceE,
  PathFinder,
  MapDataCells,
  MapUnit;

type

  { TVehicleType }

  TVehicleType = class(TMapUnitType)
  public
    constructor Create; override;
  public const
    DefaultTimeBeforeRandomTowerRotation = 5;
  protected
    fTimeBeforeRandomTowerRotation: single;
    { do not forget to override this method.
      by default the speed is 1. everywhere
      Speed is measured in cells per 1 millisecond
    }
    procedure AssignDefaults;
    function GetSpeedAt(const aTerrain: TTerrainType): single; virtual;
  public
    property SpeedAt[const aTerrain: TTerrainType]: single read GetSpeedAt;
  end;

  TVehicleTypeClass = class of TVehicleType;

  TVehicle = class(TMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fPathFind: TPathFind;
    fPath: TCellNumberVector;
    fDeltaX, fDeltaY: single;
    fGraphicalRectUpdateRequired: boolean;
    fTimeBeforeRandomTowerRotation: single;
    procedure LogNavigation(const aText: string);
    procedure UpdateNavigate;
    procedure UpdateMove(const aTime: double); virtual;
    procedure UpdateStep; virtual;
    procedure UpdateStep(const aNextCell: TCellNumber);
    procedure SureUpdateStep(const aNextCell: TCellNumber);
    procedure RecalculatePath;
    function GetCurrentSpeed: single;
    function GetMyType: TVehicleType;
    function GetTerrainPossible(const aTerrain: PTerrain): boolean; override;
    function GetCellPossible(const aCell: TCellNumber): boolean;
    procedure SetMovementDirection(const aDeltaX, aDeltaY: integer); virtual;
  public
    property PathFind: TPathFind read fPathFind;
    property Path: TCellNumberVector read fPath;
    property DeltaX: single read fDeltaX write fDeltaX;
    property DeltaY: single read fDeltaY write fDeltaY;
    property CurrentSpeed: single read GetCurrentSpeed; // cells per 1 ms
    property MyType: TVehicleType read GetMyType;
    property GraphicalRectUpdateRequired: boolean read fGraphicalRectUpdateRequired;
    {$REGION IMoveableMapUnit}
    procedure Navigate(const aCell: TCellNumber);
    {$ENDREGION}
    procedure Update(const aTime: double); virtual;
    procedure UpdateGraphicalRect(const aScroll: IMapScrollManager); override;
    procedure Draw(const aScroll: IMapScrollManager); override;
    destructor Destroy; override;
  end;

  TVehicleClass = class of TVehicle;


implementation

constructor TVehicleType.Create;
begin
  inherited Create;
  AssignDefaults;
end;

procedure TVehicleType.AssignDefaults;
begin
  fTimeBeforeRandomTowerRotation := DefaultTimeBeforeRandomTowerRotation;
end;

function TVehicleType.GetSpeedAt(const aTerrain: TTerrainType): single;
var
  terrain: PTerrain;
begin
  terrain := (GlobalGameManager.Level.Terrain.Reverse as ITerrainManagerE).Terrains[aTerrain];
  if terrain^.Vehicleable then
    result := 1/1000
  else
    result := 0;
end;

constructor TVehicle.Create(const aType: TMapUnitType);
begin
  Assert(aType is TVehicleType);
  inherited Create(aType);
end;

procedure TVehicle.LogNavigation(const aText: string);
begin
  {$IFDEF DEBUG_LOG_VEHICLE_NAVIGATION}
  Log.Write(aText);
  {$ENDIF}
end;

procedure TVehicle.UpdateNavigate;
begin
  if Assigned(PathFind) then
  begin
    PathFind.Iterate;
    if PathFind.Ready then
    begin
      if Assigned(Path) then
        FreeAndNil(fPath);
      if Length(PathFind.Path) > 0 then
        LogNavigation('Path found ' + ToText(PathFind.Path))
      else
        LogNavigation('Impossible to reach destination.');
      fPath := TCellNumberVector.Create(PathFind.Path); // create vector from array
      FreeAndNil(fPathFind);
    end;
  end;
end;

procedure TVehicle.UpdateMove(const aTime: double);
var
  xApproached, yApproached: boolean;
  currentCellApproached: boolean;
begin
  if Path = nil then
    exit;
  xApproached := DeltaApproach(fDeltaX, aTime * CurrentSpeed);
  yApproached := DeltaApproach(fDeltaY, aTime * CurrentSpeed);
  fGraphicalRectUpdateRequired := true;
  currentCellApproached := xApproached and yApproached;
  if currentCellApproached then
    UpdateStep;
end;

procedure TVehicle.UpdateStep;
var
  nextCell: TCellNumber;
begin
  if Path = nil then
    exit;
  if Path.Count = 0 then
  begin
    FreeAndNil(fPath);
    exit;
  end;
  nextCell.Assign( Path.Extract(0) );
  UpdateStep(nextCell);
end;

procedure TVehicle.UpdateStep(const aNextCell: TCellNumber);
begin
  if GetCellPossible(aNextCell) then
    SureUpdateStep(aNextCell)
  else
    RecalculatePath;
end;

procedure TVehicle.SureUpdateStep(const aNextCell: TCellNumber);
var
  previousCell: TCellNumber;
  dx, dy: integer;
begin
  previousCell.Assign( LeftTopCell^ );
  LeftTopCell^.Assign(aNextCell);
  LogNavigation('Now stepping to this cell: ' + LeftTopCell^);
  GetGlobalMap.Cells.Matrix[previousCell.X, previousCell.Y].busy := false;
  GetGlobalMap.Cells.Matrix[LeftTopCell^.X, LeftTopCell^.Y].busy := true;

  dx := LeftTopCell^.X - previousCell.X;
  dy := LeftTopCell^.Y - previousCell.Y;
  SetMovementDirection(dx, dy);
end;

procedure TVehicle.RecalculatePath;
var
  destination: TCellNumber;
begin
  destination.Assign(Path.Last);
  Navigate(destination);
end;

function TVehicle.GetCurrentSpeed: single;
begin
  result :=
    MyType.SpeedAt
    [
      GlobalGameManager.Level.Map.Cells.Matrix
      [
        LeftTopCell^.X,
        LeftTopCell^.Y
      ]
      .typee
    ]
  ;
end;

function TVehicle.GetMyType: TVehicleType;
begin
  result := UnitType as TVehicleType;
end;

function TVehicle.GetTerrainPossible(const aTerrain: PTerrain): boolean;
begin
  result := aTerrain^.Vehicleable;
end;

function TVehicle.GetCellPossible(const aCell: TCellNumber): boolean;
var
  terrainType: TTerrainType;
  terrain: PTerrain;
begin
  if not GetGlobalMap.Cells.CellExists[aCell.X, aCell.Y] then
    exit(false);
  terrainType := GetGlobalMap.Cells.Matrix[aCell.X, aCell.Y].typee;
  terrain := (GlobalGameManager.Level.Terrain.Reverse as ITerrainManagerE).Terrains[terrainType];
  result := GetTerrainPossible(terrain);
  result := result and not GetGlobalMap.Cells.Matrix[aCell.X, aCell.Y].busy;
end;

procedure TVehicle.SetMovementDirection(const aDeltaX, aDeltaY: integer);
begin
  fDeltaX := - aDeltaX;
  fDeltaY := - aDeltaY;
end;

procedure TVehicle.Navigate(const aCell: TCellNumber);
begin
  LogNavigation('Someone wants me to move: ' +
    LeftTopCell^.ToText + '->' + aCell.ToText);
  // clear old path information ~~~
  FreeAndNil(fPath);
  FreeAndNil(fPathFind);
  // setup new Path Finder ~~~
  fPathFind := TPathFind.Create;
  {$IFDEF DEBUG_LOG_VEHICLE_NAVIGATION}
  PathFind.Log := TLog.Create(GlobalLogManager, 'PathFind');
  {$ENDIF}
  PathFind.Map := GlobalGameManager.Level.Map;
  PathFind.Start := LeftTopCell;
  PathFind.Destination := @aCell;
  PathFind.CellPossibleMethod := @GetCellPossible;
end;

procedure TVehicle.Update(const aTime: double);
begin
  UpdateNavigate;
  UpdateMove(aTime);
end;

procedure TVehicle.UpdateGraphicalRect(const aScroll: IMapScrollManager);
begin
  inherited UpdateGraphicalRect(aScroll);
  MoveRect(GraphicalRect, DeltaX * aScroll.TileWidth, DeltaY * aScroll.TileHeight);
end;

procedure TVehicle.Draw(const aScroll: IMapScrollManager);
begin
  inherited Draw(aScroll);
  if GraphicalRectUpdateRequired then
  begin
    fGraphicalRectUpdateRequired := false;
    UpdateGraphicalRect(aScroll);
  end;
end;

destructor TVehicle.Destroy;
begin
  FreeAndNil(fPath);
  FreeAndNil(fPathFind);
  inherited Destroy;
end;

end.

