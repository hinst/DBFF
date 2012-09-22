unit VehicleUnit;

{$DEFINE DEBUG_LOG_VEHICLE_NAVIGATION}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  LogEntityFace,
  LogEntity,
  DeltaApproachUnit,

  Common,
  TerrainManagerFaceE,
  PathFinder,
  MapDataCells,
  MapUnit;

type

  { TVehicleType }

  TVehicleType = class(TMapUnitType)
  public
      { do not forget to override this method.
        by default the speed is 1. everywhere
        Speed is measured in cells per 1 millisecond
      }
  protected
    function GetSpeedAt(const aTerrain: TTerrainType): single; virtual;
  public
    property SpeedAt[const aTerrain: TTerrainType]: single read GetSpeedAt;
  end;

  TVehicleTypeClass = class of TVehicleType;

  { TVehicle }

  TVehicle = class(TMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fPathFind: TPathFind;
    fPath: TCellNumberVector;
    fDeltaX, fDeltaY: single;
    fMyType: TVehicleType;
    procedure LogNavigation(const aText: string);
    procedure UpdateNavigate;
    procedure UpdateMove(const aTime: double);
    procedure UpdateStep;
    function GetCurrentSpeed: single;
    function GetMyType: TVehicleType;
  public
    property PathFind: TPathFind read fPathFind;
    property Path: TCellNumberVector read fPath;
    property DeltaX: single read fDeltaX write fDeltaX;
    property DeltaY: single read fDeltaY write fDeltaY;
    property CurrentSpeed: single read GetCurrentSpeed; // cells per 1 ms
    property MyType: TVehicleType read GetMyType;
    {$REGION IMoveableMapUnit}
    procedure Navigate(const aCell: TCellNumber);
    {$ENDREGION}
    procedure Update(const aTime: double); virtual;
    destructor Destroy; override;
  end;

  TVehicleClass = class of TVehicle;

implementation

function TVehicleType.GetSpeedAt(const aTerrain: TTerrainType): single;
var
  terrain: PTerrain;
begin
  terrain := (GlobalGameManager.Level.Terrain.Reverse as ITerrainManagerE).Terrains[aTerrain];
  if terrain^.Vehicleable then
    result := 1
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
  currentCellApproached: boolean;
begin
  currentCellApproached :=
    DeltaApproach(fDeltaX, aTime * CurrentSpeed)
    and
    DeltaApproach(fDeltaY, aTime * CurrentSpeed)
  ;
  if currentCellApproached then
    UpdateStep;
end;

procedure TVehicle.UpdateStep;
var
  previousCell: TCellNumber;
begin
  if Path = nil then
    exit;
  if Path.Count = 0 then
  begin
    FreeAndNil(fPath);
    exit;
  end;
  previousCell.Assign(LeftTopCell^);
  LeftTopCell^.Assign( Path.Extract(0) );
  LogNavigation('Now stepping to this cell: ' + LeftTopCell^);

  fDeltaX := LeftTopCell^.X - previousCell.X;
  fDeltaY := LeftTopCell^.Y - previousCell.Y;
  if Path.Count = 0 then
    FreeAndNil(fPath);
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
end;

procedure TVehicle.Update(const aTime: double);
begin
  UpdateNavigate;
  UpdateMove(aTime);
end;

destructor TVehicle.Destroy;
begin
  FreeAndNil(fPath);
  FreeAndNil(fPathFind);
  inherited Destroy;
end;

end.

