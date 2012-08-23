unit UnitManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,

  zgl_math_2d,

  NiceExceptions,
  LogEntityFace,
  LogEntity,

  ZenGLFCLGraphics,
  Common,
  UnitManagerFace,
  BuildingUnit,
  BuildingUnitFaceA,
  MapUnit,
  MapUnitFace,
  MapDataFace,
  MapScrollManager,
  BasicVehicleFactoryUnit;

type
  TIMapUnits = specialize TFPGList<IMapUnit>;

  TBuildingTypes = specialize TFPGList<TBuildingType>;

  { TUnitManager
    Manages and draws units.
  }
  TUnitManager = class(TComponent, IUnitManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fMapUnits: TIMapUnits;
    fBuildingTypes: TBuildingTypes;
    fScroll: TMapScrollManager;
    fMap: IMapData;
    function GetUnitAtWindowPoint(const aX, aY: integer): IMapUnit;
    procedure Initialize;
    procedure ReleaseUnits;
    procedure ReleaseBuildingTypes;
    procedure MarkBusyCells(const aUnit: IMapUnit);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property MapUnits: TIMapUnits read fMapUnits;
    property BuildingTypes: TBuildingTypes read fBuildingTypes;
      // this property should be assigned
    property Scroll: TMapScrollManager read fScroll write fScroll;
      // this property should be assigned
    property Map: IMapData read fMap write fMap;
    property UnitAtWindowPoint[const x, y: integer]: IMapUnit read GetUnitAtWindowPoint;
    procedure LoadBasicBuildingTypes;
    function AddNewBuildingType: IAbstractBuildingType;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure PlaceOnMap(const aUnit: IMapUnit);
    function FindBuildingType(const aClass: TBuildingTypeClass): TBuildingType;
    procedure AddBasicVehicleFactory(const aX, aY: integer);
    destructor Destroy; override;
  end;

implementation

{ TUnitManager }

constructor TUnitManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

function TUnitManager.GetUnitAtWindowPoint(const aX, aY: integer): IMapUnit;
var
  u: IMapUnit;
  selected: boolean;
  x, y: single;
  r: zglPRect;
begin
  result := nil;
  x := Scroll.ViewLeft + aX;
  y := Scroll.ViewTop + aY;
  Log.Write('GetUnitAtWindowPoint: ' + FloatToStr(x) + ' ' + FloatToStr(y));
  for u in MapUnits do
  begin
    r := u.GraphicalRect;
    Log.Write(RectToText(r));
    selected := true
      and (r^.X < x) and (x < r^.X + r^.W)
      and (r^.Y < y) and (y < r^.Y + r^.H);
    if selected then
    begin
      result := u;
      break;
    end;
  end;
end;

procedure TUnitManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'UnitMan');
  fBuildingTypes := TBuildingTypes.Create;
  fMapUnits := TIMapUnits.Create;
end;

procedure TUnitManager.ReleaseUnits;
var
  unitItem: IMapUnit;
begin
  for unitItem in MapUnits do
  begin
    unitItem.Free;
    unitItem := nil;
  end;
  MapUnits.Clear;
end;

procedure TUnitManager.ReleaseBuildingTypes;
var
  buildingType: TBuildingType;
begin
  for buildingType in BuildingTypes do
    FreeAndNil(buildingType);
  BuildingTypes.Clear;
end;

procedure TUnitManager.MarkBusyCells(const aUnit: IMapUnit);
var
  cellNumber: TCellNumber;
  cells: TCellNumbers;
  cell: PCell;
begin
  cells := aUnit.OccupatedCells;
  for cellNumber in cells do
  begin
    cell := Map.Cells.AccessCell(cellNumber.X, cellNumber.Y);
    if cell = nil then continue;
    cell^.busy := true;
  end;
end;

procedure TUnitManager.Finalize;
begin
  ReleaseUnits;
  FreeAndNil(fMapUnits);
  ReleaseBuildingTypes;
  FreeAndNil(fBuildingTypes);
  FreeLog(fLog);
end;

procedure TUnitManager.LoadBasicBuildingTypes;
var
  basicVehicleFactory: TBasicVehicleFactoryType;
begin
  basicVehicleFactory := TBasicVehicleFactoryType.Create;
  basicVehicleFactory.Load(GlobalGameManager.Engine);
  BuildingTypes.Add(basicVehicleFactory);
end;

function TUnitManager.AddNewBuildingType: IAbstractBuildingType;
begin
  result := TBuildingType.Create;
end;

procedure TUnitManager.Draw;
var
  u: IMapUnit;
begin
  for u in MapUnits do
    u.Draw(Scroll);
end;

procedure TUnitManager.Update(const aTime: double);
var
  u: IMapUnit;
begin
  for u in MapUnits do
    u.Update(aTime);
end;

procedure TUnitManager.PlaceOnMap(const aUnit: IMapUnit);
begin
  MapUnits.Add(aUnit);
  MarkBusyCells(aUnit);
end;

function TUnitManager.FindBuildingType(const aClass: TBuildingTypeClass): TBuildingType;
var
  unitType: TBuildingType;
begin
  result := nil;
  for unitType in BuildingTypes do
    if unitType.ClassType = aClass then
    begin
      result := unitType;
      break;
    end;
end;

procedure TUnitManager.AddBasicVehicleFactory(const aX, aY: integer);
var
  t: TBuildingType;
  u: TBasicVehicleFactory;
begin
  t := FindBuildingType(TBasicVehicleFactoryType);
  AssertAssigned(t, TBasicVehicleFactoryType.ClassName);
  u := TBasicVehicleFactory.Create(t);
  u.LeftTopCell^.X := aX;
  u.LeftTopCell^.Y := aY;
  u.UpdateGraphicalRect(Scroll);
  PlaceOnMap(u);
end;

destructor TUnitManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

