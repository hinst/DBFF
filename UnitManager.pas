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

  Common,
  UnitManagerFace,
  BuildingUnit,
  BuildingUnitFaceA,
  MapUnit,
  MapUnitFace,
  MapDataFace,
  MapScrollManager,
  VehicleUnit,
  {$REGION Map units}
  BasicVehicleFactoryUnit,
  BasicGunTurretUnit,
  BasicSingleTurretUnit,
  BasicTankUnit
  {$ENDREGION}
  ;

type
  TIMapUnits = specialize TFPGList<IMapUnit>;

  TBuildingTypes = specialize TFPGList<TBuildingType>;

  TVehicleTypes = specialize TFPGList<TVehicleType>;

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
    fVehicleTypes: TVehicleTypes;
    fScroll: TMapScrollManager;
    fMap: IMapData;
    function GetUnitAtWindowPoint(const aX, aY: integer): IMapUnit;
    procedure Initialize;
    procedure ReleaseUnits;
    procedure ReleaseBuildingTypes;
    procedure ReleaseVehicleTypes;
    procedure MarkBusyCells(const aUnit: IMapUnit);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property MapUnits: TIMapUnits read fMapUnits;
    property BuildingTypes: TBuildingTypes read fBuildingTypes;
    property VehicleTypes: TVehicleTypes read fVehicleTypes;
      // this property should be assigned
    property Scroll: TMapScrollManager read fScroll write fScroll;
      // this property should be assigned
    property Map: IMapData read fMap write fMap;
    property UnitAtWindowPoint[const x, y: integer]: IMapUnit read GetUnitAtWindowPoint;
    procedure LoadBuildingType(const aBuildingTypeClass: TBuildingTypeClass);
    procedure LoadBasicBuildingTypes;
    procedure LoadVehicleType(const aVehicleTypeClass: TVehicleTypeClass);
    procedure LoadBasicVehicleTypes;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure AddUnit(const aUnit: TMapUnit);
    procedure AddBuilding(const aClass: TBuildingClass; const aType: TBuildingTypeClass;
      const aX, aY: integer);
    procedure AddVehicle(const aClass: TVehicleClass; const aType: TVehicleTypeClass;
      const aX, aY: integer);
    function FindBuildingType(const aClass: TBuildingTypeClass): TBuildingType;
    function FindVehicleType(const aClass: TVehicleTypeClass): TVehicleType;
    procedure AddBasicVehicleFactory(const aX, aY: integer);
    procedure AddBasicGunTurret(const aX, aY: integer);
    procedure AddBasicSingleTurret(const aX, aY: integer);
    procedure AddBasicTank(const aX, aY: integer);
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
//{$DEFINE DEBUG_THIS_PROCEDURE}
var
  u: IMapUnit;
  selected: boolean;
  x, y: single;
  r: zglPRect;
begin
  result := nil;
  x := Scroll.ViewLeft + aX;
  y := Scroll.ViewTop + aY;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('GetUnitAtWindowPoint: ' + FloatToStr(x) + ' ' + FloatToStr(y));
  {$ENDIF}
  for u in MapUnits do
  begin
    r := u.GraphicalRect;
    {$IFDEF DEBUG_THIS_PROCEDURE}
    Log.Write(RectToText(r));
    {$ENDIF}
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
  fVehicleTypes := TVehicleTypes.Create;
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

procedure TUnitManager.ReleaseVehicleTypes;
var
  vehicleType: TVehicleType;
begin
  for vehicleType in VehicleTypes do
    FreeAndNil(vehicleType);
  VehicleTypes.Clear;
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
  ReleaseVehicleTypes;
  FreeAndNil(fVehicleTypes);
  ReleaseBuildingTypes;
  FreeAndNil(fBuildingTypes);
  FreeLog(fLog);
end;

procedure TUnitManager.LoadBuildingType(const aBuildingTypeClass: TBuildingTypeClass);
var
  aBuildingType: TBuildingType;
begin
  aBuildingType := aBuildingTypeClass.Create;
  aBuildingType.Load;
  BuildingTypes.Add(aBuildingType);
end;

procedure TUnitManager.LoadBasicBuildingTypes;
begin
  LoadBuildingType(TBasicVehicleFactoryType);
  LoadBuildingType(TBasicGunTurretType);
  LoadBuildingType(TBasicSingleTurretType);
end;

procedure TUnitManager.LoadVehicleType(const aVehicleTypeClass: TVehicleTypeClass);
var
  t: TVehicleType;
begin
  t := aVehicleTypeClass.Create;
  t.Load;
  VehicleTypes.Add(t);
end;

procedure TUnitManager.LoadBasicVehicleTypes;
begin
  LoadVehicleType(TBasicTankType);
end;

procedure TUnitManager.Draw;
var
  u: IMapUnit;
begin
  for u in MapUnits do
    u.Draw(Scroll);
  for u in MapUnits do
    u.DrawTopLayer(Scroll);
end;

procedure TUnitManager.Update(const aTime: double);
var
  u: IMapUnit;
begin
  for u in MapUnits do
    u.Update(aTime);
end;

procedure TUnitManager.AddUnit(const aUnit: TMapUnit);
begin
  Assert(aUnit is IMapUnit);
  MapUnits.Add(aUnit as IMapUnit);
  MarkBusyCells(aUnit as IMapUnit);
end;

procedure TUnitManager.AddBuilding(const aClass: TBuildingClass;
  const aType: TBuildingTypeClass; const aX, aY: integer);
var
  t: TBuildingType;
  u: TBuilding;
begin
  t := FindBuildingType(aType);
  AssertAssigned(t, aType.ClassName);
  u := aClass.Create(t);
  u.LeftTopCell^.SetXY(aX, aY);
  u.UpdateGraphicalRect(Scroll);
  AddUnit(u);
end;

procedure TUnitManager.AddVehicle(const aClass: TVehicleClass;
  const aType: TVehicleTypeClass; const aX, aY: integer);
var
  t: TVehicleType;
  u: TVehicle;
begin
  t := FindVehicleType(aType);
  AssertAssigned(t, aType.ClassName);
  u := aClass.Create(t);
  u.LeftTopCell^.SetXY(aX, aY);
  u.UpdateGraphicalRect(Scroll);
  AddUnit(u);
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

function TUnitManager.FindVehicleType(const aClass: TVehicleTypeClass
  ): TVehicleType;
var
  t: TVehicleType;
begin
  result := nil;
  for t in VehicleTypes do
    if t.ClassType = aClass then
    begin
      result := t;
      break;
    end;
end;

procedure TUnitManager.AddBasicVehicleFactory(const aX, aY: integer);
begin
  AddBuilding(TBasicVehicleFactory, TBasicVehicleFactoryType, aX, aY);
end;

procedure TUnitManager.AddBasicGunTurret(const aX, aY: integer);
begin
  AddBuilding(TBasicGunTurret, TBasicGunTurretType, aX, aY);
end;

procedure TUnitManager.AddBasicSingleTurret(const aX, aY: integer);
begin
  AddBuilding(TBasicSingleTurret, TBasicSingleTurretType, aX, aY);
end;

procedure TUnitManager.AddBasicTank(const aX, aY: integer);
begin
  AddVehicle(TBasicTank, TBasicTankType, aX, aY);
end;

destructor TUnitManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

