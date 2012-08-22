unit UnitManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,

  NiceExceptions,

  Common,
  UnitManagerFace,
  BuildingUnit,
  BuildingUnitFaceA,
  MapUnit,
  MapDataFace,
  MapScrollManager,
  BasicVehicleFactoryUnit;

type
  TUnitList = specialize TFPGList<TMapUnit>;

  TBuildingTypes = specialize TFPGList<TBuildingType>;

  { TUnitManager
    Manages and draws units.
  }
  TUnitManager = class(TComponent, IUnitManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fMapUnits: TUnitList;
    fBuildingTypes: TBuildingTypes;
    fScroll: TMapScrollManager;
    fMap: IMapData;
    procedure Initialize;
    procedure ReleaseUnits;
    procedure ReleaseBuildingTypes;
    procedure Finalize;
  public
    property MapUnits: TUnitList read fMapUnits;
    property BuildingTypes: TBuildingTypes read fBuildingTypes;
      // this property should be assigned
    property Scroll: TMapScrollManager read fScroll write fScroll;
      // this property should be assigned
    property Map: IMapData read fMap write fMap;
    procedure LoadBasicBuildingTypes;
    function AddNewBuildingType: IAbstractBuildingType;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure AddUnit(const aUnit: TMapUnit);
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

procedure TUnitManager.Initialize;
begin
  fBuildingTypes := TBuildingTypes.Create;
  fMapUnits := TUnitList.Create;
end;

procedure TUnitManager.ReleaseUnits;
var
  unitItem: TMapUnit;
begin
  for unitItem in MapUnits do
    FreeAndnil(unitItem);
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

procedure TUnitManager.Finalize;
begin
  ReleaseUnits;
  FreeAndNil(fMapUnits);
  ReleaseBuildingTypes;
  FreeAndNil(fBuildingTypes);
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
  u: TMapUnit;
begin
  for u in MapUnits do
    u.Draw(Scroll);
end;

procedure TUnitManager.Update(const aTime: double);
var
  u: TMapUnit;
begin
  for u in MapUnits do
    u.Update(aTime);
end;

procedure TUnitManager.AddUnit(const aUnit: TMapUnit);
begin
  MapUnits.Add(aUnit);
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
  AddUnit(u);
end;

destructor TUnitManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

