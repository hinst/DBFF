unit UnitManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,

  Common,
  UnitManagerFace,
  BuildingUnit,
  BuildingUnitFaceA,
  MapUnit,
  BasicVehicleFactoryUnit;

type
  TUnitList = specialize TFPGList<TMapUnit>;

  TBuildingTypes = specialize TFPGList<TBuildingType>;

  { TUnitManager }

  TUnitManager = class(TComponent, IUnitManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fMapUnits: TUnitList;
    fBuildingTypes: TBuildingTypes;
    procedure Initialize;
    procedure ReleaseBuildingTypes;
    procedure Finalize;
  public
    property MapUnits: TUnitList read fMapUnits;
    property BuildingTypes: TBuildingTypes read fBuildingTypes;
    procedure LoadBasicBuildingTypes;
    function AddNewBuildingType: IAbstractBuildingType;
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

procedure TUnitManager.ReleaseBuildingTypes;
var
  buildingType: TBuildingType;
begin
  for buildingType in BuildingTypes do
    FreeAndNil(buildingType);
end;

procedure TUnitManager.Finalize;
begin
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

destructor TUnitManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

