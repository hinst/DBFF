unit MapViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  NiceTypes,
  NiceExceptions,

  TerrainViewer,
  MapDataFace,
  MapScrollManager,
  TerrainManagerFace,
  TerrainManagerFaceE;

type

  { TMapView }

  TMapView = class(TComponent)
  public
    constructor Create(const aOwner: TComponent);
  private
    fTerrainView: TTerrainView;
    fScroll: TMapScrollManager;
    procedure Initialize;
    procedure SetTerrainManager(const aManager: ITerrainManagerE);
    procedure SetMap(const aMap: IMapData);
    procedure AssertAssignedTerrainView;
    procedure Finalize;
  public
    property TerrainView: TTerrainView read fTerrainView;
    property Scroll: TMapScrollManager read fScroll;
    property TerrainManager: ITerrainManagerE write SetTerrainManager;
    property Map: IMapData write SetMap;
    procedure Draw;
    procedure UpdateView;
    procedure ReceiveInput(const aTime: double);
    destructor Destroy; override;
  end;

implementation

{ TMapView }

constructor TMapView.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TMapView.Initialize;
begin
  fScroll := TMapScrollManager.Create;
  fTerrainView := TTerrainView.Create(self);
  TerrainView.Scroll := fScroll;
end;

procedure TMapView.SetTerrainManager(const aManager: ITerrainManagerE);
begin
  AssertAssignedTerrainView;
  TerrainView.Terrain := aManager;
end;

procedure TMapView.SetMap(const aMap: IMapData);
begin
  AssertAssignedTerrainView;
  Scroll.Map := aMap;
  TerrainView.Map := aMap;
end;

procedure TMapView.AssertAssignedTerrainView;
begin
  AssertAssigned(TerrainView, 'TerrainView', TVariableType.Propertie);
end;

procedure TMapView.Finalize;
begin
  FreeAndNil(fScroll);
  FreeAndNil(fTerrainView);
end;

procedure TMapView.Draw;
begin
  TerrainView.DrawTerrainLayer;
end;

procedure TMapView.UpdateView;
begin
  Scroll.Update;
  TerrainView.Update;
end;

procedure TMapView.ReceiveInput(const aTime: double);
begin
  AssertAssignedTerrainView;
  Scroll.ReceiveInput(aTime);
end;

destructor TMapView.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

