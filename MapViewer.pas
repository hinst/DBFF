unit MapViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  NiceExceptions,

  TerrainViewer,
  MapDataFace,
  TerrainManagerFace,
  TerrainManagerFaceE;

type

  { TMapView }

  TMapView = class(TComponent)
  public
    constructor Create(const aOwner: TComponent);
  private
    fTerrainView: TTerrainView;
    procedure Initialize;
    procedure SetTerrainManager(const aManager: ITerrainManagerE);
    procedure SetMap(const aMap: IMapData);
    procedure AssertAssignedTerrainView;
    procedure Finalize;
  public
    property TerrainView: TTerrainView read fTerrainView;
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
  fTerrainView := TTerrainView.Create(self);
end;

procedure TMapView.SetTerrainManager(const aManager: ITerrainManagerE);
begin
  AssertAssignedTerrainView;
  TerrainView.Terrain := aManager;
end;

procedure TMapView.SetMap(const aMap: IMapData);
begin
  AssertAssignedTerrainView;
  TerrainView.Map := aMap;
end;

procedure TMapView.AssertAssignedTerrainView;
begin
  AssertAssigned(TerrainView, 'TerrainView');
end;

procedure TMapView.Finalize;
begin
  FreeAndNil(fTerrainView);
end;

procedure TMapView.Draw;
begin
  TerrainView.DrawTerrainLayer;
end;

procedure TMapView.UpdateView;
begin
  TerrainView.Update;
end;

procedure TMapView.ReceiveInput(const aTime: double);
begin
  AssertAssignedTerrainView;
  TerrainView.ReceiveInput(aTime);
end;

destructor TMapView.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

