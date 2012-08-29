unit UnitProduction;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,

  zgl_primitives_2d,

  NiceExceptions,

  Common,
  MapUnitFace,
  MapDataFace,
  MapScrollManager,
  TerrainManagerFaceE;

type

  TUnitProductionItem = class
  private
    fProgress: single;
    fTimeCost: single;
    fMapUnit: IMapUnit;
  public
    property Progress: single read fProgress write fProgress;
      // ms
    property TimeCost: single read fTimeCost write fTimeCost;
    property MapUnit: IMapUnit read fMapUnit write fMapUnit;
  end;

  TUnitProductionList = specialize TFPGList<TUnitProductionItem>;

  { TUnitProduction }

  TUnitProduction = class
  public
    constructor Create(const aFactory: IMapUnit); reintroduce;
  protected
    fFactory: IMapUnit;
    fQue: TUnitProductionList;
    function GetTerrain: ITerrainManagerE;
    procedure Initialize(const aFactory: IMapUnit);
    procedure Finalize;
  public const
    VerticalGap = 8;
    BarHeight = 8;
  public
    property Factory: IMapUnit read fFactory;
    property Que: TUnitProductionList read fQue;
    procedure Update(const aTime: double);
    procedure Draw(const aScroll: TMapScrollManager);
    procedure Complete(const aItem: TUnitProductionItem);
    function FindFreeCell: TCellNumber;
    destructor Destroy; override;
  end;

implementation

{ TUnitProduction }

constructor TUnitProduction.Create(const aFactory: IMapUnit);
begin
  inherited Create;
  Initialize(aFactory);
end;

function TUnitProduction.GetTerrain: ITerrainManagerE;
var
  terrainMan: TObject;
begin
  terrainMan := GlobalGameManager.Level.Terrain.Reverse;
  AssertAssigned(terrainMan, 'terrainMan');
  Assert(terrainMan is ITerrainManagerE, 'terrainMan is ITerrainManagerE');
  result := terrainMan as ITerrainManagerE;
end;

procedure TUnitProduction.Initialize(const aFactory: IMapUnit);
begin
  fFactory := aFactory;
  fQue := TUnitProductionList.Create;
end;

procedure TUnitProduction.Finalize;
var
  item: TUnitProductionItem;
begin
  for item in Que do
    item.Free;
  fQue.Clear;
  FreeAndNil(fQue);
end;

procedure TUnitProduction.Update(const aTime: double);
var
  item: TUnitProductionItem;
begin
  if Que.Count = 0 then exit;
  item := Que.First;
  item.fProgress += aTime / item.TimeCost;
  if item.Progress >= 1 then
  begin
    Complete(item);
    Que.Remove(item);
    item.Free;
  end;
end;

procedure TUnitProduction.Draw(const aScroll: TMapScrollManager);
var
  x, y, w, ww, h: single;
  item: TUnitProductionItem;
begin
  if Que.Count = 0 then exit;
  x := aScroll.ScreenX(Factory.LeftTopCell^);
  y := aScroll.ScreenY(Factory.LeftTopCell^);
  y += aScroll.TileHeight * Factory.UnitHeight;
  y += VerticalGap;
  w := aScroll.TileWidth * Factory.UnitWidth;
  h := BarHeight;
  item := Que.First;
  ww := w * item.Progress;
  pr2d_Rect(x, y, ww, h, $00FF00, 255 div 2, PR2D_FILL);
  pr2d_Rect(x, y, w, h, $000000, 255 div 3 * 2);
end;

procedure TUnitProduction.Complete(const aItem: TUnitProductionItem);
begin

end;

function TUnitProduction.FindFreeCell: TCellNumber;
var
  cells: TCellNumbers;
  iCell: TCellNumber;
  cell: TCell;
  map: IMapData;
  tman: ITerrainManagerE;
  possible: boolean;
begin
  cells := factory.OccupatedCells;
  map := GlobalGameManager.Level.Map;
  tman := GetTerrain;
  possible := false;
  for iCell in cells do
  begin
    cell := map.Cells.Matrix[iCell.X, iCell.Y];
    possible := Que.First.MapUnit.TerrainPossible[tman.Terrains[cell.typee]];
  end;
end;

destructor TUnitProduction.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

