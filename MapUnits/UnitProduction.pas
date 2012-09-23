unit UnitProduction;

{$mode objfpc}{$H+}
{$DEFINE HYPER_INLINE}

interface

uses
  Classes,
  SysUtils,
  fgl,

  zgl_primitives_2d,

  NiceTypes,
  NiceExceptions,
  LogEntityFace,
  LogEntity,

  Common,
  UnitManagerExtendedFace,
  MapUnitFace,
  MapDataCells,
  MapDataFace,
  MapScrollManagerFace,
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
    fLog: ILog;
    fFactory: IMapUnit;
    fQue: TUnitProductionList;
    function GetTerrain: ITerrainManagerE;
    function GetUnitMan: IUnitManagerExtended;
      {$IFDEF HYPER_INLINE} INLINE; {$ENDIF}
    procedure Initialize(const aFactory: IMapUnit);
    procedure Finalize;
  public const
    VerticalGap = 8;
    BarHeight = 8;
  public
    property Log: ILog read fLog;
    property Factory: IMapUnit read fFactory;
    property Que: TUnitProductionList read fQue;
    property UnitMan: IUnitManagerExtended read GetUnitMan;
    procedure Update(const aTime: double);
    procedure Draw(const aScroll: IMapScrollManager);
    procedure Complete(const aItem: TUnitProductionItem);
    function FindSuitable1Cell(const aItem: TUnitProductionItem): TCellNumber;
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
  AssertAssigned(terrainMan, 'terrainMan', TVariableType.Local);
  Assert(terrainMan is ITerrainManagerE, 'terrainMan is ITerrainManagerE');
  result := terrainMan as ITerrainManagerE;
  Assert(result <> nil, 'result <> nil');
end;

function TUnitProduction.GetUnitMan: IUnitManagerExtended;
var
  obj: TObject;
begin
  obj := GlobalGameManager.Level.UnitManager.Reverse;
  Assert(obj is IUnitManagerExtended);
  result := obj as IUnitManagerExtended
end;

procedure TUnitProduction.Initialize(const aFactory: IMapUnit);
begin
  fLog := TLog.Create(GlobalLogManager, 'UnitProd');
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
  FreeLog(fLog);
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

procedure TUnitProduction.Draw(const aScroll: IMapScrollManager);
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
var
  cell: TCellNumber;
begin
  cell := FindSuitable1Cell(aItem);
  Log.Write('Complete. Cell found: ' + cell);
  if cell.IsNegative then
  begin
    Log.Write('Impossible to complete: no suitable 1 cell found.');
    aItem.MapUnit.Free;
    exit;
  end;
  UnitMan.AddUnit(aItem.MapUnit, cell.X, cell.Y);
end;

function TUnitProduction.FindSuitable1Cell(const aItem: TUnitProductionItem
  ): TCellNumber;
var
  occupatedCells: TCellNumberArray;
  map: IMapData;
  nearbyCells: TCellNumberVector;
  tman: ITerrainManagerE;
  u: IMapUnit;

  iCell: TCellNumber;
  cell: TCell;
  terrain: PTerrain;
  cellPossibru: boolean;
begin
  result := TCellNumber.Negative;

  occupatedCells := factory.OccupatedCells;
  map := GlobalGameManager.Level.Map;
  nearbyCells := map.ContiguousCells[occupatedCells];
  tman := GetTerrain;
  u := aItem.MapUnit;
  Assert(u <> nil, 'u <> nil');

  for iCell in nearbyCells do
  begin
    cell := map.Cells.Matrix[iCell.X, iCell.Y];
    terrain := tman.Terrains[cell.typee];
    cellPossibru :=
      u.TerrainPossible[terrain] and (not cell.busy)
    ;
    if cellPossibru then
    begin
      result := iCell;
      break;
    end;
  end;
  nearbyCells.Free;
end;

destructor TUnitProduction.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

