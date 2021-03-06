unit LevelData;

{$mode objfpc}{$H+}

interface

uses
  {$REGION RTL units}
  Classes,
  SysUtils,
  {$ENDREGION}

  {$REGION FCL units}
  FPimage,
  FPReadPNG,
  {$ENDREGION}

  {$REGION UltimateLibrary units}
  NiceTypes,
  NiceExceptions,
  LogEntityFace,
  LogEntity,
  {$ENDREGION}

  {$REGION Custom units}
  ZenGLFCLGraphics,
  LevelDataFace,
  MapDataCells,
  MapDataFace,
  MapDataContainer,
  TerrainManagerFace,
  TerrainManagerFaceE,
  TerrainManager,
  MapViewer,
  UnitManagerFace,
  UnitManager
  {$ENDREGION}
  ;

type

  { TLevelData }

  TLevelData = class(TComponent, ILevelData)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fMap: TMapData;
    fTerrain: TTerrainManager;
    fMapView: TMapView;
    fUnitMan: TUnitManager;
    procedure Initialize;
    function GetMap: IMapData;
    function GetTerrain: ITerrainManager;
    function GetUnitManager: IUnitManager;
    function GetTerrainTypeByColor(const aColor: LongWord): TTerrainType;
    { Assign terrain types to the cells according to terrain substitution color information
      stored in the Terrain Manager property }
    procedure MapColors(var aMap: TCells.TMatrix; const aImage: TFPCustomImage);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property Map: TMapData read fMap;
    property TerrainManager: TTerrainManager read fTerrain;
    property MapView: TMapView read fMapView;
    property UnitMan: TUnitManager read fUnitMan;
    procedure LoadTerrainMapFromImage(const aImage: TFPCustomImage);
    procedure LoadTerrainMapFromImageFile(const aFileName: string);
    procedure Draw;
    procedure Update(const aTime: double);
    procedure ReceiveInput(const aTime: double);
    destructor Destroy; override;
  end;

implementation

uses
  Common;

{ TLevelData }

constructor TLevelData.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TLevelData.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'LevelData');
  fTerrain := TTerrainManager.Create(self);
  fMap := TMapData.Create(self);
  fMapView := TMapView.Create(self);
  fUnitMan := TUnitManager.Create(self);
  UnitMan.Scroll := MapView.Scroll;
  UnitMan.Map := Map;
end;

function TLevelData.GetMap: IMapData;
begin
  result := fMap;
end;

function TLevelData.GetTerrain: ITerrainManager;
begin
  result := fTerrain;
end;

function TLevelData.GetUnitManager: IUnitManager;
begin
  result := fUnitMan;
end;

function TLevelData.GetTerrainTypeByColor(const aColor: LongWord): TTerrainType;
var
  i: integer;
begin
  result := -1;
  if TerrainManager.TerrainsCount = 0 then
    Log.Write('No terrains');
  for i := 0 to TerrainManager.TerrainsCount - 1 do
    if TerrainManager.Terrains[i]^.Color = aColor then
    begin
      result := TerrainManager.Terrains[i]^.id;
      break;
    end;
end;

procedure TLevelData.MapColors(var aMap: TCells.TMatrix;
  const aImage: TFPCustomImage);
//{$DEFINE DEBUG_THIS_PROCEDURE}
var
  x, y: integer;
  color: TFPColor;
  colorNumber: LongWord;
begin
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('Now mapping colors...');
  {$ENDIF}
  for x := 0 to aImage.Width - 1 do
    for y := 0 to aImage.Height - 1 do
    begin
      color := aImage.Colors[x, y];
      colorNumber := FPColorToLongWordColor(color);
      aMap[x,y].typee := GetTerrainTypeByColor(colorNumber);
      {$IFDEF DEBUG_THIS_PROCEDURE}
      Log.Write('#' + IntToStr(x) + 'x' + IntToStr(y) + ': ' + IntToHex(colorNumber, 6)
        + ' => ' + IntToStr(aMap[x,y].typee));
      {$ENDIF}
    end;
end;
{$UNDEF DEBUG_THIS_PROCEDURE}

procedure TLevelData.Finalize;
begin
  FreeAndNil(fUnitMan);
  FreeAndNil(fMap);
  FreeAndNil(fTerrain);
  FreeLog(fLog);
end;

procedure TLevelData.LoadTerrainMapFromImage(const aImage: TFPCustomImage);
var
  matrix: TCells.TMatrix;
begin
  AssertsAssigned(Map, 'Map', TVariableType.Field)
    .Assigned(Map.Cells, 'Map.Cells');
  Map.Cells.Reallocate(aImage.Width, aImage.Height);
  matrix := Map.Cells.Matrix; // direct access
  MapColors(matrix, aImage);
  MapView.TerrainManager := TerrainManager;
  MapView.Map := Map;
  Log.Write('Updating view...');
  MapView.UpdateView;
  MapView.Scroll.FocusOnMapCenter;
end;

procedure TLevelData.LoadTerrainMapFromImageFile(const aFileName: string);
const
  DEBUG = true;
var
  image: TFPMemoryImage;
begin
  AssertFileExists(aFileName);
  if DEBUG then
    Log.Write('Now loading terrain map from image...'
      + LineEnding + 'File name is: "' + aFileName + '"');
  image := TFPMemoryImage.Create(0, 0);
  if DEBUG then
    Log.Write('Image created. Loading...');
  image.LoadFromFile(aFileName);
  if DEBUG then
    Log.Write('Processing image ' + IntToStr(image.Width) + 'x' + IntToStr(image.Height) + '...');
  LoadTerrainMapFromImage(image);
  if DEBUG then
    Log.Write('Releasing image...');
  image.Free;
  if DEBUG then
    Log.Write('Releasing image - Done.');
end;

procedure TLevelData.Draw;
begin
  MapView.Draw;
  UnitMan.Draw;
  MapView.TerrainView.DrawDebugInfo;
end;

procedure TLevelData.Update(const aTime: double);
begin
  UnitMan.Update(aTime);
end;

procedure TLevelData.ReceiveInput(const aTime: double);
begin
  MapView.ReceiveInput(aTime);
end;

destructor TLevelData.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

