unit LevelDataContainer;

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
  LogEntityFace,
  LogEntity,
  NiceExceptions,
  {$ENDREGION}

  {$REGION Custom units}
  ZenGLFCLGraphics,
  LevelDataFace,
  MapDataFace,
  MapDataContainer,
  TerrainManagerFace,
  TerrainManager,
  MapViewer,
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
      { Assign terrain types to the cells according to terrain substitution color information
      stored in the Terrain Manager property }
    function GetTerrainTypeByColor(const aColor: LongWord): TTerrainType;
    procedure MapColors(var aMap: TCells.TMatrix; const aImage: TFPCustomImage);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property Map: TMapData read fMap;
    property Terrain: TTerrainManager read fTerrain;
    property MapView: TMapView read fMapView;
    property UnitMan: TUnitManager read fUnitMan;
    procedure LoadTerrainMapFromImage(const aImage: TFPCustomImage);
    procedure LoadTerrainMapFromImageFile(const aFileName: string);
    procedure Draw;
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
end;

function TLevelData.GetMap: IMapData;
begin
  result := fMap;
end;

function TLevelData.GetTerrain: ITerrainManager;
begin
  result := fTerrain;
end;

function TLevelData.GetTerrainTypeByColor(const aColor: LongWord): TTerrainType;
var
  i: integer;
  terrains: TTerrains;
begin
  terrains := Terrain.Terrains; // direct access
  if Length(Terrains) = 0 then
    raise EUnassigned.Create('Terrains');
  for i := 0 to Length(Terrain.Terrains) - 1 do
    if Terrain.Terrains[i].Color = aColor then
      exit(Terrain.Terrains[i].id);
  exit(-1);
end;

procedure TLevelData.MapColors(var aMap: TCells.TMatrix;
  const aImage: TFPCustomImage);
const
  DEBUG = false;
var
  x, y: integer;
  color: TFPColor;
  colorNumber: LongWord;
begin
  if DEBUG then
    Log.Write('Now mapping colors...');
  for x := 0 to aImage.Width - 1 do
    for y := 0 to aImage.Height - 1 do
    begin
      color := aImage.Colors[x, y];
      colorNumber := FPColorToLongWordColor(color);
      aMap[x,y].typee := GetTerrainTypeByColor(colorNumber);
      if DEBUG then
        Log.Write('#' + IntToStr(x) + 'x' + IntToStr(y) + ': ' + IntToHex(colorNumber, 6)
          + ' => ' + IntToStr(aMap[x,y].typee));
    end;
end;

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
  AssertAssigned(Map, 'Map');
  AssertAssigned(Map.Cells, 'Map.Cells');
  Map.Cells.Reallocate(aImage.Width, aImage.Height);
  matrix := Map.Cells.Matrix; // direct access
  MapColors(matrix, aImage);
  MapView.Terrain := Terrain;
  MapView.Map := Map;
  Log.Write('Updating map...');
  MapView.Update;
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
  MapView.DrawTerrainLayerSimples;
  MapView.DrawGridLines;
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

