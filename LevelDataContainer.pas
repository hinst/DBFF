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
  ZenGL_FCL_Graphics,
  LevelDataFace,
  MapDataFace,
  MapDataContainer,
  TerrainManagerFace,
  TerrainManager
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
    procedure Initialize;
    function GetMap: IMapData;
    function GetTerrain: ITerrainManager;
      { Assign terrain types to the cells according to terrain substitution color information
      stored in the Terrain Manager property }
    function GetTerrainTypeByColor(const aColor: LongWord): TTerrainType;
    procedure MapColors(const aMap: TCells.TMatrix; const aImage: TFPCustomImage);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property Map: TMapData read fMap;
    property Terrain: TTerrainManager read fTerrain;
    procedure LoadTerrainMapFromImage(const aImage: TFPCustomImage);
    procedure LoadTerrainMapFromImage(const aFileName: string);
    destructor Destroy; override;
  end;

implementation

{ TLevelData }

constructor TLevelData.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TLevelData.Initialize;
begin
  fTerrain := TTerrainManager.Create(self);
  fMap := TMapData.Create(self);
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
  AssertAssigned(Assigned(Terrains), 'Terrains');
  for i := 0 to Length(Terrain.Terrains) - 1 do
    if Terrain.Terrains[i].Color = aColor then
      exit(Terrain.Terrains[i].id);
  exit(-1);
end;

procedure TLevelData.MapColors(const aMap: TCells.TMatrix;
  const aImage: TFPCustomImage);
var
  x, y: integer;
  color: TFPColor;
  colorNumber: LongWord;
begin
  for x := 0 to aImage.Width - 1 do
    for y := 0 to aImage.Height - 1 do
    begin
      color := aImage.Colors[x, y];
      colorNumber := FPColorToLongWordColor(color);
      aMap[x,y].typ := GetTerrainTypeByColor(colorNumber);
    end;
end;

procedure TLevelData.Finalize;
begin
  if Assigned(Log) then
  begin
    Log.Free;
    fLog := nil;
  end;
  if Assigned(Map) then
    FreeAndNil(fMap);
  if Assigned(Terrain) then
    FreeAndNil(fTerrain);
end;

procedure TLevelData.LoadTerrainMapFromImage(const aImage: TFPCustomImage);
var
  matrix: TCells.TMatrix;
begin
  Map.Cells.Reallocate(aImage.Width, aImage.Height);
  matrix := Map.Cells.Matrix; // direct access
  MapColors(matrix, aImage);
end;

procedure TLevelData.LoadTerrainMapFromImage(const aFileName: string);
var
  image: TFPCustomImage;
begin
  AssertFileExists(aFileName);
  image := TFPCustomImage.Create(0, 0);
  image.LoadFromFile(aFileName);
  LoadTerrainMapFromImage(image);
  image.Free;
end;

destructor TLevelData.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

