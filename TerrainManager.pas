unit TerrainManager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$DEFINE LOG_WARN_ON_NO_TEXTURE}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  zgl_textures,

  NiceExceptions,
  StringFeatures,
  LogEntity,
  LogEntityFace,
  EngineManagerFace,

  ZenGLFCLGraphics,

  Common,
  TerrainManagerFace,
  TerrainManagerFaceE,
  MapDataCells;

type

  { TTerrainManager }

  TTerrainManager = class(TComponent, ITerrainManager, ITerrainManagerE)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fTerrains: TTerrains;
    fMasks: TMultiTexture;
    fEngine: IEngineManager;
    procedure Initialize;
    procedure LoadTerrains(const aFile: TIniFile);
    procedure LoadTerrainsFromList(const aFile: TIniFile; const aList: TStrings);
    procedure LoadTerrain(var aTerrain: TTerrain; const aFile: TIniFile);
    procedure LoadTerrainTexture(var aTerrain: TTerrain; const aFile: TIniFile);
    function GetTerrain(const aIndex: integer): PTerrain;
    function GetTerrainsCount: integer;
    procedure ReleaseTerrains;
    procedure LoadMasks(const aFile: TIniFile);
    function GetMasks: TMultiTexture;
    procedure Finalize;
  public const
    ColorIdent = 'replacingColor';
    VehicleableIdent = 'Vehicleable';
    TextureFilePathIdent = 'textureFile';
    CommonSection = 'common';
  public
    property Log: ILog read fLog;
    property Terrains[const aIndex: integer]: PTerrain read GetTerrain;
    property TerrainsCount: integer read GetTerrainsCount;
    property Masks: TMultiTexture read fMasks;
    property Engine: IEngineManager read fEngine;
    procedure LoadTerrains(const aFileName: string);
    procedure LoadMasks(const aFileName: string);
    function GetTerrainsInfoAsText: string;
    function GetTypeColor(const aType: TTerrainType): LongWord;
    function Reverse: TObject;
    destructor Destroy; override;
  end;

implementation

{ TTerrainManager }

constructor TTerrainManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TTerrainManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'TerrainManager');
  fEngine := GlobalGameManager.Engine;
  fMasks := TMultiTexture.Create;
  Masks.Engine := fEngine;
end;

procedure TTerrainManager.LoadTerrainsFromList(const aFile: TIniFile;
  const aList: TStrings);
var
  i: integer;
begin
  SetLength(fTerrains, aList.Count);
  for i := 0 to aList.Count - 1 do
  begin
    Terrains[i]^.Init;
    Terrains[i]^.Name := aList[i];
    Terrains[i]^.id := i;
    LoadTerrain(fTerrains[i], aFile);
  end;
end;

procedure TTerrainManager.LoadMasks(const aFile: TIniFile);
const
  DEBUG = true;

  function LoadMask(const aSection: string): zglPTexture;
  var
    MaskFilePath: string;
  begin
    MaskFilePath := aFile.ReadString(aSection, TextureFilePathIdent, '');
    if DEBUG then
      Log.Write('Now loading mask "' + aSection + '"'
        + LineEnding + '"' + MaskFilePath + '"');
    MaskFilePath := GlobalApplicationPath + MaskFilePath;
    if not FileExists(MaskFilePath) then
      raise EFileNotFound.Create(MaskFilePath);
    result := Engine.LoadTexture(MaskFilePath);
  end;

var
  sections: TStrings;
  s: string;
  texture: zglPTexture;
begin
  sections := TStringList.Create;
  aFile.ReadSections(sections);
  Masks.Count := sections.Count;
  if DEBUG then
    Log.Write('Now loading masks: ' + IntToStr(Masks.Count) + ' items');
  for s in sections do
  begin
    texture := LoadMask(s);
    Masks.Add(texture);
    if DEBUG then
      Log.Write('Now disposing texture...');
    Engine.DisposeTexture(texture);
  end;
  Masks.FinishArea;
  sections.Free;
end;

function TTerrainManager.GetMasks: TMultiTexture;
begin
  result := fMasks;
end;

procedure TTerrainManager.ReleaseTerrains;
var
  i: integer;
begin
  for i := 0 to TerrainsCount - 1 do
    fTerrains[i].Done;
  SetLength(fTerrains, 0);
end;

procedure TTerrainManager.Finalize;
begin
  ReleaseTerrains;
  FreeAndNil(fMasks);
  FreeLog(fLog);
end;

procedure TTerrainManager.LoadTerrains(const aFileName: string);
var
  ini: TIniFile;
begin
  AssertFileExists(aFileName);
  ini := TIniFile.Create(aFileName);
  LoadTerrains(ini);
  ini.Free;
end;

procedure TTerrainManager.LoadMasks(const aFileName: string);
var
  ini: TIniFile;
begin
  AssertFileExists(aFileName);
  ini := TIniFile.Create(aFileName);
  LoadMasks(ini);
  ini.Free;
end;

procedure TTerrainManager.LoadTerrains(const aFile: TIniFile);
  procedure LoadTheTerrains;
  var
    terrainList: TStrings;
    commonSectionIndex: integer;
  begin
    terrainList := TStringList.Create;
    aFile.ReadSections(terrainList);
    commonSectionIndex := terrainList.IndexOf(CommonSection);
    if commonSectionIndex <> -1 then
      terrainList.Delete(commonSectionIndex); // "common" section should be excluded
    LoadTerrainsFromList(aFile, terrainList);
    terrainList.Free;
  end;

begin
  LoadTheTerrains;
end;

procedure TTerrainManager.LoadTerrain(var aTerrain: TTerrain;
  const aFile: TIniFile);
begin
  aTerrain.Color := StrHexToLongWord(aFile.ReadString(aTerrain.Name, ColorIdent, '000000'));
  aTerrain.Vehicleable := aFile.ReadBool(aTerrain.Name, VehicleableIdent, false);
  LoadTerrainTexture(aTerrain, aFile);
end;

procedure TTerrainManager.LoadTerrainTexture(var aTerrain: TTerrain;
  const aFile: TIniFile);
{$DEFINE DEBUG_THIS}

  procedure DebugWrite(const aText: string);
  begin
    {$IFDEF DEBUG_THIS}
    Log.Write(aText);
    {$ENDIF}
  end;

var
  textureFilePath: string;
begin
  textureFilePath := aFile.ReadString(aTerrain.Name, TextureFilePathIdent, '');
  if textureFilePath = '' then
  begin
    {$IFDEF LOG_WARN_ON_NO_TEXTURE}
    Log.Write('No texture for "' + aTerrain.Name + '" specified');
    {$ENDIF}
    aTerrain.Texture := nil;
  end
  else
  begin
    DebugWrite('Now loading texture for "' + aTerrain.Name + '" from:'
      + LineEnding + '"' + textureFilePath + '"');
    textureFilePath := GlobalApplicationPath + textureFilePath;
    if FileExists(textureFilePath) then
      aTerrain.Texture := Engine.LoadTexture(textureFilePath)
    else
      Log.Write(logTagError, 'File does not exists'
        + LineEnding + '"' + textureFilePath + '"');
  end;
end;

function TTerrainManager.GetTerrain(const aIndex: integer): PTerrain;
begin
  result := nil;
  AssertIndexInBounds(0, aIndex, TerrainsCount - 1, 'Terrain type out of bounds');
  result := @ ( fTerrains[aIndex] );
end;

function TTerrainManager.GetTerrainsCount: integer;
begin
  result := Length(fTerrains);
end;

function TTerrainManager.GetTerrainsInfoAsText: string;
var
  i: integer;
begin
  result := 'Terrain types: ' + IntToStr(Length(fTerrains)) + ' items total';
  for i := 0 to TerrainsCount - 1 do
    result += LineEnding + Terrains[i]^.ToText;
end;

function TTerrainManager.GetTypeColor(const aType: TTerrainType): LongWord;
begin
  AssertIndexInBounds(0, aType, TerrainsCount - 1, 'No such terrain type');
  result := Terrains[aType]^.Color;
end;

function TTerrainManager.Reverse: TObject;
begin
  result := self;
end;

destructor TTerrainManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

