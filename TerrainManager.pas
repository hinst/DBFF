unit TerrainManager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  zgl_textures,
  zgl_textures_png,

  NiceExceptions,
  StringFeatures,
  LogEntity,
  LogEntityFace,

  TerrainManagerFace,
  MapDataFace;

type

  { TTerrain }

  TTerrain = object
  public
    constructor Init;
    procedure Clean;
  public
    id: TTerrainType;
    Name: string;
    Color: LongWord;
    Texture: zglPTexture;
    function ToText: string;
    destructor Done;
  end;

  TTerrains = array of TTerrain;

  { TTerrainManager }

  TTerrainManager = class(TComponent, ITerrainManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fTerrains: TTerrains;
    fMasks: zglPTexture;
    procedure Initialize;
    procedure LoadTerrains(const aFile: TIniFile);
    procedure LoadTerrainsFromList(const aFile: TIniFile; const aList: TStrings);
    procedure LoadTerrain(var aTerrain: TTerrain; const aFile: TIniFile);
    procedure LoadTerrainTexture(var aTerrain: TTerrain; const aFile: TIniFile);
    procedure ReleaseTerrains;
    procedure LoadMasks(const aFile: TIniFile);
    procedure Finalize;
  public const
    ColorIdent = 'replacingColor';
    TextureFilePathIdent = 'textureFile';
    WarnOnNoTexture = true;
    CommonSection = 'common';
  public
    property Log: ILog read fLog;
    property Terrains: TTerrains read fTerrains;
    property Masks: zglPTexture read fMasks;
    procedure LoadTerrains(const aFileName: string);
    procedure LoadMasks(const aFileName: string);
    function GetTerrainsInfoAsText: string;
    function GetTypeColor(const aType: TTerrainType): LongWord;
    function Reverse: TObject;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

constructor TTerrain.Init;
begin
  Clean;
end;

procedure TTerrain.Clean;
begin
  id := 0;
  Name := '';
  Color := 0;
  Texture := nil;
end;

function TTerrain.ToText: string;
begin
  result := IntToStr(id) + ': ' + Name + '; RC=' + IntToHex(Color, 6);
end;

destructor TTerrain.Done;
begin
  if GlobalEngineRunning and Assigned(Texture) then
    tex_Del(Texture);
  Clean;
end;

{ TTerrainManager }

constructor TTerrainManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TTerrainManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'TerrainManager');
end;

procedure TTerrainManager.LoadTerrainsFromList(const aFile: TIniFile;
  const aList: TStrings);
var
  i: integer;
begin
  SetLength(fTerrains, aList.Count);
  for i := 0 to aList.Count - 1 do
  begin
    Terrains[i].Init;
    Terrains[i].Name := aList[i];
    Terrains[i].id := i;
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
    result := tex_LoadFromFile(MaskFilePath);
  end;

var
  sections: TStrings;
  s: string;
begin
  sections := TStringList.Create;
  aFile.ReadSections(sections);
  for s in sections do
    LoadMask(s);
  sections.Free;
end;

procedure TTerrainManager.ReleaseTerrains;
var
  i: integer;
begin
  for i := 0 to Length(Terrains) - 1 do
    Terrains[i].Done;
  SetLength(fTerrains, 0);
end;

procedure TTerrainManager.Finalize;
begin
  ReleaseTerrains;
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
  LoadTerrainTexture(aTerrain, aFile);
end;

procedure TTerrainManager.LoadTerrainTexture(var aTerrain: TTerrain;
  const aFile: TIniFile);
const
  DEBUG = true;
var
  textureFilePath: string;
begin
  textureFilePath := aFile.ReadString(aTerrain.Name, TextureFilePathIdent, '');
  if textureFilePath = '' then
  begin
    if WarnOnNoTexture then
      Log.Write('No texture for "' + aTerrain.Name + '" specified');
    aTerrain.Texture := nil;
  end
  else
  begin
    if DEBUG then
      Log.Write('Now loading texture for "' + aTerrain.Name + '" from:'
        + LineEnding + '"' + textureFilePath + '"');
    textureFilePath := GlobalApplicationPath + textureFilePath;
    if FileExists(textureFilePath) then
      aTerrain.Texture := tex_LoadFromFile(textureFilePath)
    else
      Log.Write(logTagError, 'File does not exists'
        + LineEnding + '"' + textureFilePath + '"');
  end;
end;

function TTerrainManager.GetTerrainsInfoAsText: string;
var
  i: integer;
begin
  result := 'Terrain types: ' + IntToStr(Length(Terrains)) + ' items total';
  for i := 0 to Length(Terrains) - 1 do
    result += LineEnding + Terrains[i].ToText;
end;

function TTerrainManager.GetTypeColor(const aType: TTerrainType): LongWord;
begin
  AssertIndexInBounds(0, aType, Length(Terrains) - 1, 'No such terrain type');
  result := Terrains[aType].Color;
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

