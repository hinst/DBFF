unit TerrainManager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,
  zgl_textures,
  IniFiles,
  NiceExceptions,
  StringFeatures,
  TerrainManagerFace,
  MapDataFace;

type

  { TTerrain }

  TTerrain = object
  public
    constructor Init;
  public
    id: TTerrainType;
    Name: string;
    Color: LongWord;
    Texture: zglPTexture;
    procedure Clean;
    destructor Done;
  end;

  TTerrains = array of TTerrain;

  { TTerrainManager }

  TTerrainManager = class(TComponent)
  private
    fTerrains: TTerrains;
    procedure ReleaseTerrains;
    procedure Finalize;
  public const
    ColorIdent = 'replacingColor';
  public
    property Terrains: TTerrains read fTerrains;
    procedure LoadTerrains(const aFileName: string);
    procedure LoadTerrains(const aFile: TIniFile);
    procedure LoadTerrain(var aTerrain: TTerrain; const aFile: TIniFile);
    destructor Destroy; override;
  end;

implementation

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

destructor TTerrain.Done;
begin
  if Assigned(Texture) then
    tex_Del(Texture);
  Clean;
end;

{ TTerrainManager }

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

procedure TTerrainManager.LoadTerrains(const aFile: TIniFile);
var
  sections: TStrings;
  i: integer;
begin
  sections := TStringList.Create;
  aFile.ReadSections(sections);
  SetLength(fTerrains, sections.Count);
  for i := 0 to sections.Count - 1 do
  begin
    Terrains[i].Init;
    Terrains[i].Name := sections[i];
    Terrains[i].id := i;
    LoadTerrain(Terrains[i], aFile);
  end;
  sections.Free;
end;

procedure TTerrainManager.LoadTerrain(var aTerrain: TTerrain;
  const aFile: TIniFile);
begin
  aTerrain.Color := StrHexToLongWord(aFile.ReadString(aTerrain.Name, ColorIdent, '000000'));
  aTerrain.Texture := nil;
end;

destructor TTerrainManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

