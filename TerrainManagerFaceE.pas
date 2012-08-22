unit TerrainManagerFaceE;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Common,

  zgl_textures,

  ZenGLFCLGraphics,

  MapDataFace,
  TerrainManagerFace;

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

  ITerrainManagerE = interface(ITerrainManager)
    function GetMasks: TMultiTexture;
    property Masks: TMultiTexture read GetMasks;
    function GetTerrains: TTerrains;
    property Terrains: TTerrains read GetTerrains;
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

end.

