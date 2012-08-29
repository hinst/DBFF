unit TerrainManagerFaceE;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  ZenGLFCLGraphics,

  Common,
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
    Vehicleable: boolean;
    function ToText: string;
    destructor Done;
  end;

  PTerrain = ^TTerrain;

  TTerrains = array of TTerrain;

  ITerrainManagerE = interface(ITerrainManager) ['ITerrainManagerE']
    function GetMasks: TMultiTexture;
    property Masks: TMultiTexture read GetMasks;
    function GetTerrain(const aIndex: integer): PTerrain;
    property Terrains[const aIndex: integer]: PTerrain read GetTerrain;
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
  result := IntToStr(id) + ': ' + Name + '; RC=' + IntToHex(Color, 6)
    + '; VH=' + BoolToStr(Vehicleable, true);
end;

destructor TTerrain.Done;
begin
  if GlobalEngineRunning and Assigned(Texture) then
    tex_Del(Texture);
  Clean;
end;

end.

