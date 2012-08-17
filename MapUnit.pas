unit MapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  MapDataFace;

type
  TMapUnit = class
  end;

  { TBuilding }

  TBuilding = class(TMapUnit)
  private
    fTexture: zglPTexture;
    fLeftTopCell: TCellNumber;
    function GetLeftTopCell: PCellNumber; inline;
  public
    property Texture: zglPTexture read fTexture;
    property LeftTopCell: PCellNumber read GetLeftTopCell;
  end;

implementation

{ TBuilding }

function TBuilding.GetLeftTopCell: PCellNumber;
begin
  resutl := @fLeftTopCell;
end;

end.

