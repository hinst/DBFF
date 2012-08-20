unit MapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  MapUnitFace,
  MapDataFace;

type
  TMapUnit = class(IMapUnit)
    procedure Draw; virtual; abstract;
  end;

implementation

end.

