unit MapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type


  TMapUnit = class(IMapUnit)
  protected
    function GetOccupatedCells: TCellNumbers; virtual; abstract;
  public
    property OccupatedCells: TCellNumbers read GetOccupatedCells;
    procedure Draw(const aScroll: TMapScrollManager); virtual; abstract;
    procedure Update(const aTime: double); virtual; abstract;
  end;

implementation

end.

