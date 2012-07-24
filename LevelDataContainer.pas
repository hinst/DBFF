unit LevelDataContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  LevelDataFace,
  MapDataFace,
  MapDataContainer;

type

  { TLevelData }

  TLevelData = class(TComponent, ILevelData)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fMap: TMapData;
    procedure Initialize;
    function GetMap: IMapData;
  public
    property Map: TMapData read fMap;
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
  fMap := TMapData.Create(self);
end;

function TLevelData.GetMap: IMapData;
begin
  result := fMap;
end;

end.

