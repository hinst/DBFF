unit MapViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MapDataContainer;

type

  { TMapView }

  TMapView = class(TComponent)
  private
    fViewX, fViewY: integer;
  public
    property ViewX: integer read fViewX write fViewX;
    property ViewY: integer read fViewY write fViewY;
    procedure DrawTerrainLayer(const aMap: TMapData);
  end;

implementation

{ TMapView }

procedure TMapView.DrawTerrainLayer(const aMap: TMapData);
begin

end;

end.

