unit MapViewerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils;

type
  IMapView = interface
    function GetViewXLeft: single;
    property ViewXCorner: single read GetViewXLeft;
    function GetViewYUp: single;
    property ViewYCorner: single read GetViewYUp;
    function GetFieldWidth: single;
    property FieldWidth: single read GetFieldWidth;
    function GetFieldHeight: single;
    property FieldHeight: single read GetFieldWidth;
  end;

implementation

end.

