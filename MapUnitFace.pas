unit MapUnitFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  MapScrollManager;

type
  IMapUnit = interface
    procedure Draw(const aScroll: TMapScrollManager);
  end;

implementation

end.

