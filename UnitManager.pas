unit UnitManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,

  UnitManagerFace,
  MapUnit;

type
  TUnitList = specialize TFPGList<TMapUnit>;

  { TUnitManager }

  TUnitManager = class(TComponent, IUnitManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fMapUnits: TUnitList;
    procedure Initialize;
    procedure Finalize;
  public
    property MapUnits: TUnitList read fMapUnits;
    destructor Destroy; override;
  end;

implementation

{ TUnitManager }

constructor TUnitManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TUnitManager.Initialize;
begin
  fMapUnits := TUnitList.Create;
end;

procedure TUnitManager.Finalize;
begin
  FreeAndNil(fMapUnits);
end;

destructor TUnitManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

