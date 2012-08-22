unit ZenGLShowMeFPS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_main,
  zgl_font;

type

  { TFPSDrawer }

  TFPSDrawer = class
  public
    constructor Create;
  private
    fFont: zglPFont;
    fOwnsFont: boolean;
    procedure Initialize;
    procedure Finalize;
  public
    POwnsFont: ^boolean;
    Font: zglPFont;
    destructor Destroy; override;
  end;

implementation

{ TFPSDrawer }

constructor TFPSDrawer.Create;
begin

end;

procedure TFPSDrawer.Initialize;
begin
  fOwnsFont := true;
  POwnsFont := @fOwnsFont;
end;

procedure TFPSDrawer.Finalize;
begin

end;

destructor TFPSDrawer.Destroy;
begin
  inherited Destroy;
end;

end.

