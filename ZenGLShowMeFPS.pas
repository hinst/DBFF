unit ZenGLShowMeFPS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  NiceExceptions,

  zgl_main,
  zgl_utils,
  zgl_window,
  zgl_primitives_2d,
  zgl_font,
  zgl_text;

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
  public const
    Gap = 6;
  public
    POwnsFont: ^boolean;
    Font: zglPFont;
    procedure Draw;
    destructor Destroy; override;
  end;

implementation

{ TFPSDrawer }

constructor TFPSDrawer.Create;
begin
  Initialize;
end;

procedure TFPSDrawer.Initialize;
begin
  fOwnsFont := true;
  POwnsFont := @fOwnsFont;
end;

procedure TFPSDrawer.Finalize;
begin
  if POwnsFont^ then
    font_Del(fFont);
end;

procedure TFPSDrawer.Draw;
var
  textHeight: single;
  textWidth: single;
  text: string;

  procedure DrawBackgroundRectangle;
  begin
    pr2d_Rect(
      wndWidth - Gap - Gap - textWidth - Gap,
      wndHeight - Gap - Gap - textHeight - Gap,
      Gap + textWidth + Gap,
      Gap + textHeight + Gap,
      $FFFFFF,
      255
    );
    pr2d_Rect(
      wndWidth - Gap - Gap - textWidth - Gap,
      wndHeight - Gap - Gap - textHeight - Gap,
      Gap + textWidth + Gap,
      Gap + textHeight + Gap,
      $FFFFFF,
      255 div 2,
      PR2D_FILL
    );
  end;

begin
  AssertAssigned(Font, 'Font');
  text := u_IntToStr( zgl_Get( RENDER_FPS ) );
  textWidth := text_GetWidth(Font, text);
  textHeight := text_GetHeight(Font, textWidth, text, 1);
  DrawBackgroundRectangle;
  text_DrawEx(
    Font,
    wndWidth - Gap - Gap - textWidth, // X
    wndHeight - Gap - Gap - textHeight, // Y
    1, 0, // scale, step
    text,
    255, // Alpha
    $000000, // Color
    0 // Flags
  );
end;

destructor TFPSDrawer.Destroy;
begin
  inherited Destroy;
end;

end.

