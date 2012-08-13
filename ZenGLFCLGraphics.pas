unit ZenGLFCLGraphics;

{$mode objfpc}{$H+}
{$UNDEF DEBUG_MULTITEXTURESETGETFAREA}

interface

uses
  FPimage,
  SysUtils,
  Classes,
  SyncObjs,

  zgl_textures,
  zgl_sprite_2d,
  zgl_render_target,

  NiceExceptions,
  LogManager,
  LogEntityFace,
  SynchroThread,

  EngineManagerFace
  ;


type

  TMultiTexture = class;

  { TMultiTexture }

  TMultiTexture = class
  public
    constructor Create;
  private
    fEngine: IEngineManager;
    ffArea: zglPRenderTarget;
    fCount: integer;
    fWidth: integer;
    fHeight: integer;
    procedure SetfArea(const afArea: zglPRenderTarget); inline;
    function GetfArea: zglPRenderTarget; inline;
    property fArea:zglPRenderTarget read GetfArea write SetfArea;
    procedure Clean;
    procedure Allocate(const aWidth, aHeight: integer);
  public type

    { TAddTexture }

    TAddTexture = class(ISynchroJob)
    public
      constructor Create(const aMulti: TMultiTexture; const aTexture: zglPTexture);
    private
      fLog: ILog;
      fMultiTexture: TMultiTexture;
      fTexture: zglPTexture;
    public
      property Log: ILog read fLog write fLog;
      property MultiTexture: TMultiTexture read fMultiTexture;
      property Texture: zglPTexture read fTexture;
      procedure Execute;
    end;

    { TFinishArea }

    TFinishArea = class(ISynchroJob)
    public
      constructor Create(const aMulti: TMultiTexture);
      procedure Execute;
    private
      MultiTexture: TMultiTexture;
    end;

  public
    property Engine: IEngineManager read fEngine write fEngine;
    property Area: zglPRenderTarget read GetfArea;
    property Count: integer read fCount write fCount;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    procedure DirectAdd(const aTexture: zglPTexture);
    procedure Add(const aTexture: zglPTexture);
    procedure DirectFinishArea;
    procedure FinishArea;
    procedure ReleaseArea;
    destructor Destroy; override;
  end;

  { TCreateRenderTarget }

  TCreateRenderTarget = class(ISynchroJob)
  public
    constructor Create(const aWidth, aHeight: integer);
  public
    Width, Height: integer;
    Result: zglPRenderTarget;
    procedure Execute;
  end;

  { TLoadTexture }

  TLoadTexture = class(ISynchroJob)
  public
    constructor Create(const aFileName: string);
  private
    fFileName: string;
    fResult: zglPTexture;
  public
    procedure Execute;
    property FileName: string read fFileName;
    property Result: zglPTexture read fResult;
  end;

  { TDisposeTexture }

  TDisposeTexture = class(ISynchroJob)
  public
    constructor Create(const aTexture: zglPTexture);
  private
    fTexture: zglPTexture;
  public
    property Texture: zglPTexture read fTexture;
    procedure Execute;
    destructor Destroy; override;
  end;

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;

implementation

function FPColorToLongWordColor(const aColor: TFPColor): LongWord;
begin
  result :=0;
  result += (aColor.red shr 8) * 256 * 256;
  result += (aColor.green shr 8) * 256;
  result += (aColor.blue shr 8) * 1;
end;

{ TCreateRenderTarget }

constructor TCreateRenderTarget.Create(const aWidth, aHeight: integer);
begin
  inherited Create;
  Width := aWidth;
  Height := aHeight;
end;

procedure TCreateRenderTarget.Execute;
begin
  Result := rtarget_Add(tex_CreateZero(Width, Height), RT_DEFAULT);
end;

{ TMultiTexture.TFinishArea }

constructor TMultiTexture.TFinishArea.Create(const aMulti: TMultiTexture);
begin
  MultiTexture := aMulti;
end;

procedure TMultiTexture.TFinishArea.Execute;
begin
  AssertAssigned(MultiTexture, 'MultiTexture');
  MultiTexture.DirectFinishArea;
end;

{ TLoadTexture }

constructor TLoadTexture.Create(const aFileName: string);
begin
  inherited Create;
  fFileName := aFileName;
end;

procedure TLoadTexture.Execute;
begin
  AssertFileExists(FileName);
  fResult := tex_LoadFromFile(FileName);
end;

{ TDisposeTexture }

constructor TDisposeTexture.Create(const aTexture: zglPTexture);
begin
  inherited Create;
  fTexture := aTexture;
end;

procedure TDisposeTexture.Execute;
begin
  AssertAssigned(Texture, 'Texture');
  tex_Del(fTexture);
end;

destructor TDisposeTexture.Destroy;
begin
  inherited Destroy;
end;

{ TMultiTexture.TAddTexture }

constructor TMultiTexture.TAddTexture.Create(const aMulti: TMultiTexture;
  const aTexture: zglPTexture);
begin
  inherited Create;
  fMultiTexture := aMulti;
  fTexture := aTexture;
end;

procedure TMultiTexture.TAddTexture.Execute;
begin
  MultiTexture.DirectAdd(Texture);
end;

{ TMultiTexture }

constructor TMultiTexture.Create;
begin
  inherited Create;
  Clean;
end;

procedure TMultiTexture.SetfArea(const afArea: zglPRenderTarget);
begin
  {$IFDEF DEBUG_MULTITEXTURESETGETFAREA}
  WriteLN('SetArea ', afArea <> nil);
  {$ENDIF}
  ffArea := afArea;
end;

function TMultiTexture.GetfArea: zglPRenderTarget;
begin
  result := ffArea;
  {$IFDEF DEBUG_MULTITEXTURESETGETFAREA}
  WriteLN('GetArea ', result <> nil);
  {$ENDIF}
end;

procedure TMultiTexture.Clean;
begin
  fArea := nil;
  fCount := 0;
  fWidth := 0;
  fHeight := 0;
end;

procedure TMultiTexture.Allocate(const aWidth, aHeight: integer);
var
  newArea: zglPRenderTarget;
begin
  fWidth := aWidth;
  fHeight := aHeight;
  if Count = 0 then
    raise Exception.Create('Multi texture count property unassigned');
  newArea := rtarget_Add(tex_CreateZero(Width * Count, Height), RT_DEFAULT);
  AssertAssigned(newArea, 'newArea');
  fArea := newArea;
end;

procedure TMultiTexture.DirectAdd(const aTexture: zglPTexture);
begin
  if Width = 0 then
  begin
    Allocate(aTexture^.Width, aTexture^.Height);
    fCount := 0;
  end;
  rtarget_Set(Area);
  ssprite2d_Draw(aTexture, Count * Width, 0, Width, Height, 0);
  rtarget_Set(nil);
  inc(fCount);
end;

procedure TMultiTexture.Add(const aTexture: zglPTexture);
var
  job: ISynchroJob;
begin
  AssertAssigned(Engine, 'Engine');
  job := TAddTexture.Create(self, aTexture);
  Engine.Batch.Execute(job);
end;

procedure TMultiTexture.DirectFinishArea;
begin
  AssertAssigned(Area, 'Area');
  tex_SetFrameSize(Area^.Surface, Width, Height);
end;

procedure TMultiTexture.FinishArea;
var
  job: TFinishArea;
begin
  job := TFinishArea.Create(self);
  Engine.Batch.Execute(job);
end;

procedure TMultiTexture.ReleaseArea;
begin
  {$IFDEF DEBUG_MULTITEXTURESETGETFAREA}
  WriteLN('Releasing area ', ffArea <> nil);
  {$ENDIF}
  rtarget_Del(ffArea);
end;

destructor TMultiTexture.Destroy;
begin
  inherited Destroy;
end;

end.

