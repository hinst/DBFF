unit EngineManagerFace;

{$mode objfpc}{$H+}
{$interfaces CORBA}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_render_target,

  SynchroThread;

type
  IEngineManager = interface
    function GetBatch: TSynchroThread;
    property Batch: TSynchroThread read GetBatch;
    function LoadTexture(const aFileName: string): zglPTexture;
    function DirectCopyTexture(const aTexture: zglPTexture): zglPTexture;
    procedure DirectCleanTexture(const aTexture: zglPRenderTarget);
    function CreateRenderTarget(const aWidth, aHeight: integer): zglPRenderTarget;
    procedure DisposeTexture(var aTexture: zglPTexture);
  end;

implementation

end.

