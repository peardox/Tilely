program TilelyLaz;

{$mode objfpc}{$H+}
{$ifndef cgeapp}
{$NOTE Lazarus project}
{$else}
{$NOTE CGE project}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GUIInitialization, castle_components, castle_base, ShowCameraSettings,
  MiscHelpers, RGBAlphaImageHelp
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tilely';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCastleForm, CastleForm);
  Application.CreateForm(TCameraForm, CameraForm);
  Application.Run;
end.

