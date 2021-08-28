unit ShowCameraSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CastleCameras, CastleViewport, CastleQuaternions,
  MiscHelpers;

type

  { TCameraForm }

  TCameraForm = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private

  public
    procedure ShowStats(const AViewport: TCastleViewport);
  end;

var
  CameraForm: TCameraForm;

implementation

{$R *.lfm}

 uses GUIInitialization, MainGameUnit;

{ TCameraForm }

procedure TCameraForm.FormShow(Sender: TObject);
begin
  Top := CastleForm.Top;
  Left := CastleForm.Left + CastleForm.Width + 4;
  ShowStats(CastleApp.Viewport);
end;

procedure TCameraForm.ShowStats(const AViewport: TCastleViewport);
var
  Q: TQuaternion;
  Extents: TExtents;
begin
  With CastleApp do
    begin
      if Assigned(Scene) and Assigned(AViewport) then
        begin
          Q := OrientationQuaternionFromDirectionUp(AViewport.Camera.Direction, AViewport.Camera.Up);
          Extents := AViewport.CalcAngles(Scene);

          Label6.Caption  := FormatFloat('##0.00000', AViewport.Camera.Position.X);
          Label7.Caption  := FormatFloat('##0.00000', AViewport.Camera.Position.Y);
          Label8.Caption  := FormatFloat('##0.00000', AViewport.Camera.Position.Z);

          Label10.Caption := FormatFloat('##0.00000', AViewport.Camera.Direction.X);
          Label11.Caption := FormatFloat('##0.00000', AViewport.Camera.Direction.Y);
          Label12.Caption := FormatFloat('##0.00000', AViewport.Camera.Direction.Z);

          Label14.Caption := FormatFloat('##0.00000', AViewport.Camera.Up.X);
          Label15.Caption := FormatFloat('##0.00000', AViewport.Camera.Up.Y);
          Label16.Caption := FormatFloat('##0.00000', AViewport.Camera.Up.Z);

          Label18.Caption := FormatFloat('##0.00000', CameraElevation);
          Label20.Caption := FormatFloat('##0', (CameraRotation / CameraRotationSteps) * 360);

          Label26.Caption := FormatFloat('##0.00000', Extents.Max.X);
          Label29.Caption := FormatFloat('##0.00000', Extents.Min.X);
          Label27.Caption := FormatFloat('##0.00000', Extents.Size.X);

          Label30.Caption := FormatFloat('##0.00000', Extents.Max.Y);
          Label31.Caption := FormatFloat('##0.00000', Extents.Min.Y);
          Label32.Caption := FormatFloat('##0.00000', Extents.Size.Y);

          Label34.Caption := FormatFloat('####0', Extents.Pixels.X);
          Label36.Caption := FormatFloat('####0', Extents.Pixels.Y);
          Label38.Caption := FormatFloat('##0.00000', Extents.Aspect);
          Label40.Caption := FormatFloat('##0.00000', AViewport.Camera.Orthographic.Scale);
        end;
    end;
end;

end.

