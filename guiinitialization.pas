unit GUIInitialization;

{$mode objfpc}{$H+}

interface
{
Factors of 360:
1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 18, 20, 24, 30, 36, 40, 45, 60, 72, 90, 120, 180, 360
}
uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ExtCtrls, CastleControl, CastleDialogs, CastleControls,
  CastleColors, CastleUIControls, CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform, CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleApplicationProperties, CastleLog, MainGameUnit, CastleTimeUtils,
  CastleKeysMouse;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    CastleOpenDialog1: TCastleOpenDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuExit: TMenuItem;
    FrontMenu: TMenuItem;
    Iso21Menu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MilitaryMenu: TMenuItem;
    ToolButton9: TToolButton;
    ToolPanel: TPanel;
    ToolButton8: TToolButton;
    TrueIsoMenu: TMenuItem;
    MenuTile: TMenuItem;
    MenuOpen: TMenuItem;
    MenuDebug: TMenuItem;
    MenuInfo: TMenuItem;
    NavPanel: TPanel;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    TrackBar1: TTrackBar;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TiltClick(Sender: TObject);
    procedure ViewMenuClick(Sender: TObject);
    procedure MenuDebugClick(Sender: TObject);
    procedure MenuInfoClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure RotateLeftClick(Sender: TObject);
    procedure RotateRightClick(Sender: TObject);
    procedure GrabSpriteClick(Sender: TObject);
    procedure OpenModelClick(Sender: TObject);
    procedure OpenDirectoryClick(Sender: TObject);
    procedure DebugClick(Sender: TObject);
    procedure ShowInfoClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
    procedure OpenModel;
    procedure ShowDebug;
    procedure UpdateCaption;
    procedure SetViewpoint(const ViewID: Integer);
  public
    CurrentFile: String;
    CurrentProjection: String;
  end;

var
  CastleForm: TCastleForm;

implementation

uses ShowCameraSettings;

{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  InitializeLog;
  AppTime := CastleGetTickCount64;
  Caption := 'Tilely';
  CurrentFile := EmptyStr;
  CurrentProjection := EmptyStr;
  Trackbar1.Visible := False;
end;

procedure TCastleForm.TiltClick(Sender: TObject);
begin
  // Tilt model forwards
  with CastleApp do
    begin
    if Assigned(Viewport) and not(SettingUp) then
      begin
        Inc(SceneTilt);
        if(SceneTilt > 3) then
          SceneTilt := 0;
        Scene.Rotation := Vector4(1, 0, 0, SceneTilt * 2 * (Pi / 4));
        Reflow;
      end;
    end;
end;

procedure TCastleForm.ViewMenuClick(Sender: TObject);
var
  ViewID: Integer;
begin
  with Sender as TMenuItem do
    begin
      CurrentProjection := Caption;
      ViewID := Integer(Tag);
    end;

  SetViewpoint(ViewID);
  UpdateCaption;
end;

procedure TCastleForm.UpdateCaption;
begin
  Caption := 'Tilely : ' + CurrentFile +
    ' shown in ' + CurrentProjection +
    ' at ' +
    FloatToStr((CastleApp.CameraRotation / CastleApp.CameraRotationSteps) * 360) +
    ' degrees rotation';
end;

procedure TCastleForm.SetViewpoint(const ViewID: Integer);
var
  i: Integer;
begin
  for i := 0 to PopupMenu1.Items.Count - 1 do
    PopupMenu1.Items[i].Checked := False;

  if ViewID < PopupMenu1.Items.Count then
    PopupMenu1.Items[ViewID].Checked := True;

  with CastleApp do
    begin
      case ViewID of
        0: CameraElevation := 0;
        1: CameraElevation := 9999;
        2: CameraElevation := 0.81625;
        3: CameraElevation := 1;
        4: CameraElevation := Sqrt(2);
        5: CameraElevation := 2;
        6: CameraElevation := 2;
      end;

      if Assigned(Viewport) and not(SettingUp) then
        begin
          Viewport := CreateView(Scene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
          Reflow;
        end;
  end;

  if ViewID = 7 then
    Trackbar1.Visible := True
  else
    Trackbar1.Visible := False;

end;

procedure TCastleForm.DebugClick(Sender: TObject);
begin
  ShowDebug;
end;

procedure TCastleForm.MenuDebugClick(Sender: TObject);
begin
  ShowDebug;
end;

procedure TCastleForm.ShowDebug;
begin
  With CastleApp do
    begin
      if Assigned(Scene) then
        begin
          Debug.Exists := not Debug.Exists;
          MenuDebug.Checked := Debug.Exists;
        end;
    end;
end;

procedure TCastleForm.MenuInfoClick(Sender: TObject);
begin
  With CastleApp do
    begin
      if Assigned(Scene) then
        begin
          InfoLabel.Exists := not InfoLabel.Exists;
          MenuInfo.Checked := InfoLabel.Exists;
        end;
    end;
end;

procedure TCastleForm.MenuOpenClick(Sender: TObject);
begin
  OpenModel;
end;

procedure TCastleForm.OpenModel;
var
  i: Integer;
begin
  CastleOpenDialog1.Filter := '3D Models|*.gltf;*.glb;*.obj;*.x3d;*.x3dv';
  if CastleOpenDialog1.Execute then
    begin
      if CastleOpenDialog1.Files.Count = 1 then
        begin
          CurrentFile := CastleOpenDialog1.Files[0];
          WriteLnLog('Opening ' + CurrentFile);
          CastleApp.LoadScene(CurrentFile);
          UpdateCaption;
        end
      else
        begin
          for i := 0 to CastleOpenDialog1.Files.Count - 1 do
            begin
              WriteLnLog('Opening ' + CastleOpenDialog1.Files[i]);
              CastleApp.LoadScene(CastleOpenDialog1.Files[i]);
            end;
        end;
    end;
  With CastleApp do
    begin
      if Assigned(Scene) then
        begin
          MenuDebug.Checked := Debug.Exists;
          MenuInfo.Checked := InfoLabel.Exists;
        end;
    end;
end;

procedure TCastleForm.RotateLeftClick(Sender: TObject);
begin
  with CastleApp do
    begin
      if Assigned(Viewport) and not(SettingUp) then
        begin
          CameraRotation := CameraRotation - 1;
          Viewport := CreateView(Scene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
          Reflow;
        end;
    end;
end;

procedure TCastleForm.RotateRightClick(Sender: TObject);
begin
  with CastleApp do
    begin
      if Assigned(Viewport) and not(SettingUp) then
        begin
          CameraRotation := CameraRotation + 1;
          Viewport := CreateView(Scene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
          Reflow;
        end;
    end;
end;

procedure TCastleForm.GrabSpriteClick(Sender: TObject);
begin
  With CastleApp do
    GrabSprite(Trunc(ViewWidth), Trunc(ViewHeight), 8);
end;

procedure TCastleForm.OpenModelClick(Sender: TObject);
begin
  OpenModel;
end;

procedure TCastleForm.OpenDirectoryClick(Sender: TObject);
begin
//  OpenDirectory;
end;

procedure TCastleForm.ShowInfoClick(Sender: TObject);
begin
  CameraForm.Show;
end;

procedure TCastleForm.TrackBar1Change(Sender: TObject);
var
  campos: Single;
begin
  campos := TrackBar1.Position / (TrackBar1.Max + 1);
  campos := 1-(1/(1-campos));
  if campos < -10000 then
    campos := -9999;
  with CastleApp do
    begin
      if Assigned(Viewport) and not(SettingUp) then
        begin
          CameraElevation := -campos;
          Viewport := CreateView(Scene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
          Reflow;
        end;
    end;
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  Width := 640 + NavPanel.Width;
  Height := 640 + ToolBar1.Height + MainMenu1.Height;
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Window);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usNone;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
end;

end.

