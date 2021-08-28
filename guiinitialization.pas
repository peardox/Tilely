unit GUIInitialization;

{$mode objfpc}{$H+}

interface

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
    MenuTile: TMenuItem;
    MenuOpen: TMenuItem;
    MenuDebug: TMenuItem;
    MenuInfo: TMenuItem;
    NavPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    TrackBar1: TTrackBar;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuDebugClick(Sender: TObject);
    procedure MenuInfoClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
    procedure OpenModel;
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
end;

procedure TCastleForm.MenuDebugClick(Sender: TObject);
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
          WriteLnLog(CastleOpenDialog1.Files[0]);
          CastleApp.LoadScene(CastleOpenDialog1.Filename);
          Caption := 'Tilely : ' + CastleOpenDialog1.Filename;
        end
      else
        begin
          for i := 0 to CastleOpenDialog1.Files.Count - 1 do
            begin
              WriteLnLog(CastleOpenDialog1.Files[i]);
              CastleApp.LoadScene(CastleOpenDialog1.Files[i]);
            end;
        end;
    end;
  With CastleApp do
    begin
      if Assigned(Scene) then
        begin
          MenuDebug.Checked := Debug.Exists;
          MenuInfo.Checked := Scene.HeadlightOn
        end;
    end;
end;

procedure TCastleForm.ToolButton1Click(Sender: TObject);
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

procedure TCastleForm.ToolButton2Click(Sender: TObject);
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

procedure TCastleForm.ToolButton3Click(Sender: TObject);
begin
  With CastleApp do
    GrabSprite(512, 512, 1);
end;

procedure TCastleForm.ToolButton4Click(Sender: TObject);
begin
  OpenModel;
end;

procedure TCastleForm.ToolButton6Click(Sender: TObject);
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
          CameraElevation := campos;
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
  Width := 640 + TrackBar1.Width + NavPanel.Width;
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

