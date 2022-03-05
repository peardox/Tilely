unit GUIInitialization;

{$mode objfpc}{$H+}

interface
{
Factors of 360:
1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 18, 20, 24, 30, 36, 40, 45, 60, 72, 90, 120, 180, 360
Factors of 11520
1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 30, 32, 36, 40, 45, 48, 60, 64, 72, 80, 90, 96, 120, 128, 144, 160, 180, 192, 240, 256, 288, 320, 360, 384, 480, 576, 640, 720, 768, 960, 1152, 1280, 1440, 1920, 2304, 2880, 3840, 5760, 11520
}
uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ExtCtrls, MaskEdit, StdCtrls, CastleControl, CastleDialogs,
  CastleControls, CastleColors, CastleUIControls, CastleTriangles, CastleShapes,
  CastleVectors, CastleSceneCore, CastleScene, CastleTransform, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleApplicationProperties, CastleLog, MainGameUnit, CastleTimeUtils,
  anitxtrec, CastleFilesUtils, CastleKeysMouse, CastleGLVersion;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    CastleOpenDialog1: TCastleOpenDialog;
    CheckOversample: TCheckBox;
    EditHeight: TEdit;
    EditFrames: TEdit;
    EditWidth: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuExit: TMenuItem;
    FrontMenu: TMenuItem;
    Iso21Menu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuGrabSprite: TMenuItem;
    LoadAniTxt: TMenuItem;
    MenuGrabAll: TMenuItem;
    MilitaryMenu: TMenuItem;
    Splitter1: TSplitter;
    StaticText1: TStaticText;
    ToolButton10: TToolButton;
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
    procedure CheckOversampleChange(Sender: TObject);
    procedure EditHeightChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadAniTxtClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuGrabAllClick(Sender: TObject);
    procedure TiltClick(Sender: TObject);
    procedure TextureAltasClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
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
    procedure SetViewpoint;
    procedure SwitchToSelectedNode;
    procedure GrabAll;
  public
    CurrentFile: String;
    CurrentModel: String;
    CurrentProjection: String;
    ViewID: Integer;
  end;

var
  CastleForm: TCastleForm;

const
  DegreeSign = #$C2#$B0;  //this is the Utf8-sequnce for this symbol

implementation

uses ShowCameraSettings, CastleURIUtils, MiscHelpers;

{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  AppTime := CastleGetTickCount64;
  Caption := 'Tilely';
  CurrentFile := EmptyStr;
  CurrentModel := EmptyStr;
  CurrentProjection := EmptyStr;
  Trackbar1.Visible := False;
end;

procedure TCastleForm.LoadAniTxtClick(Sender: TObject);
begin
  CastleApp.LoadSubActions('C:\\saved\\dev\\GLTF\\crazy-rabbits-animations-list.txt');
end;

procedure TCastleForm.MenuExitClick(Sender: TObject);
begin
  Exit;
end;

procedure TCastleForm.MenuGrabAllClick(Sender: TObject);
begin
  GrabAll;
end;

procedure TCastleForm.TiltClick(Sender: TObject);
begin
  // Tilt model forwards
  CastleApp.ToggleBorders;
  Exit;
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

procedure TCastleForm.TextureAltasClick(Sender: TObject);
begin
  GrabAll;
end;

procedure TCastleForm.GrabAll;
var
  SavePath: String;
begin
  SavePath := 'tests/check';
  CheckForceDirectories(SavePath);
//  Directions := StrToIntDef(EditFrames.Text, 1);
  if Assigned(CastleApp) then
    begin
      With CastleApp do
        begin
          WriteLnLog('Starting render');
          if UseOversample then
            GrabAtlas(Trunc(ViewWidth), Trunc(ViewHeight), SavePath, 0, 8)
          else
            GrabAtlas(Trunc(ViewWidth), Trunc(ViewHeight), SavePath, 1, 1);
          WriteLnLog('Finished render');
        end;
    end;
end;

procedure TCastleForm.SwitchToSelectedNode;
var
  Node: TTreeNode;
  Model: TSpritelyModel;
begin
  Node := Treeview1.Selected;
  if (Node = nil) then // Nothing to do
    exit;

  if (TObject(Node.Data).ClassName = 'TSpritelyModel') then
    begin
      With CastleApp do
        begin
          Model := TSpritelyModel(Node.Data);
          Scene := Model.Scene;
          CurrentModel := Model.ModelName;
          CastleApp.OriginalSize := Model.RealSize;
          AddDebugBox(Scene);
          SynchTrackbar;
          Viewport := CreateView(Scene);
          Reflow;
        end;
    end;
end;

procedure TCastleForm.TreeView1Click(Sender: TObject);
begin
//  SwitchToSelectedNode;
end;

procedure TCastleForm.TreeView1SelectionChanged(Sender: TObject);
begin
  SwitchToSelectedNode;
end;

procedure TCastleForm.ViewMenuClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    begin
      CurrentProjection := Caption;
      ViewID := Integer(Tag);
    end;

  SetViewpoint;
//  UpdateCaption;
end;

procedure TCastleForm.UpdateCaption;
begin
  with CastleApp do
    begin
      Caption := 'Tilely | '  + GLVersion.Renderer + ' | ' + CurrentModel +
        ' | ' + CurrentProjection +
        ' @ ' +
        FloatToStr((CameraRotation / CameraRotationSteps) * 360) + ' ' + DegreeSign;
      if not(OriginalSize.IsZero) then
        begin
          Caption := Caption +
          ' | Real - W : ' + FormatFloat('####0.00', OriginalSize.X) +
          ' H : ' + FormatFloat('####0.00', OriginalSize.Z) +
          ' D : ' + FormatFloat('####0.00', OriginalSize.Y);
        end;
      if Assigned(Scene) then
        begin
          if not(Scene.Scale.IsZero) then
            begin
              Caption := Caption +
              ' | Scale - W : ' + FormatFloat('####0.00', Scene.BoundingBox.SizeX) +
              ' H : ' + FormatFloat('####0.00', Scene.BoundingBox.SizeZ) +
              ' D : ' + FormatFloat('####0.00', Scene.BoundingBox.SizeY);
            end;
        end;
    end;
end;

procedure TCastleForm.SetViewpoint;
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
          Viewport := CreateView(Scene);
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
  modelNode: TTreeNode;
  newScene: TCastleScene;
  newModel: TSpritelyModel;
begin
  CastleOpenDialog1.Filter := '3D Models|*.glb;*.gltf;*.obj;*.x3dv;*.x3dvz;*.x3dv.gz;*.x3d;*.x3dz;*.x3d.gz;*.x3db;*.x3db.gz;*.wrl;*.wrz;*.wrl.gz';
  if CastleOpenDialog1.Execute then
    begin
      if CastleOpenDialog1.Files.Count = 1 then
        begin
          CurrentFile := CastleOpenDialog1.Files[0];
          CurrentModel := StripExtension(ExtractURIName(CurrentFile));
          WriteLnLog('Opening ' + CurrentFile);
          newScene := CastleApp.LoadScene(CurrentFile);
          if not(newScene = nil) then
            begin
              newModel := TSpritelyModel.Create(Self, CurrentFile, newScene, CastleApp.OriginalSize);
              modelNode := Treeview1.Items.AddObject(nil, newModel.ModelName, newModel);
            end;
        end
      else
        begin
          for i := 0 to CastleOpenDialog1.Files.Count - 1 do
            begin
              CurrentFile := CastleOpenDialog1.Files[i];
              CurrentModel := StripExtension(ExtractURIName(CurrentFile));
              WriteLnLog('Opening ' + CurrentFile);
              newScene := CastleApp.LoadScene(CurrentFile);
              if not(newScene = nil) then
                begin
                  newModel := TSpritelyModel.Create(Self, CurrentFile, newScene, CastleApp.OriginalSize);
                  modelNode := Treeview1.Items.AddObject(nil, newModel.ModelName, newModel);
                end;
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
          Viewport := CreateView(Scene);
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
          Viewport := CreateView(Scene);
          Reflow;
        end;
    end;
end;

procedure TCastleForm.GrabSpriteClick(Sender: TObject);
begin
  if Assigned(CastleApp) then
    begin
      With CastleApp do
        begin
          if UseOversample then
            GrabSprite(Trunc(ViewWidth), Trunc(ViewHeight), 8)
          else
            GrabSprite(Trunc(ViewWidth), Trunc(ViewHeight), 1);
        end;
    end;
end;

procedure TCastleForm.CheckOversampleChange(Sender: TObject);
begin
  With CastleApp do
    UseOversample := CheckOversample.Checked;
end;

procedure TCastleForm.EditHeightChange(Sender: TObject);
var
  TestArg: Integer;
begin
  TestArg := StrToIntDef(EditHeight.Text, 0);
  if Assigned(CastleApp) then
    begin
      with CastleApp do
        begin
          ViewHeight := TestArg;
          if Assigned(Viewport) and not(SettingUp) then
            begin
              Viewport := CreateView(Scene);
              Reflow;
            end;
        end;
    end;
end;

procedure TCastleForm.EditWidthChange(Sender: TObject);
var
  TestArg: Integer;
begin
  TestArg := StrToIntDef(EditWidth.Text, 0);
  if Assigned(CastleApp) then
    begin
      with CastleApp do
        begin
          ViewWidth := TestArg;
          if Assigned(Viewport) and not(SettingUp) then
            begin
              Viewport := CreateView(Scene);
              Reflow;
            end;
        end;
    end;
end;

procedure TCastleForm.OpenModelClick(Sender: TObject);
begin
  OpenModel;
end;

procedure TCastleForm.OpenDirectoryClick(Sender: TObject);
begin
//  OpenDirectory;
  ShowMessage('Coming Soon : For now open multiple files at once.' + LineEnding + 'e.g. Ctrl-A in the Open File dialog');
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
          Viewport := CreateView(Scene);
          Reflow;
        end;
    end;
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  InitializeLog;

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

