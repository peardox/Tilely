unit MainGameUnit;

{$mode objfpc}{$H+}
// {$define devmode}
// {$define devstream}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, ComCtrls, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DLoadInternalOBJ, X3DNodes, X3DFields, X3DTIme, CastleRectangles,
  CastleImages, CastleGLImages, CastleDebugTransform,
  CastleTextureImages, CastleBoxes,
  CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleFilesUtils, CastleKeysMouse, CastleNotifications,
  RGBAlphaImageHelp, MiscHelpers, CastleURIUtils,
  anitxtrec, ZipUrls, CastleDownload;

type
  { TAnimationController }
  TAnimationController = record
    AnimationName: String;
    SavePath: String;
    SaveHeading: String;
    SubAction: Cardinal;
    Frame: Integer;
    FrameCount: Integer;
    FrameTime: TFloatTime;
    SpriteWidth: Integer;
    SpriteHeight: Integer;
    OverSample: Integer;
    UseTransparency: Boolean;
    AtlasImage: TCastleImage;
    FrameX: Integer;
    FrameY: Integer;
    Action: Cardinal;
    CallCounter: Cardinal;
  end;

  { TSpritelyModel }

  TSpritelyModel = class(TComponent)
  private
    fFileName: String;
    fModelName: String;
    fScene: TCastleScene;
    fRealSize: TVector3;
  public
    property FileName: String read fFileName write fFileName;
    property ModelName: String read fModelName write fModelName;
    property Scene: TCastleScene read fScene write fScene;
    property RealSize: TVector3 read fRealSize write fRealSize;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AFile: String; AScene: TCastleScene; ASize: TVector3);
  end;

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    ViewUI: TCastleRectangleControl;
    ViewPane: TCastleRectangleControl;
    ViewBack: TCastleRectangleControl;
    ViewGrid: TCastleImageControl;
    fCameraRotation: Integer;
    fCameraRotationSteps: Integer;
    fModelRotation: Integer;
    fModelRotationSteps: Integer;
    fCameraElevation: Single;
    ViewScale: Single;
    OriginalScale: Single;
    OriginalWidth: Single;
    StretchMultiplier: Single;
    Animating: Boolean;
    AniRec: TAnimationController;
    TextureAtlasX: Cardinal;
    TextureAtlasY: Cardinal;
    SubActionList: TSubActionArray;
    DoingModel: String;
    DoingNode: Integer;
    procedure setCameraRotation(const AValue: Integer);
    procedure setCameraRotationSteps(const AValue: Integer);
    procedure setModelRotation(const AValue: Integer);
    procedure setModelRotationSteps(const AValue: Integer);
    procedure setCameraElevation(const AValue: Single);
  public
    {$ifdef devmode}
    tz: Array [0..9] of TZipFileSystem;
    {$endif}
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Debug: TDebugTransformBox;
    InfoNote: TCastleLabel;
    SettingUp: Boolean;
    LastWidth: Single;
    ViewWidth: Integer;
    ViewHeight: Integer;
    UseOversample: Boolean;
    SceneTilt: Integer;
    OriginalSize: TVector3;
    Abort: Boolean;
    AbortModel: Boolean;
    AbortAction: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure BootStrap(Sender: TObject);
    procedure FrameSync(Sender: TObject);
    procedure Reflow(const CalledFromResize: Boolean = False);
    procedure LoadUI;
    procedure LoadSubActions(AniFile: String);
    function LoadScene(const AFile: String): TCastleScene;
    function CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False; const useMainViewport: Boolean = True): TCastleImage;
    procedure ShowAppMessage(const AMsg: String);
    procedure MakeAtlas;
    procedure GrabAtlas(const SpriteWidth: Integer; const SpriteHeight: Integer; const SavePath: String; const Action: Cardinal = 0; const OverSample: Integer = 8; const CallCounter: Cardinal = 0; const IsFirstCall: Boolean = True);
    procedure GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
    function FetchSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True): TCastleImage;
    property CameraRotation: Integer read fCameraRotation write setCameraRotation;
    property CameraRotationSteps: Integer read fCameraRotationSteps write setCameraRotationSteps;
    property ModelRotation: Integer read fModelRotation write setModelRotation;
    property ModelRotationSteps: Integer read fModelRotationSteps write setModelRotationSteps;
    property CameraElevation: Single read fCameraElevation write setCameraElevation;
    procedure ShowInfo;
    function CreateView(const SourceScene: TCastleScene): TCastleViewport;
    function CreateView(const SourceScene: TCastleScene; const VWidth: Cardinal; const VHeight: Cardinal): TCastleViewport;
    procedure AddDebugBox(const AScene: TCastleScene);
    procedure SynchTrackbar;
    procedure ToggleBorders;
    procedure ProcessAllModels(NodeIdx: Integer; SavePath: String);
  end;

var
  AppTime: Int64;
  CastleApp: TCastleApp;

const
  ValidModelMimeTypes: TStringArray = (
    'application/x-wavefront-obj',
    'model/gltf-binary',
    'model/gltf+json',
    'model/vrml',
    'model/x3d+vrml',
    'model/x3d+xml',
    'model/x3d+binary'
    );

// ValidHeadings: TStringArray = ('s');
// ValidHeadings: TStringArray = ('s', 'e', 'n', 'w');
 ValidHeadings: TStringArray = ('s', 'se', 'e', 'ne', 'n', 'nw', 'w', 'sw');

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization, ShowCameraSettings;
{$endif}

{ TSpritelyModel }

constructor TSpritelyModel.Create(AOwner: TComponent);
begin
  inherited;
end;

constructor TSpritelyModel.Create(AOwner: TComponent; const AFile: String; AScene: TCastleScene; ASize: TVector3);
begin
  Create(AOwner);

  fFileName := AFile;
  fModelName := StripExtension(ExtractURIName(AFile));
  fScene := AScene;
  fRealSize := ASize;
end;

{ TCastleApp }

constructor TCastleApp.Create(AOwner: TComponent);
{$ifdef devstream}
var
  TestStream: TStream;
{$endif}
begin
  inherited;
//  LogTextureCache := True;
  WavefrontPhongMaterials := False;
  ViewScale := 1;
  ViewWidth := 256;
  ViewHeight := 256;
  UseOversample := False;
  SettingUp := True;
  Abort := False;
  AbortAction := False;
  AbortModel := False;
  SceneTilt := 0;
  AniRec := Default(TAnimationController);
  TextureAtlasX := 2048;
  TextureAtlasY := 2048;
  Animating := False;
  SubActionList := nil;
  StretchMultiplier := 1;
  CameraRotationSteps := 8;
  CameraRotation := 0;
  ModelRotationSteps := CameraRotationSteps;
  ModelRotation := 0;
  CameraElevation := 0; // .81625; // sqrt(2); //

  {$ifndef cgeapp}
  DoingModel := EmptyStr;
  DoingNode := -1;

  with CastleForm do
    begin
      ViewID := 0;
      CurrentProjection := PopupMenu1.Items[ViewID].Caption;
      PopupMenu1.Items[ViewID].Checked := True;
      EditWidth.Text := IntToStr(ViewWidth);
      EditHeight.Text := IntToStr(ViewHeight);
    end;
  {$endif}
  OriginalSize := TVector3.Zero;
  FullSize := True;
  SettingUp := False;

//  LoadSubActions('tests/dbat.txt');
//  LoadSubActions('tests/birds.txt');
  LoadSubActions('tests/crazy-rabbits-animations-list.txt');
//  LoadSubActions('tests/crazy-rabbits-animations-eat.txt');

  {$ifdef devmode}
  tz[0] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/freshwater_goby_no.1.zip');
  tz[1] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/honda_cb500f_scrambler_custom.zip');
  tz[2] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/modular_dungeon.zip');
  tz[3] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/sandbags_-_defense_line.zip');
  tz[4] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/soul_of_the_forest_many_animations.zip');

  {$ifdef devstream}
  TestStream := Download('castle-data:/Models/Paid/bear.zip', [soForceMemoryStream]);
  tz[5] := TZipFileSystem.Create(Self, TestStream, 'bear.zip');
  FreeAndNil(TestStream);
  {$else}
  tz[5] := TZipFileSystem.Create(Self, 'castle-data:/Models/Paid/bear.zip');
  {$endif}

  tz[6] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/viking_room.zip');
  tz[7] := TZipFileSystem.Create(Self, 'castle-data:/Models/Sketchfab/dungeon_modular_set.zip');
  tz[8] := TZipFileSystem.Create(Self, 'castle-data:/Models/Paid/Retro-Interiors-EnviroKit-gltf.zip');
  tz[9] := TZipFileSystem.Create(Self, 'castle-data:/Models/Paid/Retro-Interiors-EnviroKit.zip');
  {$endif}
end;

destructor TCastleApp.Destroy;
begin
  inherited;
end;

procedure TCastleApp.FrameSync(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TCastleApp.BootStrap(Sender: TObject);
begin
  WriteLnLog('BootStrap = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  LoadUI;
  {$ifdef devmode}
//  LoadScene('castle-data:/logo.png');
//  LoadScene(ZipFileSystem.Protocol + '/gltf/arch_interior_floorBig_stone_varA.gltf');
//  LoadScene('castle-data:/Models/paid/Retro-Interiors-EnviroKit/gltf/arch_interior_floorBig_stone_varA.gltf');
// LoadScene(ZipFileSystem.Protocol + '/glb/arch_interior_floorBig_stone_varA.glb');
// LoadScene(ZipFileSystem.Protocol + '/scene.gltf');
  LoadScene(tz[5].Protocol + '/scene.gltf');
//  LoadScene(tz[8].Protocol + '/gltf/arch_interior_floorBig_stone_varA.gltf');
//  LoadScene(tz[9].Protocol + '/glb/arch_interior_floorBig_stone_varA.glb');
  {$else}
//  LoadScene('castle-data:/tests/oblique.glb');
//  LoadScene('castle-data:/Models/paid/potato4.glb');
//  LoadScene('castle-data:/KidSpin.glb');
  {$endif}

end;

procedure TCastleApp.setCameraRotation(const AValue: Integer);
begin
  if not(fCameraRotation = AValue) then
    begin
      fCameraRotation := AValue;
      fCameraRotation := fCameraRotation mod fCameraRotationSteps;
      if Assigned(Viewport) then
        begin
        Viewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
        {$ifndef cgeapp}
        CameraForm.ShowStats(Viewport);
        {$endif}
        end;
      ShowInfo;

    end;
end;

procedure TCastleApp.setCameraRotationSteps(const AValue: Integer);
begin
  if not(fCameraRotationSteps = AValue) then
    begin
      fCameraRotationSteps := AValue;
      if Assigned(Viewport) then
        begin
        Viewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
        {$ifndef cgeapp}
        CameraForm.ShowStats(Viewport);
        {$endif}
        end;
      ShowInfo;
    end;
end;

procedure TCastleApp.setModelRotation(const AValue: Integer);
begin
  if not(fModelRotation = AValue) then
    begin
      fModelRotation := AValue;
      fModelRotation := fModelRotation mod fModelRotationSteps;
      if Assigned(Viewport) then
        begin
// sbdbg        Scene.Rotation (2, CameraElevation, 2 * pi * (ModelRotation / ModelRotationSteps));
          Scene.Rotation := Vector4(0, 1, 0, 2 * pi * (ModelRotation / ModelRotationSteps));
        {$ifndef cgeapp}
        CameraForm.ShowStats(Viewport);
        {$endif}
        end;
      ShowInfo;

    end;
end;

procedure TCastleApp.setModelRotationSteps(const AValue: Integer);
begin
  if not(fModelRotationSteps = AValue) then
    begin
      fModelRotationSteps := AValue;
      if Assigned(Viewport) then
        begin
        Scene.Rotation := Vector4(0, 1, 0, 2 * pi * (ModelRotation / ModelRotationSteps));
        {$ifndef cgeapp}
        CameraForm.ShowStats(Viewport);
        {$endif}
        end;
      ShowInfo;
    end;
end;

procedure TCastleApp.setCameraElevation(const AValue: Single);
begin
  if not(fCameraElevation = AValue) then
    begin
      fCameraElevation := AValue;
      if Assigned(Viewport) then
        begin
          Viewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
          {$ifndef cgeapp}
          CameraForm.ShowStats(Viewport);
          {$endif}
        end;
      ShowInfo;
    end;
end;

procedure TCastleApp.ShowInfo;
var
  Info: String;
begin
  if Assigned(Scene) then
    begin
      Info := 'ViewScale : ' + FloatToStr(ViewScale) + LineEnding +
        'BB : ' + Scene.BoundingBox.Data[0].ToString + ' - ' + Scene.BoundingBox.Data[1].ToString + LineEnding +
        'Scale : ' + Scene.Scale.ToString + LineEnding +
        'Center : ' + Scene.Center.ToString + LineEnding +
        'Original : ' + OriginalSize.ToString + LineEnding +
        'Elevation : ' + FLoatToStr(CameraElevation) + LineEnding +
        'Rotation : ' + FloatToStr((CameraRotation / CameraRotationSteps) * 360) + LineEnding +
        'Translation : ' + Scene.Translation.ToString;

//        'Extents : ' + FloatToStr(Extents.Min.X) + ' x ' + FloatToStr(Extents.Min.Y) + ' - '  + FloatToStr(Extents.Max.X) + ' x '  + FloatToStr(Extents.Max.Y) + ' : '  + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y) + LineEnding +
    end;
end;

procedure TCastleApp.LoadUI;
begin
  WriteLnLog('LoadUI UIState = ' + FloatToStr(Width) + ' x ' + FloatToStr(Height));
  WriteLnLog('LoadUI Effective = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  WriteLnLog('LoadUI EffectiveRect = ' + EffectiveRect.ToString);

  ViewUI := TCastleRectangleControl.Create(Self);
  ViewUI.SetRect(EffectiveRect);
  ViewUI.Color := Vector4(0.0, 0.0, 0.0, 1.0);
  InsertFront(ViewUI);

  ViewPane := TCastleRectangleControl.Create(ViewUI);
  ViewPane.FitRect(ViewWidth, ViewHeight, EffectiveRect);
  ViewPane.Color := Vector4(1, 1, 1, 1.0);
//  ViewPane.Color := Vector4(0.05, 0.05, 0.05, 1.0);
  ViewUI.InsertFront(ViewPane);

  CreateLabel(InfoNote, 0);
end;

procedure TCastleApp.SynchTrackbar;
begin
  SettingUp := True;
  CastleForm.TrackBar1.Position := CastleForm.TrackBar1.Max div 2;
  SettingUp := False;
end;

procedure TCastleApp.AddDebugBox(const AScene: TCastleScene);
begin
  if Assigned(Debug) then
    FreeAndNil(Debug);

  Debug := TDebugTransformBox.Create(Self);
  Debug.Parent := AScene;
  Debug.BoxColor := Vector4(1,0,0, 1);
  Debug.Exists := False;
  {$ifndef cgeapp}
  if CastleForm.MenuDebug.Checked then
    Debug.Exists := True;
  {$endif}
end;

function TCastleApp.LoadScene(const AFile: String): TCastleScene;
var
  newScene: TCastleScene;
  i: Integer;
  mime: String;
  mimeok: Boolean;
begin
  AppTime := CastleGetTickCount64;
  Result := nil;

  mime := URIMimeType(AFile);
  mimeok := False;

  for i := 0 to Length(ValidModelMimeTypes) - 1 do
    begin
      if mime = ValidModelMimeTypes[i] then
        begin
          mimeok := True;
          Break;
        end;
    end;

  if not(mimeok) then
    begin
      Exit(Result);
    end;

  try
    newScene := TCastleScene.Create(Self);
    newScene.Spatial := [ssDynamicCollisions, ssRendering];
    newScene.RenderOptions.MinificationFilter := minNearest;
    newScene.RenderOptions.MagnificationFilter := magNearest;
    newScene.Setup2D;
    newScene.Load(AFile);
    OriginalSize := newScene.Normalize;
    newScene.HeadlightOn := True;

    Viewport := CreateView(newScene);
    newScene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);

    AddDebugBox(newScene);

    {$ifndef cgeapp}
    SynchTrackbar;
    CastleForm.CurrentFile := AFile;
    CastleForm.CurrentModel := StripExtension(ExtractURIName(AFile));
    {$endif}

    Result := newScene;
    Scene := newScene;
    if(Scene.AnimationsList.Count > 0) then
      begin
        CastleForm.EditFrames.Enabled := True;
      end
    else
      begin
        CastleForm.EditFrames.Enabled := False;
      end;
    Reflow;
  except
    on E : Exception do
      begin
        WriteLnLog('Error : ' + LineEnding + E.ClassName + LineEnding + E.Message);
        FreeAndNil(Scene);
      end;
  end;

end;

procedure TCastleApp.Start;
begin
  inherited;
  WriteLnLog('Starting = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  WaitForRenderAndCall(@BootStrap);
end;

procedure TCastleApp.Stop;
begin
  WriteLnLog('Stopping');
  inherited;
end;

procedure TCastleApp.Resize;
begin
  inherited;
  if Assigned(ViewUI) then
    begin
      ViewUI.SetRect(EffectiveRect);
      if Assigned(ViewPane) then
        begin
          ViewPane.FitRect(ViewPane.RenderRect, ViewPane.ParentRect);
          if Assigned(Viewport) then
            begin
              Reflow(True);
            end;
        end;
    end;

end;

procedure TCastleApp.Reflow(const CalledFromResize: Boolean = False);
begin
  if Assigned(Viewport) and Assigned(Scene) then
    begin
      if not(CalledFromResize) then
        begin
          ViewUI.SetRect(EffectiveRect);
          ViewPane.FitParent;
        end;
      ViewGrid.FitParent;
      ViewBack.FitParent;
      Viewport.FitParent;
{
      ViewGrid.FitRect(ViewGrid.RenderRect, ViewGrid.ParentRect);
      ViewBack.FitRect(ViewBack.RenderRect, ViewBack.ParentRect);
      Viewport.FitRect(Viewport.RenderRect, Viewport.ParentRect);
}
      if not(Viewport.Width = LastWidth) then
        begin
          Viewport.Camera.Orthographic.Scale := OriginalScale * (OriginalWidth / Viewport.Width);;
          LastWidth := Viewport.Width;
        end;
      {$ifndef cgeapp}
      CastleForm.UpdateCaption;
      {$endif}

    end
  else WriteLnLog('Viewport or Scene not set' + LineEnding);

end;

procedure TCastleApp.ProcessAllModels(NodeIdx: Integer; SavePath: String);
var
  Node: TTreeNode;
  Model: TSpritelyModel;
begin
  WriteLnLog('ProcessAllModels(' + IntToStr(NodeIdx) + ' , ' + SavePath + ')');
  if DoingNode < CastleForm.Treeview1.Items.Count then
    begin
      Node := CastleForm.Treeview1.Items[NodeIdx];
      if Node = nil then
        begin
          DoingNode := -1;
          DoingModel := EmptyStr;
          AniRec := Default(TAnimationController);
          ShowMessage('Finished');
          Exit;
        end;
      if NodeIdx = 0 then
        SavePath := 'tests/check';
      if (TObject(Node.Data).ClassName = 'TSpritelyModel') then
        begin
          Model := TSpritelyModel(Node.Data);
          Scene := Model.Scene;
          DoingModel := Model.ModelName;
          DoingNode := NodeIdx;
          Viewport := CreateView(Scene);
          Reflow;
          WriteLnLog('Doing Model' + DoingModel);
          if UseOversample then
            GrabAtlas(Trunc(ViewWidth), Trunc(ViewHeight), SavePath, 0, 8, 0, True)
          else
            GrabAtlas(Trunc(ViewWidth), Trunc(ViewHeight), SavePath, 0, 1, 0, True);
        end;
    end
  else
    begin
      DoingNode := -1;
      DoingModel := EmptyStr;
      AniRec := Default(TAnimationController);
      ShowMessage('Finished');
    end;
end;

procedure TCastleApp.BeforeRender;
begin
  inherited;
end;

procedure TCastleApp.Render;
begin
  inherited;
  if not(AppTime = 0) then
    begin
      if Assigned(Scene) and Scene.IsVisible then
        begin
          AppTime := CastleGetTickCount64 - AppTime;
          WriteLnLog('Load time : ' + IntToStr(AppTime));
          AppTime := 0;
        end;
    end;
  if Animating then
    begin
       MakeAtlas;
    end;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

procedure TCastleApp.ShowAppMessage(const AMsg: String);
begin
  WriteLnLog(AMsg);
end;

procedure TCastleApp.MakeAtlas;
var
  FinalImage: TCastleImage;
  SName: String;
  FrameTime: TFloatTime;
  SavePath: String;
begin
  if(AniRec.Frame < AniRec.FrameCount) then
    begin
      FrameTime := AniRec.FrameTime * AniRec.Frame;
      WriteLnLog('Frame : ' + FloatToStr(FrameTime));
      Scene.ForceAnimationPose(AniRec.AnimationName, FrameTime, False, True);
      if SubActionList[AniRec.SubAction].Active = 1 then
        begin
          FinalImage := FetchSprite(AniRec.SpriteWidth, AniRec.SpriteHeight, AniRec.OverSample, AniRec.UseTransparency);
          if not(FinalImage = nil) then
            begin
              if DoingModel = EmptyStr then
                SName := FileNameAutoInc(AniRec.SavePath + '/' + SubActionList[AniRec.SubAction].Name + '/' + AniRec.SaveHeading + '/' + SubActionList[AniRec.SubAction].Name + '_%4.4d.png')
              else
                SName := FileNameAutoInc(AniRec.SavePath + '/' + DoingModel + '/' + SubActionList[AniRec.SubAction].Name + '/' + AniRec.SaveHeading + '/' + SubActionList[AniRec.SubAction].Name + '_%4.4d.png');
              SaveImage(FinalImage, SName);
              WriteLnLog('Saving to ' + SName);
              FreeAndNil(FinalImage);

              InfoNote.Caption := 'Doing Model : ' + DoingModel + ' (' + IntToStr(DoingNode + 1) + '/' + IntToStr(CastleForm.Treeview1.Items.Count) + ')' + LineEnding +
                  'Doing Action : ' + SubActionList[AniRec.SubAction].Name + ' (' + IntToStr(AniRec.SubAction + 1)  + '/' + IntToStr(Length(SubActionList)) + ')' + LineEnding +
                  'Heading : ' + ValidHeadings[AniRec.CallCounter].ToUpper + ' (' + IntToStr(AniRec.CallCounter + 1) + '/' + IntToStr(Length(ValidHeadings)) + ')' + LineEnding +
                  'Frame : ' + IntToStr(AniRec.Frame - SubActionList[AniRec.SubAction].Start + 1) + '/' + IntToStr(SubActionList[AniRec.SubAction].Length) + LineEnding +
                  'Time = ' + FormatFloat('#.00', FrameTime) + ' / ' +
                  FormatFloat('#.00', Scene.AnimationDuration(AniRec.AnimationName));
            end;
        end
      else
        InfoNote.Caption := 'Skipping Model : ' + DoingModel + ' (' + IntToStr(DoingNode + 1) + '/' + IntToStr(CastleForm.Treeview1.Items.Count) + ')' + LineEnding +
            'Skipping Action : ' + SubActionList[AniRec.SubAction].Name + ' (' + IntToStr(AniRec.SubAction + 1)  + '/' + IntToStr(Length(SubActionList)) + ')' + LineEnding +
            'Heading : ' + ValidHeadings[AniRec.CallCounter].ToUpper + ' (' + IntToStr(AniRec.CallCounter + 1) + '/' + IntToStr(Length(ValidHeadings)) + ')' + LineEnding +
            'Frame : ' + IntToStr(AniRec.Frame - SubActionList[AniRec.SubAction].Start + 1) + '/' + IntToStr(SubActionList[AniRec.SubAction].Length) + LineEnding +
            'Time = ' + FormatFloat('#.00', FrameTime) + ' / ' +
            FormatFloat('#.00', Scene.AnimationDuration(AniRec.AnimationName));

      if Abort then
      // if AniRec.Frame > 3 then
        begin
          Animating := False;
          DoingNode := -1;
          DoingModel := EmptyStr;
          AniRec := Default(TAnimationController);
          ShowMessage('Aborted');
          Abort := False;
        end;

      AniRec.Frame := AniRec.Frame + 1;
    end
  else
    begin
      Animating := False;
      FreeAndNil(AniRec.AtlasImage);

      AniRec.CallCounter := AniRec.CallCounter + 1;
      if(AniRec.CallCounter < Length(ValidHeadings)) then
        begin
          if UseOversample then
            GrabAtlas(AniRec.SpriteWidth, AniRec.SpriteHeight, AniRec.SavePath, AniRec.Action, AniRec.OverSample, AniRec.CallCounter, False)
          else
            GrabAtlas(AniRec.SpriteWidth, AniRec.SpriteHeight, AniRec.SavePath, AniRec.Action, AniRec.OverSample, AniRec.CallCounter, False);
        end
      else if(AniRec.SubAction < Length(SubActionList) - 1) then
        begin // Process next SubAction
          if AbortAction then
            begin
              Animating := False;
              DoingNode := -1;
              DoingModel := EmptyStr;
              AniRec := Default(TAnimationController);
              ShowMessage('Aborted');
              AbortAction := False;
            end
          else
            begin
              AniRec.CallCounter := 0;
              AniRec.SubAction := AniRec.SubAction + 1;
              if UseOversample then
                GrabAtlas(AniRec.SpriteWidth, AniRec.SpriteHeight, AniRec.SavePath, AniRec.Action, AniRec.OverSample, AniRec.CallCounter, False)
              else
                GrabAtlas(AniRec.SpriteWidth, AniRec.SpriteHeight, AniRec.SavePath, AniRec.Action, AniRec.OverSample, AniRec.CallCounter, False);
            end;
        end
      else if not(DoingNode = -1) then
        begin
          if AbortModel then
            begin
              Animating := False;
              DoingNode := -1;
              DoingModel := EmptyStr;
              AniRec := Default(TAnimationController);
              ShowMessage('Aborted');
              AbortModel := False;
            end
          else
            begin
              Animating := False;
              SavePath := AniRec.SavePath;
              AniRec := Default(TAnimationController);
              Inc(DoingNode);
              ProcessAllModels(DoingNode, SavePath);
            end;
        end
      else
        begin
          AniRec := Default(TAnimationController);
          ShowMessage('Finished');
        end;

    end;
end;

procedure TCastleApp.GrabAtlas(const SpriteWidth: Integer; const SpriteHeight: Integer; const SavePath: String; const Action: Cardinal = 0; const OverSample: Integer = 8; const CallCounter: Cardinal = 0; const IsFirstCall: Boolean = True);
begin
  if not (SubActionList = nil) then
    begin
    if(not(Animating)) then
      begin
        if(Scene.AnimationsList.Count > 0) then
          begin
            WriteLnLog('Scene.AnimationsList.Count = ' + IntToStr(Scene.AnimationsList.Count));

            if (AniRec.CallCounter = 0) and IsFirstCall then
              begin
                AniRec := Default(TAnimationController);
                AniRec.AnimationName := Scene.AnimationsList[Action];
                AniRec.SubAction := 0;
              end;

            while (SubActionList[AniRec.SubAction].Active = 0) and
                  (AniRec.SubAction < Length(SubActionList) - 1) do
              begin
                AniRec.SubAction := AniRec.SubAction + 1;
              end;

            if Length(ValidHeadings) = 4 then
               CameraRotation := CallCounter * 2
            else
               CameraRotation := CallCounter;


            Viewport := CreateView(Scene);
            Reflow;
{
            if SubActionList[AniRec.SubAction].Length <= 60 then
              SubActionList[AniRec.SubAction].Active := 1;
}
            AniRec.SavePath := SavePath;
            AniRec.Action := Action;
            AniRec.SaveHeading := ValidHeadings[AniRec.CallCounter];
            if SubActionList[AniRec.SubAction].Active = 1 then
              begin
                if DoingModel = EmptyStr then
                  CheckForceDirectories(AniRec.SavePath + '/' + SubActionList[AniRec.SubAction].Name + '/' + AniRec.SaveHeading)
                else
                  CheckForceDirectories(AniRec.SavePath + '/' + DoingModel + '/' + SubActionList[AniRec.SubAction].Name + '/' + AniRec.SaveHeading);
              end;

            AniRec.Frame := SubActionList[AniRec.SubAction].Start;
            AniRec.FrameCount := AniRec.Frame + SubActionList[AniRec.SubAction].Length;
            AniRec.FrameTime :=  Scene.AnimationDuration(AniRec.AnimationName) /
                                (Scene.AnimationDuration(AniRec.AnimationName) * 30);
{
            AniRec.FrameCount := AniRec.Frame + 30;
            AniRec.FrameTime := (Scene.AnimationDuration(AniRec.AnimationName) /
                                (Scene.AnimationDuration(AniRec.AnimationName) * 30)) *
                                (SubActionList[AniRec.SubAction].Length / 30);
}

            Animating := True;

            Scene.ForceAnimationPose(AniRec.AnimationName, 0, False, True);

            AniRec.SpriteWidth := SpriteWidth;
            AniRec.SpriteHeight := SpriteHeight;
            AniRec.OverSample := OverSample;
            AniRec.UseTransparency := True;
  //          AniRec.AtlasImage := TRGBAlphaImage.Create(TextureAtlasX, TextureAtlasY);
            AniRec.FrameX := 0;
            AniRec.FrameY := 0;
          end;
      end;
    end;
end;

procedure TCastleApp.LoadSubActions(AniFile: String);
var
  I: Integer;
begin
  SubActionList := AniTxtToSubAction(AniFile);

  if not (SubActionList = nil) then
    for I := 0 to Length(SubActionList) - 1 do
      WriteLnLog('AniTxt #' + IntToStr(I) +
        ' Start: ' + IntToStr(SubActionList[I].Start) +
        ', Length: ' + IntToStr(SubActionList[I].Length) +
        ', Name: ' + SubActionList[I].Name);

end;

procedure TCastleApp.GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
var
  FinalImage: TCastleImage;
  SName: String;
begin
  FinalImage := FetchSprite(SpriteWidth, SpriteHeight, OverSample, UseTransparency);
  if not(FinalImage = nil) then
    begin
      SName := FileNameAutoInc('grab_%4.4d.png');
      SaveImage(FinalImage, SName);
      FreeAndNil(FinalImage);
      InfoNote.Caption := 'Saved Sprite : ' + SName;
    end;
end;

function TCastleApp.FetchSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True): TCastleImage;
var
  Sprite: TRGBAlphaImage;
  FinalImage: TCastleImage;
  UseMainViewport: Boolean;
//  SName: String;
begin
  Result := nil;
  UseMainViewport := False;

  if not (Scene = nil) then
    begin
      FinalImage := CreateSpriteImage(Scene, SpriteWidth * OverSample, SpriteHeight * OverSample, UseTransparency, UseMainViewport);

      if not(FinalImage = nil) then
        begin
//          SName := FileNameAutoInc('full_%4.4d.png');
//          SaveImage(FinalImage, SName);
          if FinalImage.Height > FinalImage.Width then
            begin
              if (OverSample <> 1) then
                FinalImage.Resize(Trunc((SpriteWidth * OverSample) / (FinalImage.Height / FinalImage.Width)), (SpriteHeight * OverSample), riMitchel)
              else
                FinalImage.Resize(Trunc(SpriteWidth / (FinalImage.Height / FinalImage.Width)), SpriteHeight, riNearest)
            end
          else
            begin
              if (OverSample <> 1) then
                FinalImage.Resize((SpriteWidth * OverSample), Trunc((SpriteHeight * OverSample) / (FinalImage.Width / FinalImage.Height)), riMitchel)
              else
                FinalImage.Resize(SpriteWidth, Trunc(SpriteHeight / (FinalImage.Width / FinalImage.Height)), riNearest);
            end;
          Sprite := TRGBAlphaImage.Create(SpriteWidth * OverSample, SpriteHeight * OverSample);
          Sprite.ClearAlpha(0);
          if FinalImage.Height > FinalImage.Width then
            Sprite.DrawFrom(FinalImage, (Sprite.Width - FinalImage.Width) div 2, 0, 0, 0, FinalImage.Width, FinalImage.Height, dmOverwrite)
          else
            Sprite.DrawFrom(FinalImage, 0, (Sprite.Height - FinalImage.Height) div 2, 0, 0, FinalImage.Width, FinalImage.Height, dmOverwrite);

          if (OverSample > 1) then
            begin
              Sprite.Resize(Trunc(Sprite.Width / OverSample), Trunc(Sprite.Height / OverSample), riMitchel); // riNearest, riBilinear, riMitchel, riLanczos);
            end;
          FreeAndNil(FinalImage);
          Result := Sprite;
        end;
    end;
end;

function TCastleApp.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False; const useMainViewport: Boolean = True): TCastleImage;
var
  SourceViewport: TCastleViewport;
  CloneScene: TCastleScene;
  ViewportRect: TRectangle;
  Image: TDrawableImage;
  BackImage: TRGBAlphaImage;
  Extents: TExtents;
  HeightAdjust: Single;
begin
  SourceViewport := nil;

  if not(SourceScene = nil) and (TextureWidth > 0) and (TextureHeight > 0) then
    begin
      try
        try
          SourceViewport := TCastleViewport.Create(nil);

          if isSpriteTransparent then
            begin
              SourceViewport.Transparent := True;
              SourceViewport.BackgroundColor := Vector4(1,1,1,0);
            end
          else
            begin
              SourceViewport.Transparent := False;
              SourceViewport.BackgroundColor := Vector4(0,0,0,1);
            end;

          SourceViewport.AutoCamera := False;
          SourceViewport.Setup2D;
          SourceViewport.Camera.ProjectionType := ptOrthographic;
          SourceViewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);

          SourceViewport.Width := TextureWidth;
          SourceViewport.Height := TextureHeight;

          if not (StretchMultiplier = 1) then
            begin
              SourceViewport.Camera.Orthographic.Stretch := True;
              SourceViewport.Camera.Orthographic.Width := SourceViewport.EffectiveWidth / StretchMultiplier;
              SourceViewport.Camera.Orthographic.Height := SourceViewport.EffectiveHeight;
            end;

          if not useMainViewport then
            begin
              CloneScene := SourceScene.Clone(nil);
              CloneScene.Normalize;
            end;

          SourceViewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
          if useMainViewport then
            Extents := SourceViewport.CalcAngles(SourceScene)
          else
            Extents := SourceViewport.CalcAngles(CloneScene);

          SourceViewport.Camera.Orthographic.Scale := Extents.Size.X / TextureWidth;

{
WriteLnLog(CloneScene.BoundingBox.MaxSize.ToString + ' / ' + CloneScene.BoundingBox.Size.ToString);
CloneScene.Scale := Vector3( CloneScene.BoundingBox.MaxSize / 4,
                             CloneScene.BoundingBox.MaxSize / 4,
                             CloneScene.BoundingBox.MaxSize / 4);
WriteLnLog('Extents : ' + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y));
WriteLnLog(CloneScene.BoundingBox.MaxSize.ToString + ' / ' + CloneScene.BoundingBox.Size.ToString);
WriteLnLog('==================================');
}
          if useMainViewport then { Possibly use clone if desired }
            SourceViewport.Items := ViewPort.Items
          else
            begin
              SourceViewport.Items.UseHeadlight := hlMainScene;
              SourceViewport.Items.Add(CloneScene);
              SourceViewport.Items.MainScene := CloneScene;
            end;

          HeightAdjust := Extents.Size.Y / Extents.Size.X;
          SourceViewport.Height := Trunc(TextureHeight * HeightAdjust);

          {$ifndef cgeapp}
          CameraForm.ShowStats(SourceViewport);
          {$endif}

          BackImage := TRGBAlphaImage.Create(Trunc(TextureWidth), Trunc(TextureHeight * HeightAdjust));
          BackImage.ClearAlpha(0);
          Image := TDrawableImage.Create(BackImage, true, true);
          Image.RenderToImageBegin;

          ViewportRect := Rectangle(0, 0, Trunc(TextureWidth), Trunc(TextureHeight * HeightAdjust));
          {$ifndef cgeapp}CastleForm.{$endif}Window.Container.RenderControl(SourceViewport,ViewportRect);

          Image.RenderToImageEnd;

          if not False { Application.OpenGLES } then
          begin
            try
              Result := Image.GetContents(TRGBAlphaImage);
            except
              on E : Exception do
                begin
                  ShowAppMessage(E.ClassName + LineEnding + E.Message);
                end;
            end;
          end;

        except
          on E : Exception do
            begin
              ShowAppMessage(E.ClassName + LineEnding + E.Message);
            end;
        end;
      finally
        FreeAndNil(SourceViewport);
        FreeAndNil(Image);
        if not useMainViewport then
          CloneScene.Free;
      end;
    end;
end;

function TCastleApp.CreateView(const SourceScene: TCastleScene): TCastleViewport;
begin
  Result := CreateView(SourceScene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
end;

function TCastleApp.CreateView(const SourceScene: TCastleScene; const VWidth: Cardinal; const VHeight: Cardinal): TCastleViewport;
var
  NewVP: TCastleViewport;
  Extents: TExtents;
  HeightAdjust: Single;
begin
  if Assigned(Viewport) then
    begin
      if Assigned(ViewBack) then
        ViewBack.RemoveControl(Viewport);
      FreeAndNil(Viewport);
    end;
  if Assigned(ViewBack) then
    begin
      if Assigned(ViewGrid) then
        ViewGrid.RemoveControl(ViewBack);
      FreeAndNil(ViewBack);
    end;

  if Assigned(ViewGrid) then
    begin
      if Assigned(ViewPane) then
        ViewPane.RemoveControl(ViewGrid);
      FreeAndNil(ViewGrid);
    end;

  NewVP := TCastleViewport.Create(Self);

  NewVP.Setup2D;
  NewVP.AutoCamera := False;
//  NewVP.NavigationType := ntNone;
  NewVP.Camera.ProjectionType := ptOrthographic;
  NewVP.Camera.Orthographic.Origin := Vector2(0.5, 0.5);

  NewVP.Width := VWidth;
  NewVP.Height := VHeight;
  NewVP.Transparent := True;

  NewVP.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
  Extents := NewVP.CalcAngles(SourceScene);
  HeightAdjust := Extents.Size.Y / Extents.Size.X;

  OriginalScale := Extents.Size.Y / (VWidth * HeightAdjust);
  NewVP.Camera.Orthographic.Scale := OriginalScale;
//  WriteLnLog('Extents : ' + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y));
  NewVP.Height := Trunc(VWidth * HeightAdjust);

  if not (StretchMultiplier = 1) then
    begin
      NewVP.Camera.Orthographic.Stretch := True;
      NewVP.Camera.Orthographic.Width := NewVP.Width / StretchMultiplier;
      NewVP.Camera.Orthographic.Height := NewVP.Height;
    end;

  OriginalWidth := NewVP.Width;
  LastWidth := OriginalWidth;

//  WriteLnLog('OriginalWidth : ' + FloatToStr(OriginalWidth));

  NewVP.Items.UseHeadlight := hlMainScene;
  NewVP.Items.Add(SourceScene);
  NewVP.Items.MainScene := SourceScene;

  {$ifndef cgeapp}
  CameraForm.ShowStats(NewVP);
  {$endif}

  ViewGrid := TCastleImageControl.Create(Self);
  ViewGrid.Stretch := True;
  ViewPane.InsertFront(ViewGrid);

  ViewGrid.FitParent;

  ViewBack := TCastleRectangleControl.Create(Self);
  ViewBack.Color := Vector4(1, 0.1, 0.1, 0.0);
  ViewGrid.InsertFront(ViewBack);

  ViewBack.InsertFront(NewVP);
  ViewBack.FitParent;

  Result := NewVP;
end;

{
Border Nesting Order

Form
  Window
    ViewUI             Cyan
      ViewPane         Red
        ViewGrid       Green
          ViewBack     Blue
            Viewport   Yellow

}
procedure TCastleApp.ToggleBorders;
var
  newWidth: Cardinal;
begin
  if ViewUI.Border.AllSides = 0 then
    newWidth := 1
  else
    newWidth := 0;

  ViewUI.Border.AllSides := newWidth;
  ViewUI.BorderColor := Vector4(0, 1, 1, 1);
  ViewPane.Border.AllSides := newWidth;
  ViewPane.BorderColor := Vector4(1, 0, 0, 1);
  ViewGrid.Border.AllSides := newWidth;
  ViewGrid.BorderColor := Vector4(0, 1, 0, 1);
  ViewBack.Border.AllSides := newWidth;
  ViewBack.BorderColor := Vector4(0, 0, 1, 1);
  Viewport.Border.AllSides := newWidth;
  Viewport.BorderColor := Vector4(1, 1, 0, 1);

  WriteLnLog( LineEnding );

  Reflow;
end;

end.

