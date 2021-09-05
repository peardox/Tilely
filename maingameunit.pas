unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DLoadInternalOBJ, X3DNodes, X3DFields, X3DTIme, CastleRectangles,
  CastleImages, CastleGLImages, CastleDebugTransform,
  CastleTextureImages, CastleCompositeImage, CastleBoxes,
  CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleFilesUtils, CastleKeysMouse, CastleNotifications,
  RGBAlphaImageHelp, MiscHelpers, CastleURIUtils,
  ZipUrls, CastleDownload;

type
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
    fCameraElevation: Single;
    ViewScale: Single;
    OriginalScale: Single;
    OriginalWidth: Single;
    StretchMultiplier: Single;
    procedure setCameraRotation(const AValue: Integer);
    procedure setCameraRotationSteps(const AValue: Integer);
    procedure setCameraElevation(const AValue: Single);
  public
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Debug: TDebugTransformBox;
    InfoLabel: TCastleLabel;
    InfoNote: TCastleNotifications;
    SettingUp: Boolean;
    LastWidth: Single;
    ViewWidth: Integer;
    ViewHeight: Integer;
    UseOversample: Boolean;
    SceneTilt: Integer;
    OriginalSize: TVector3;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure BootStrap(Sender: TObject);
    procedure Reflow(const CalledFromResize: Boolean = False);
    procedure LoadUI;
    function LoadScene(const AFile: String): TCastleScene;
    function CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False; const useMainViewport: Boolean = True): TCastleImage;
    procedure ShowAppMessage(const AMsg: String);
    procedure GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
    property CameraRotation: Integer read fCameraRotation write setCameraRotation;
    property CameraRotationSteps: Integer read fCameraRotationSteps write setCameraRotationSteps;
    property CameraElevation: Single read fCameraElevation write setCameraElevation;
    procedure ShowInfo;
    function CreateView(const SourceScene: TCastleScene): TCastleViewport;
    function CreateView(const SourceScene: TCastleScene; const VWidth: Cardinal; const VHeight: Cardinal): TCastleViewport;
    procedure AddDebugBox(const AScene: TCastleScene);
    procedure SynchTrackbar;
    procedure ToggleBorders;
  end;

var
  AppTime: Int64;
  CastleApp: TCastleApp;
  PackedDataReader: TPackedDataReader;

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
begin
  inherited;
//  LogTextureCache := True;
  WavefrontPhongMaterials := False;
  ViewScale := 1;
  ViewWidth := 256;
  ViewHeight := 256;
  UseOversample := True;
  SettingUp := True;
  SceneTilt := 0;
  StretchMultiplier := 1;
  CameraRotationSteps := 8;
  CameraRotation := 1;
  CameraElevation := 0.81625; // sqrt(2); //
  {$ifndef cgeapp}
  with CastleForm do
    begin
      ViewID := 2;
      CurrentProjection := PopupMenu1.Items[ViewID].Caption;
      PopupMenu1.Items[ViewID].Checked := True;
      EditWidth.Text := IntToStr(ViewWidth);
      EditHeight.Text := IntToStr(ViewHeight);
    end;
  {$endif}
  OriginalSize := TVector3.Zero;
  FullSize := True;
  SettingUp := False;

  PackedDataReader := TPackedDataReader.Create;
  PackedDataReader.SourceZipFileName := 'castle-data:/Models/Paid/Retro-Interiors-EnviroKit.zip';
  RegisterUrlProtocol('Retro-Interiors-EnviroKit', @PackedDataReader.ReadUrl, nil);

  //  IVC := TTexturesVideosCache.Create;
end;

destructor TCastleApp.Destroy;
begin
//  FreeAndNil(IVC);
  FreeAndNil(PackedDataReader);
  inherited;
end;

procedure TCastleApp.BootStrap(Sender: TObject);
begin
  WriteLnLog('BootStrap = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  LoadUI;
  LoadScene('castle-data:/tests/oblique.glb');
//  LoadScene('Retro-Interiors-EnviroKit:/glb/arch_interior_floorBig_stone_varA.glb');
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
      InfoLabel.Caption := Info;
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
  ViewPane.Color := Vector4(0.05, 0.05, 0.05, 1.0);
  ViewUI.InsertFront(ViewPane);

  CreateLabel(InfoLabel, 0);
  CreateNotification(InfoNote, 0, False);
  InfoLabel.Exists := False;
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

//    Viewport := CreateView(newScene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
    Viewport := CreateView(newScene);
    newScene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);

    AddDebugBox(newScene);

    {$ifndef cgeapp}
    SynchTrackbar;
    CastleForm.MenuInfo.Checked := InfoLabel.Exists;
    CastleForm.CurrentFile := AFile;
    CastleForm.CurrentModel := StripExtension(ExtractURIName(AFile));
    {$endif}

    Result := newScene;
    Scene := newScene;
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



procedure TCastleApp.BeforeRender;
begin
  inherited;
end;

procedure TCastleApp.Render;
begin
  inherited;
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

procedure TCastleApp.GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
var
  Sprite: TCastleImage;
  SName: String;
begin
  if not (Scene = nil) then
    begin
      InfoNote.Show('Creating Sprite');
      Sprite := CreateSpriteImage(Scene, SpriteWidth * OverSample, SpriteHeight * OverSample, UseTransparency);
      if not(Sprite = nil) then
        begin
          if (OverSample > 1) then
            begin
              Sprite.Resize(Trunc(Sprite.Width / OverSample), Trunc(Sprite.Height / OverSample), riLanczos); // Mitchel);
            end;
          SName := FileNameAutoInc('grab_%4.4d.png');
          SaveImage(Sprite, SName);
          FreeAndNil(Sprite);
          InfoNote.Show('Saved Sprite : ' + SName);
        end;
    end;
end;


function TCastleApp.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False; const useMainViewport: Boolean = True): TCastleImage;
var
  SourceViewport: TCastleViewport;
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

          SourceViewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
          Extents := SourceViewport.CalcAngles(SourceScene);
          SourceViewport.Camera.Orthographic.Scale := Extents.Size.X / TextureWidth;

          if useMainViewport then { Possibly use clone if desired }
            SourceViewport.Items := ViewPort.Items
          else
            begin
              SourceViewport.Items.UseHeadlight := hlMainScene;
              SourceViewport.Items.Add(SourceScene);
              SourceViewport.Items.MainScene := SourceScene;
            end;

          HeightAdjust := Extents.Size.Y / Extents.Size.X;
          SourceViewport.Height := Trunc(TextureHeight * HeightAdjust);

          {$ifndef cgeapp}
          CameraForm.ShowStats(SourceViewport);
          {$endif}


          BackImage := TRGBAlphaImage.Create(TextureWidth, Trunc(TextureHeight * HeightAdjust));
          BackImage.ClearAlpha(0);
          Image := TDrawableImage.Create(BackImage, true, true);
          Image.RenderToImageBegin;

          ViewportRect := Rectangle(0, 0, TextureWidth, Trunc(TextureHeight * HeightAdjust));
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
  NewVP.NavigationType := ntNone;
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

//  NewVP.Width := Trunc(VWidth / HeightAdjust);
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
//  ViewGrid.OwnsImage := True;
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

