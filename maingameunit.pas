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
  X3DNodes, X3DFields, X3DTIme, CastleRectangles,
  CastleImages, CastleGLImages, CastleDebugTransform,
  CastleTextureImages, CastleCompositeImage, CastleBoxes,
  CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleFilesUtils, CastleKeysMouse, CastleNotifications,
  RGBAlphaImageHelp, MiscHelpers;

type
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
    ViewGrid: TCastleImageControl;
    fCameraRotation: Integer;
    fCameraRotationSteps: Integer;
    fCameraElevation: Single;
    ViewScale: Single;
    OriginalSize: TVector3;
    OriginalScale: Single;
    OriginalWidth: Single;
    StretchMultiplier: Single;
    procedure setCameraRotation(const AValue: Integer);
    procedure setCameraRotationSteps(const AValue: Integer);
    procedure setCameraElevation(const AValue: Single);
  public
    ViewPane: TCastleRectangleControl;
    ViewBack: TCastleRectangleControl;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Debug: TDebugTransformBox;
    InfoLabel: TCastleLabel;
    InfoNote: TCastleNotifications;
    SettingUp: Boolean;
    LastWidth: Single;
    ViewWidth: Single;
    ViewHeight: Single;
    SceneTilt: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure BootStrap(Sender: TObject);
    procedure Reflow;
    procedure LoadUI;
    procedure LoadScene(const AFile: String);
    function CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False; const useMainViewport: Boolean = True): TCastleImage;
    procedure ShowAppMessage(const AMsg: String);
    procedure GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
    property CameraRotation: Integer read fCameraRotation write setCameraRotation;
    property CameraRotationSteps: Integer read fCameraRotationSteps write setCameraRotationSteps;
    property CameraElevation: Single read fCameraElevation write setCameraElevation;
    procedure ShowInfo;
    function CreateView(const SourceScene: TCastleScene; const VWidth: Cardinal; const VHeight: Cardinal): TCastleViewport;
  end;

var
  AppTime: Int64;
  CastleApp: TCastleApp;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization, ShowCameraSettings;
{$endif}

{ TCastleApp }

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
//  LogTextureCache := True;
  ViewScale := 1;
  ViewWidth := 64;
  ViewHeight := 64;
  SettingUp := False;
  SceneTilt := 0;
  StretchMultiplier := 1;
  CameraRotationSteps := 8;
  CameraRotation := 1;
  CameraElevation := 1; // -0.81625; // -sqrt(2); //
  {$ifndef cgeapp}
  CastleForm.CurrentProjection := CastleForm.PopupMenu1.Items[3].Caption;
  CastleForm.PopupMenu1.Items[3].Checked := True;
  {$endif}
  OriginalSize := TVector3.Zero;
  FullSize := True;
//  IVC := TTexturesVideosCache.Create;
end;

destructor TCastleApp.Destroy;
begin
//  FreeAndNil(IVC);
  inherited;
end;

procedure TCastleApp.BootStrap(Sender: TObject);
begin
  WriteLnLog('BootStrap = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  LoadUI;
//  LoadScene('castle-data:/tests/brick_tent.gltf');
//  LoadScene('castle-data:/tests/isocam.glb');
//  LoadScene('castle-data:/tests/up.glb');
//  LoadScene('castle-data:/tests/sword.glb');
  LoadScene('castle-data:/tests/oblique.x3dv');
//  LoadScene('C:\Assets\Creative Trio\gltf\bridge\stone\Bridge_11.glb');
//  LoadScene('C:\Assets\ZerinLabs\Retro-Dungeon-EnviroKit\gltf\verA\floor_A.gltf');
//  LoadScene('C:\Assets\ZerinLabs\Retro-Dungeon-EnviroKit\gltf\verA\floor_B.gltf');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Buildings/Inn.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Crate.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Path_Square.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Path_Straight.glb');
//  LoadScene('castle-data:/medieval_objects/archeryrange.glb');
//  LoadScene('castle-data:/dungeon_tiles/floorDecoration_wood.glb');

  {$ifndef cgeapp}
  CastleForm.UpdateCaption;
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
//        'Viewport : ' + FloatToStr(ViewWidth) + ' x ' + FloatToStr(ViewHeight) + LineEnding +
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

  ViewPane := TCastleRectangleControl.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  ViewPane.FitRect(ViewWidth, ViewHeight, EffectiveRect);
  ViewPane.Color := Vector4(1,1,1,1);
  InsertFront(ViewPane);

  ViewGrid := TCastleImageControl.Create(ViewPane);
  ViewGrid.OwnsImage := True;
  ViewGrid.Stretch := True;
  ViewGrid.FullSize := True;
  ViewPane.InsertBack(ViewGrid);
  ViewGrid.Image :=  MakeTransparentLayerGrid(Trunc(ViewWidth), Trunc(ViewHeight), Trunc(ViewPane.Width), Trunc(ViewPane.Height), 8);

  CreateLabel(InfoLabel, 0);
  CreateNotification(InfoNote, 0, False);
  InfoLabel.Exists := False;
end;

procedure TCastleApp.LoadScene(const AFile: String);
begin
  {$ifndef cgeapp}
  CastleForm.CurrentFile := AFile;
  {$endif}

  if Assigned(Debug) then
    FreeAndNil(Debug);
  if Assigned(Scene) then
    FreeAndNil(Scene);
  try
    Scene := TCastleScene.Create(Self);
    Scene.Spatial := [ssDynamicCollisions, ssRendering];
    Scene.RenderOptions.MinificationFilter := minNearest;
    Scene.RenderOptions.MagnificationFilter := magNearest;
    Scene.Setup2D;
    try
      Scene.Load(AFile);
      OriginalSize := Scene.Normalize;
      Scene.HeadlightOn := True;

      Viewport := CreateView(Scene, Trunc(ViewPane.Width), Trunc(ViewPane.Height));
      Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
          True,
          Viewport.PrepareParams);

      Debug := TDebugTransformBox.Create(Self);
      Debug.Parent := Scene;
      Debug.BoxColor := Vector4(0,0,0, 1);
      Debug.Exists := False;


      {$ifndef cgeapp}
      SettingUp := True;
      CastleForm.TrackBar1.Position := CastleForm.TrackBar1.Max div 2;
      SettingUp := False;
      {$endif}

      {$ifndef cgeapp}
      CastleForm.MenuDebug.Checked := Debug.Exists;
      CastleForm.MenuInfo.Checked := InfoLabel.Exists;
      {$endif}

    except
      on E : Exception do
        begin
          WriteLnLog('Error : ' + LineEnding + E.ClassName + LineEnding + E.Message);
        end;
    end;
  finally
    // Do something if required
    WriteLnLog('Scene created -> ReFlow ');
    Reflow;
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
  if Assigned(Viewport) then
    begin
      Reflow;
    end;
end;

procedure TCastleApp.Reflow;
begin
  if Assigned(Viewport) and Assigned(Scene) then
    begin
      ViewPane.FitRect(ViewPane.RenderRect, EffectiveRect);
//      ViewGrid.FitRect(ViewGrid.RenderRect, ViewGrid.ParentRect);
      ViewBack.FitRect(ViewBack.RenderRect, ViewBack.ParentRect);
      Viewport.FitRect(Viewport.RenderRect, Viewport.ParentRect);
      if not(Viewport.Width = LastWidth) then
        begin
          Viewport.Camera.Orthographic.Scale := OriginalScale * (OriginalWidth / Viewport.Width);;
          LastWidth := Viewport.Width;
        end;
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

function TCastleApp.CreateView(const SourceScene: TCastleScene; const VWidth: Cardinal; const VHeight: Cardinal): TCastleViewport;
var
  NewVP: TCastleViewport;
  Extents: TExtents;
  HeightAdjust: Single;
begin
  if Assigned(Viewport) then
    begin
      ViewBack.RemoveControl(Viewport);
      ViewPane.RemoveControl(ViewBack);
      FreeAndNil(Viewport);
      FreeAndNil(ViewBack);
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

  ViewBack := TCastleRectangleControl.Create(Self);
  ViewBack.Color := Vector4(0,0,0,0.25);
  ViewPane.InsertFront(ViewBack);

  ViewBack.FitRect(NewVP.Width, NewVP.Height, ViewBack.ParentRect);
  ViewBack.InsertFront(NewVP);

  Result := NewVP;
end;

end.

