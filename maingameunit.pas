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
  CastleFilesUtils, CastleKeysMouse;

type
  TExtents = record
    Min: TVector2;
    Max: TVector2;
    Size: TVector2;
    Pixels: TVector2;
    Aspect: Single;
  end;

  { TCastleViewportHelper }
  TCastleViewportHelper = class helper for TCastleViewport
  public
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
    function CalcAngles(const AScene: TCastleScene): TExtents;
  end;

  { TCastleSceneHelper }
  TCastleSceneHelper = class helper for TCastleScene
  public
    function Normalize: TVector3;
  end;

  { TUIStateHelper }
  TUIStateHelper = class helper for TUIState
  public
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
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
    fCameraRotation: Integer;
    fCameraRotationSteps: Integer;
    fCameraElevation: Single;
    ViewScale: Single;
    ViewWidth: Single;
    ViewHeight: Single;
    OriginalSize: TVector3;
    StretchMultiplier: Single;
    procedure setCameraRotation(const AValue: Integer);
    procedure setCameraRotationSteps(const AValue: Integer);
    procedure setCameraElevation(const AValue: Single);
  public
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Debug: TDebugTransformBox;
    InfoLabel: TCastleLabel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure BootStrap(Sender: TObject);
    procedure Reflow;
    procedure LoadViewport;
    procedure LoadScene(const AFile: String);
    function CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False): TCastleImage;
    procedure ShowAppMessage(const AMsg: String);
    procedure GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
    property CameraRotation: Integer read fCameraRotation write setCameraRotation;
    property CameraRotationSteps: Integer read fCameraRotationSteps write setCameraRotationSteps;
    property CameraElevation: Single read fCameraElevation write setCameraElevation;
    procedure ShowInfo;
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
  ViewWidth := 512;
  ViewHeight := 512;
  StretchMultiplier := 1;
  CameraRotationSteps := 8;
  CameraRotation := 1;
  CameraElevation := -0.81625; // -sqrt(2); //
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
  LoadViewport;
//  LoadScene('castle-data:/tests/brick_tent_8.glb');
//  LoadScene('castle-data:/tests/seperates/brick_tent_8.obj');
//  LoadScene('castle-data:/tests/isocam.glb');
//  LoadScene('castle-data:/tests/isocam_bad_offset.glb');
//  LoadScene('castle-data:/tests/isocam_micro_zoom.glb');
//  LoadScene('castle-data:/tests/isocam_macro_zoom.glb');
  LoadScene('castle-data:/tests/oblique.glb');
//  LoadScene('C:\Assets\ZerinLabs\Retro-Dungeon-EnviroKit\gltf\verA\floor_A.gltf');
//  LoadScene('C:\Assets\ZerinLabs\Retro-Dungeon-EnviroKit\gltf\verA\floor_B.gltf');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Buildings/Inn.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Crate.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Path_Square.glb');
//  LoadScene('castle-data:/Models/Quaternius/Medieval Village - Dec 2020/Props/Path_Straight.glb');
//  LoadScene('castle-data:/medieval_objects/archeryrange.glb');
//  LoadScene('castle-data:/dungeon_tiles/floorDecoration_wood.glb');
//  GrabSprite(256, 256, 8);
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
        Viewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
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
        'Viewport : ' + FloatToStr(ViewWidth) + ' x ' + FloatToStr(ViewHeight) + LineEnding +
        'Elevation : ' + FLoatToStr(CameraElevation) + LineEnding +
        'Rotation : ' + FloatToStr((CameraRotation / CameraRotationSteps) * 360) + LineEnding +
        'Translation : ' + Scene.Translation.ToString;

//        'Extents : ' + FloatToStr(Extents.Min.X) + ' x ' + FloatToStr(Extents.Min.Y) + ' - '  + FloatToStr(Extents.Max.X) + ' x '  + FloatToStr(Extents.Max.Y) + ' : '  + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y) + LineEnding +
      InfoLabel.Caption := Info;
    end;
end;

function TCastleViewportHelper.CalcAngles(const AScene: TCastleScene): TExtents;
var
  corners: TBoxCorners;
  OutputPoint3D, RayDirection, RayOrigin: TVector3;
  i: Integer;
  Extents: TExtents;
begin
  Extents.Min := Vector2(Infinity, Infinity);
  Extents.Max := Vector2(-Infinity, -Infinity);
  if not(AScene = nil) then
    begin
      if not AScene.BoundingBox.IsEmptyOrZero then
        begin
          AScene.BoundingBox.Corners(corners);
          Self.PositionToRay(Vector2(0, 0), True, RayOrigin, RayDirection);
          for i := Low(corners) to High(corners) do
            begin
              OutputPoint3D := (Self.Camera.ProjectionMatrix * Self.Camera.Matrix).MultPoint(corners[i]);
              if OutputPoint3D.X < Extents.Min.X then
                Extents.Min.X := OutputPoint3D.X;
              if OutputPoint3D.Y < Extents.Min.Y then
                Extents.Min.Y := OutputPoint3D.Y;
              if OutputPoint3D.X > Extents.Max.X then
                Extents.Max.X := OutputPoint3D.X;
              if OutputPoint3D.Y > Extents.Max.Y then
                Extents.Max.Y := OutputPoint3D.Y;
            end;

          Extents.Size.X := (Extents.Max.X - Extents.Min.X);
          Extents.Size.Y := (Extents.Max.Y - Extents.Min.Y);
          Extents.Pixels.X := (Self.EffectiveWidth);
          Extents.Pixels.Y := (Self.EffectiveHeight);
          Extents.Aspect := Extents.Size.X / Extents.Size.Y;

        end;
    end;
  WriteLnLog('2:1 = ' + FormatFloat('0.0000', ArcTan(0.5)));
  WriteLnLog('Extents.Min  = ' + Extents.Min.ToString);
  WriteLnLog('Extents.Max  = ' + Extents.Max.ToString);
  WriteLnLog('Extents.Size = ' + Extents.Size.ToString);

  Result := Extents;
end;

procedure TCastleApp.LoadViewport;
begin
  WriteLnLog('LoadViewport = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));

  Viewport := TCastleViewport.Create({$ifndef cgeapp}CastleForm.{$endif}Window);

  Viewport.FullSize := False;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;

  Viewport.BackgroundColor := Vector4(1, 0, 0, 1);
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;

  if(EffectiveHeight < EffectiveWidth) then
    begin
      Viewport.Width := EffectiveHeight;
      Viewport.Height := EffectiveHeight;
    end
  else
  begin
    Viewport.Width := EffectiveWidth;
    Viewport.Height := EffectiveWidth;
  end;

  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Width := ViewWidth;
  Viewport.Camera.Orthographic.Height := ViewHeight;
  Viewport.Camera.Orthographic.Scale := 1;
  Viewport.Camera.ProjectionType := ptOrthographic;

  InsertFront(Viewport);
  CreateLabel(InfoLabel, 0);
  InfoLabel.Exists := False;

  Reflow;
end;

procedure TCastleApp.LoadScene(const AFile: String);
var
  Extents: TExtents;
  Info: String;
begin
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
      Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
          True,
          Viewport.PrepareParams);
      OriginalSize := Scene.Normalize;

      Viewport.Items.UseHeadlight := hlMainScene;
      Scene.HeadlightOn := True;

      Debug := TDebugTransformBox.Create(Self);
      Debug.Parent := Scene;
      Debug.BoxColor := Vector4(0,0,0, 1);
      Debug.Exists := False;

      Viewport.Items.Add(Scene);
      Viewport.Items.MainScene := Scene;
      Viewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));

      Extents := Viewport.CalcAngles(Scene);
      ViewWidth := Extents.Size.X;
      ViewHeight := Extents.Size.Y;
//      HeightAdjust := Extents.Size.Y / Extents.Size.X;
      Viewport.Camera.Orthographic.Width := ViewWidth;
      Viewport.Camera.Orthographic.Height := ViewHeight;

      Viewport.Camera.Orthographic.Scale := Extents.Size.X / ViewWidth;

      Info := 'ViewScale : ' + FloatToStr(ViewScale) + LineEnding +
        'BB : ' + Scene.BoundingBox.Data[0].ToString + ' - ' + Scene.BoundingBox.Data[1].ToString + LineEnding +
        'Scale : ' + Scene.Scale.ToString + LineEnding +
        'Center : ' + Scene.Center.ToString + LineEnding +
        'Original : ' + OriginalSize.ToString + LineEnding +
        'Viewport : ' + FloatToStr(ViewWidth) + ' x ' + FloatToStr(ViewHeight) + LineEnding +
        'Extents : ' + FloatToStr(Extents.Min.X) + ' x ' + FloatToStr(Extents.Min.Y) + ' - '  + FloatToStr(Extents.Max.X) + ' x '  + FloatToStr(Extents.Max.Y) + ' : '  + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y) + LineEnding +
        'Elevation : ' + FLoatToStr(CameraElevation) + LineEnding +
        'Rotation : ' + FloatToStr((CameraRotation / CameraRotationSteps) * 360) + LineEnding +
        'Translation : ' + Scene.Translation.ToString;

      InfoLabel.Caption := Info;
      WriteLnLog(Info);

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
//  WriteLnLog('Resize = ' + FloatToStr(EffectiveWidth) + ' x ' + FloatToStr(EffectiveHeight));
  if Assigned(Viewport) then
    Reflow;
end;

procedure TCastleApp.Reflow;
var
  DesiredAspect: Single;
  ActualAspect: Single;
//  Extents: TExtents;
begin
{
  Extents := Viewport.CalcAngles(Scene);
  ViewWidth := Extents.Size.X;
  ViewHeight := Extents.Size.Y * (Extents.Size.Y / Extents.Size.X);

  Viewport.Camera.Orthographic.Width := ViewWidth;
  Viewport.Camera.Orthographic.Height := ViewHeight;
}
  DesiredAspect := ViewWidth / ViewHeight;
  ActualAspect := EffectiveWidth / EffectiveHeight;

  if DesiredAspect <= ActualAspect then // Landscape
    begin
      Viewport.Height := EffectiveHeight;
      Viewport.Width := (EffectiveHeight * DesiredAspect);
      ViewScale := Viewport.Width / ViewWidth;
    end
  else  // Portrait
    begin
      Viewport.Width := EffectiveWidth;
      Viewport.Height := (EffectiveWidth / DesiredAspect);
      ViewScale := Viewport.Height / ViewHeight;
    end;

  Viewport.Left := Trunc((EffectiveWidth - Viewport.Width) / 2);
  Viewport.Bottom := Trunc((EffectiveHeight - Viewport.Height) / 2);
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

procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
begin
  ViewFromRadius(ARadius, Vector3(sqrt(ARadius) * Cos(ATheta), AElevation, sqrt(ARadius) * Sin(ATheta)));
end;

procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
begin
  Camera.Up := Vector3(0, 1, 0);
  Camera.Direction := ADirection;
  Camera.Position  := ARadius * -ADirection.Normalize;
end;

function TCastleSceneHelper.Normalize: TVector3;
begin
  Result := TVector3.Zero;
  if not(Self = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Result := BoundingBox.Size;
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(2 / BoundingBox.MaxSize,
                             2 / BoundingBox.MaxSize,
                             2 / BoundingBox.MaxSize);

            WriteLnLog( 'BB' + BoundingBox.Data[0].ToString + ' - ' + BoundingBox.Data[1].ToString );
            Translation := -Center;
          end;
      end;
    end;
end;

procedure TUIStateHelper.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
begin
  objLabel := TCastleLabel.Create(Self);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if RightAlign then
    objLabel.Anchor(hpRight, -10)
  else
    objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
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
        end;
    end;
end;


function TCastleApp.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False): TCastleImage;
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

          SourceViewport.Setup2D;
          SourceViewport.Camera.ProjectionType := Viewport.Camera.ProjectionType;
          SourceViewport.Camera.Orthographic.Origin := Viewport.Camera.Orthographic.Origin;

          SourceViewport.Width := TextureWidth;
          SourceViewport.Height := TextureHeight;

          if Viewport.Camera.Orthographic.Stretch then
            begin
              SourceViewport.Camera.Orthographic.Stretch := True;
              SourceViewport.Camera.Orthographic.Width := SourceViewport.EffectiveWidth / StretchMultiplier;
              SourceViewport.Camera.Orthographic.Height := SourceViewport.EffectiveHeight;
            end;

          SourceViewport.ViewFromRadius(2, CameraElevation, 2 * pi * (CameraRotation / CameraRotationSteps));
          Extents := SourceViewport.CalcAngles(SourceScene);
          SourceViewport.Camera.Orthographic.Scale := Extents.Size.X / TextureWidth;

          SourceViewport.Items := ViewPort.Items;

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

end.

