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
  end;

  { TCastleViewportHelper }
  TCastleViewportHelper = class helper for TCastleViewport
  public
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
  end;

  { TCastleSceneHelper }
  TCastleSceneHelper = class helper for TCastleScene
  public
    procedure Normalize;
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
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Debug: TDebugTransformBox;
    ViewScale: Single;
    ViewWidth: Single;
    ViewHeight: Single;
    StretchMultiplier: Single;
  public
    InfoLabel: TCastleLabel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure BootStrap(Sender: TObject);
    procedure Reflow;
    procedure LoadViewport;
    procedure LoadScene(const AFile: String);
    function CalcAngles(const AScene: TCastleScene): TExtents;
    function CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False): TCastleImage;
    procedure ShowAppMessage(const AMsg: String);
    procedure GrabSprite(const SpriteWidth: Integer; const SpriteHeight: Integer; const OverSample: Integer = 8; const UseTransparency: Boolean = True);
  end;

var
  AppTime: Int64;
  CastleApp: TCastleApp;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

{ TCastleApp }

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
//  LogTextureCache := True;
  ViewScale := 1;
  ViewWidth := 64;
  ViewHeight := 64;
  StretchMultiplier := 1;
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
  LoadScene('castle-data:/medieval_objects/archeryrange.glb');
//      LoadScene('castle-data:/dungeon_tiles/floorDecoration_wood.glb');
  GrabSprite(512, 512, 1);
end;

function TCastleApp.CalcAngles(const AScene: TCastleScene): TExtents;
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
          Viewport.PositionToRay(Vector2(0, 0), True, RayOrigin, RayDirection);
          for i := Low(corners) to High(corners) do
            begin
              OutputPoint3D := (Viewport.Camera.ProjectionMatrix * Viewport.Camera.Matrix).MultPoint(corners[i]);
              WriteLnLog('Corner #' + IntToStr(i) + ' : ' + corners[i].ToString + ' => ' + FormatFloat('0.00000000', OutputPoint3D.X) + ', ' + FormatFloat('0.00000000', OutputPoint3D.Y));
              if OutputPoint3D.X < Extents.Min.X then
                Extents.Min.X := OutputPoint3D.X;
              if OutputPoint3D.Y < Extents.Min.Y then
                Extents.Min.Y := OutputPoint3D.Y;
              if OutputPoint3D.X > Extents.Max.X then
                Extents.Max.X := OutputPoint3D.X;
              if OutputPoint3D.Y > Extents.Max.Y then
                Extents.Max.Y := OutputPoint3D.Y;
            end;
          Extents.Size.X := Extents.Max.X - Extents.Min.X;
          Extents.Size.Y := Extents.Max.Y - Extents.Min.Y;
        end;
    end;
  WriteLnLog('2:1 = ' + FormatFloat('0.0000', ArcTan(0.5)));
  WriteLnLog('Extents.Min = ' + Extents.Min.ToString);
  WriteLnLog('Extents.Max = ' + Extents.Max.ToString);

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

  Reflow;
end;

procedure TCastleApp.LoadScene(const AFile: String);
var
  Extents: TExtents;
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
      Scene.Normalize;

      Debug := TDebugTransformBox.Create(Self);
      Debug.Parent := Scene;
      Debug.BoxColor := Vector4(0,0,0, 1);
      Debug.Exists := True;

      Viewport.Items.Add(Scene);
      Viewport.Items.MainScene := Scene;

      Viewport.ViewFromRadius(2, -0.81625, 2 * pi * (7/8));
      Extents := CalcAngles(Scene);

      ViewScale := Min(ViewWidth, ViewHeight) /
        Max(Extents.Size.X, Extents.Size.Y);
      Viewport.Camera.Orthographic.Scale := 1/ViewScale;

      InfoLabel.Caption := 'ViewScale : ' + FloatToStr(ViewScale) + LineEnding +
        'BB ' + Scene.BoundingBox.Data[0].ToString + ' - ' + Scene.BoundingBox.Data[1].ToString + LineEnding +
        'Scale : ' + Scene.Scale.ToString + LineEnding +
        'Center : ' + Scene.Center.ToString + LineEnding +
        'Viewport : ' + FloatToStr(ViewWidth) + ' x ' + FloatToStr(ViewHeight) + LineEnding +
        'Extents : ' + FloatToStr(Extents.Min.X) + ' x ' + FloatToStr(Extents.Min.Y) + ' - '  + FloatToStr(Extents.Max.X) + ' x '  + FloatToStr(Extents.Max.Y) + ' : '  + FloatToStr(Extents.Size.X) + ' x '  + FloatToStr(Extents.Size.Y) + LineEnding +
        'Translation : ' + Scene.Translation.ToString;

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
begin
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

procedure TCastleSceneHelper.Normalize;
begin
  if not(Self = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(2 / BoundingBox.MaxSize,
                             2 / BoundingBox.MaxSize,
                             2 / BoundingBox.MaxSize);

            WriteLnLog( 'BB' + BoundingBox.Data[0].ToString + ' - ' + BoundingBox.Data[1].ToString );
            Translation := -Center;
{
            Translation := Vector3((BoundingBox.Data[0].X - BoundingBox.Data[1].X) / 2,
                                   (BoundingBox.Data[0].Y - BoundingBox.Data[1].Y) / 2,
                                   (BoundingBox.Data[0].Z - BoundingBox.Data[1].Z) / 2);
}
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
              Sprite.Resize(SpriteWidth, SpriteHeight, riLanczos); // Mitchel);
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
begin
  SourceViewport := nil;

  if not(SourceScene = nil) and (TextureWidth > 0) and (TextureHeight > 0) then
    begin
      try
        try
          BackImage := TRGBAlphaImage.Create(TextureWidth, TextureHeight);
          BackImage.ClearAlpha(0);

          Image := TDrawableImage.Create(BackImage, true, true);

          Image.RenderToImageBegin;

          SourceViewport := TCastleViewport.Create(nil);
          SourceViewport.Width := TextureWidth;
          SourceViewport.Height := TextureHeight;
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
          SourceViewport.Camera.Up := Viewport.Camera.Up;
          SourceViewport.Camera.Direction := Viewport.Camera.Direction;
          SourceViewport.Camera.Position  := Viewport.Camera.Position;

          if Viewport.Camera.Orthographic.Stretch then
            begin
              SourceViewport.Camera.Orthographic.Stretch := True;
              SourceViewport.Camera.Orthographic.Width := SourceViewport.EffectiveWidth / StretchMultiplier;
              SourceViewport.Camera.Orthographic.Height := SourceViewport.EffectiveHeight;
            end;

          Extents := CalcAngles(SourceScene);

          SourceViewport.Camera.Orthographic.Scale := 1 / (Min(TextureWidth, TextureHeight) /
            Max(Extents.Size.X, Extents.Size.Y));

          SourceViewport.Items := ViewPort.Items;

          ViewportRect := Rectangle(0, 0, TextureWidth, TextureHeight);
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

