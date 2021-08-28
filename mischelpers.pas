unit MiscHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  CastleVectors, CastleBoxes, CastleUIState, CastleControls,
  CastleUIControls, CastleNotifications, CastleColors,
  CastleRectangles;

type
  { TExtents }
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
    procedure ViewFromRadiusZ(const ARadius: Single; const ADirection: TVector3);
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
    procedure CreateNotification(var objNotification: TCastleNotifications; const Line: Integer; const BottomUp: Boolean = True; const Rows: Cardinal = 1; RightAlign: Boolean = False);
  end;

  { TCastleUserInterface }
  TCastleUserInterfaceHelper = class helper for TCastleUserInterface
  public
    procedure SetRect(const R: TFloatRectangle);
    procedure FitRect(const AWidth: Single; const AHeight: Single; const R: TFloatRectangle);
    procedure FitRect(const S: TFloatRectangle; const R: TFloatRectangle);
  end;

implementation
uses MainGameUnit;

{ TCastleViewportHelper }

procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
begin
    ViewFromRadius(ARadius, Vector3(sqrt(ARadius) * Cos(ATheta), AElevation, sqrt(ARadius) * Sin(ATheta)));
end;

procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
begin
  Camera.Up := Vector3(0, 1, 0);
  Camera.Direction := -ADirection;
  Camera.Position  := ARadius * ADirection.Normalize;
end;

procedure TCastleViewportHelper.ViewFromRadiusZ(const ARadius: Single; const ADirection: TVector3);
begin
  Camera.Up := Vector3(0, 0, -1);
  Camera.Direction := ADirection;
  Camera.Position  := ARadius * -ADirection.Normalize;
  CastleApp.ViewPane.Color := Vector4(0, 0, 1, 1);
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
  Extents.Pixels.X := EffectiveWidth;
  Extents.Pixels.Y := EffectiveHeight;

  if (EffectiveWidth > 0) and (EffectiveHeight > 0) then
    begin
      if not(AScene = nil) then
        begin
          if not AScene.BoundingBox.IsEmptyOrZero then
            begin
              AScene.BoundingBox.Corners(corners);
              PositionToRay(Vector2(0, 0), True, RayOrigin, RayDirection);
              for i := Low(corners) to High(corners) do
                begin
                  OutputPoint3D := (Camera.ProjectionMatrix * Camera.Matrix).MultPoint(corners[i]);
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
              Extents.Aspect := Extents.Size.X / Extents.Size.Y;
            end;
        end;
    end;

  Result := Extents;
end;

{ TCastleSceneHelper }

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

            Translation := -Center;
          end;
      end;
    end;
end;

{ TUIStateHelper }

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

procedure TUIStateHelper.CreateNotification(var objNotification: TCastleNotifications; const Line: Integer; const BottomUp: Boolean = True; const Rows: Cardinal = 1; RightAlign: Boolean = False);
begin
  objNotification := TCastleNotifications.Create(Self);
  objNotification.MaxMessages := 1;
//  objNotification.Padding := 5;
  objNotification.Color := White;
//  objNotification.Frame := True;
//  objNotification.FrameColor := Black;
  objNotification.Anchor(hpLeft, 10);
  if RightAlign then
    objNotification.Anchor(hpRight, -10)
  else
    objNotification.Anchor(hpLeft, 10);
  if BottomUp then
    objNotification.Anchor(vpBottom, 10 + (Line * 35))
  else
    objNotification.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objNotification);
end;

{ TCastleUserInterfaceHelper }

procedure TCastleUserInterfaceHelper.SetRect(const R: TFloatRectangle);
begin
  Left := R.Left;
  Bottom := R.Bottom;
  Width := R.Width;
  Height := R.Height;
end;

procedure TCastleUserInterfaceHelper.FitRect(const AWidth: Single; const AHeight: Single; const R: TFloatRectangle);
begin
  setRect(FloatRectangle(0, 0, AWidth, AHeight).FitInside(R));
end;

procedure TCastleUserInterfaceHelper.FitRect(const S: TFloatRectangle; const R: TFloatRectangle);
begin
  setRect(S.FitInside(R));
end;

end.

