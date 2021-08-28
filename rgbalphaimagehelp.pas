unit RGBAlphaImageHelp;

{$mode objfpc}{$H+}
{$define usedword} // Comment out to use pixel-based approach

interface

uses
  Classes, SysUtils, CastleImages, CastleColors, CastleVectors;

type
  { TRGBAlphaImageHelper }
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
    procedure FastFillRect(const x1: Cardinal; const y1: Cardinal; const x2: Cardinal; const y2: Cardinal; const NewRGB: TVector4Byte);
  end;

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;

implementation

procedure TRGBAlphaImageHelper.FastFillRect(const x1: Cardinal; const y1: Cardinal; const x2: Cardinal; const y2: Cardinal; const NewRGB: TVector4Byte);
var
  PDest: PVector4Byte;
  XPos, YPos: Cardinal;
  AWidth, AHeight: Cardinal;
  x, y: Integer;
begin
  if not(Self = nil) then // Sanity check
    begin
      if not(Dimensions.IsZero) then // Sanity check
        begin
          if x1 < x2 then // Sort x1 + x2
            begin
              XPos := x1;
              AWidth := x2 - x1;
            end
          else
            begin
              XPos := x2;
              AWidth := x1 - x2;
            end;
          { Set XPos Min(x1, x2)
            Set AWidth accordingly (can be zero) }

          if y1 < y2 then // Sort y1 + y2
            begin
              YPos := y1;
              AHeight := y2 - y1;
            end
          else
            begin
              YPos := y2;
              AHeight := y1 - y2;
            end;
          { Set YPos Min(y1, y2)
            Set AHeight accordingly (can be zero) }

          if (XPos > Dimensions.X) then
            Exit;
          { Abort if XPos overflows }

          if (YPos > Dimensions.Y) then
            Exit;
          { Abort if YPos overflows }

          if ((XPos + AWidth) > Dimensions.X) then
            AWidth := Dimensions.X - XPos - 1;
          { Adjust AWidth if it overflows }

          if ((YPos + AHeight) > Dimensions.Y) then
            AHeight := Dimensions.Y - YPos - 1;
          { Adjust AHeight if it overflows }

          for y := 0 to AHeight do
            begin
              PDest := PixelPtr(XPos, YPos + y);
              {$ifdef usedword}
              FillDWord(PDest^, AWidth + 1, DWord(NewRGB));
              {$else}
              for x := 0 to AWidth do
                begin
                  PDest^ := NewRGB;
                  Inc(PDest);
                end;
              {$endif}
            end;
        end;
    end;
end;

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;
var
  img: TRGBAlphaImage;
  XPos: Cardinal;
  YPos: Cardinal;
  XGrid: Single;
  YGrid: Single;
  Skip: Integer;
  LightGrey: TVector4Byte;
begin
  if((AViewWidth < GridSize) or (AViewHeight < GridSize)) then
    Exit;

  img := TRGBAlphaImage.Create(AViewWidth, AViewHeight);

  LightGrey := Vector4Byte($B2, $B2, $B2, $FF); // Vector4Byte(255, 0, 0, 255);
  img.Clear(HexToColor('838383'));

  XGrid := AViewWidth / ASpriteWidth;
  YGrid := AViewHeight / ASpriteHeight;

  if (((GridSize * XGrid) < 1) or ((GridSize * YGrid) < 1)) then
    begin
      img.Clear(HexToColor('000000'));
    end
  else
    begin
      for YPos := 0 to (ASpriteHeight div GridSize) - 1 do
        begin
          Skip := YPos Mod 2;
          for XPos := 0 to (ASpriteWidth div GridSize) - 1 do
            begin
              if (((Skip + XPos) Mod 2) = 0) then
                begin
                  img.FastFillRect(Trunc(XPos * GridSize * XGrid), Trunc(YPos * GridSize * YGrid),
                    Trunc(((XPos + 1) * GridSize * XGrid)) - 1, Trunc(((YPos + 1) * GridSize * YGrid)) - 1,
                    LightGrey);
                end;
            end;
        end;
    end;

  Result := img;
end;

end.

