{
  Copyright 2019-2021 Michalis Kamburelis.

  Modified by Peardox

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit ZipUrls;

interface

uses SysUtils, Classes, Zipper,
  CastleLog, CastleUtils,
  CastleFilesUtils, CastleDownload,
  CastleStringUtils, CastleURIUtils;

type
  EZipError = class(Exception);

  TZipFileSystem = class(TComponent)
  private
    fProtocol: String;
    fZipFile: String;
    fFriendlyName: String;
    fUnzip: TUnZipper;
    fZipFiles: TStringList;
    procedure DoStartZipFile(Sender: TObject; const AFile: string);
    procedure DoEndZipFile(Sender: TObject; const Ratio: Double);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoOpenInputStream(Sender: TObject; var AStream: TStream);
    procedure DoCloseInputStream(Sender: TObject; var AStream: TStream);
    procedure SetZipFile(const AUrl: String);
  public
    function GetStream(const AUrl: string): TStream;
    function GetStream(const AUrl: string; out MimeType: string): TStream;
    function ReadZip(const AUrl: string; out MimeType: string): TStream;
    function getProtocol: String;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AUrl: string);
    destructor Destroy; override;
    property ZipFile: String read fZipFile write SetZipFile;
    property Protocol: String read getProtocol;
    property FriendlyName: String read fFriendlyName write fFriendlyName;
    property Files: TStringList read fZipFiles;
  end;

implementation

{ TZipFileSystem ---------------------------------------------------------- }

function TZipFileSystem.getProtocol: String;
begin
  if not(fProtocol = EmptyStr) then
    Result := fProtocol + ':'
  else
    begin
      Result := fProtocol;
      raise EZipError.CreateFmt('EZipError : Using undefined protocol for %s', [fZipFile]);
    end;
end;

procedure TZipFileSystem.SetZipFile(const AUrl: String);
var
  I: Integer;
  P: String;
  F: String;
  E: SizeInt;
begin
  if not(fZipFile = AUrl) then
    begin
      fZipFile := URIToFilenameSafe(AUrl);

      if fZipFile = EmptyStr then
        raise EZipError.Create('Attempt to open an unnamed Zip File');

      // Create a friendly name for the zip
      F := LowerCase(ExtractFileName(fZipFile));
      E := F.IndexOf('.zip');
      if E = 0 then // A file called .zip is being opened?
        F := 'dotzip'
      else if E > 0 then
        F := F.Remove(E);
      fFriendlyName := F;

      // Create a protocol for the zip
      I := 1;
      repeat
        P := 'zip-data-' + Format('%d', [I]);
        if URIValidProtocol(P) then
          if not(RegisteredUrlProtocol(P)) then
            Break;
        Inc(I);
      until false;

    fProtocol := P;
    RegisterUrlProtocol(fProtocol, @ReadZip, nil);
    WriteLnLog('Registered Protocol ' + fProtocol + ' for ' + fZipFile);

    fUnzip.FileName := fZipFile;
    if not(fZipFiles = nil) then
      FreeAndNil(fZipFiles);
    fZipFiles := TStringList.Create;
    fZipFiles.Sorted := True;
    fZipFiles.Duplicates := dupError;
    fUnzip.Examine;
    for I := 0 to fUnzip.Entries.Count - 1 do
      begin
//        WriteLnLog('File : ' + fUnzip.Entries[I].ArchiveFileName);
        fZipFiles.AddObject(fUnzip.Entries[I].ArchiveFileName, nil);
      end;
    end;
end;

procedure TZipFileSystem.DoStartZipFile(Sender: TObject; const AFile: string);
begin
  WritelnLog('UnZip : DoStartZipFile : AFile = ' + AFile);
end;

procedure TZipFileSystem.DoEndZipFile(Sender: TObject; const Ratio: Double);
begin
  WritelnLog('UnZip : DoEndZipFile : Ratio = ' + FloatToStr(Ratio));
end;

procedure TZipFileSystem.DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  I: Integer;
begin
  WritelnLog('UnZip : DoCreateOutZipStream : ' + AItem.ArchiveFileName);
  if fZipFiles.Find(AItem.ArchiveFileName, I) then
    begin
      AStream := TMemorystream.Create;
      fZipFiles.Objects[I] := AStream;
    end
  else
    begin
      raise EZipError.CreateFmt('Can''t locate %s in %s', [
        AItem.ArchiveFileName,
        fZipFile
      ]);
    end;
end;

procedure TZipFileSystem.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  WritelnLog('UnZip : DoDoneOutZipStream : ' + AItem.ArchiveFileName);
  AStream.Position:=0;
end;

procedure TZipFileSystem.DoOpenInputStream(Sender: TObject; var AStream: TStream);
begin
  WriteLnLog('DoOpenInputStream');
end;

procedure TZipFileSystem.DoCloseInputStream(Sender: TObject; var AStream: TStream);
begin
  WriteLnLog('DoCloseInputStream');
end;

function TZipFileSystem.GetStream(const AUrl: string): TStream;
var
  MimeType: String;
begin
  Result := ReadZip(AUrl, MimeType);
end;

function TZipFileSystem.GetStream(const AUrl: string; out MimeType: string): TStream;
begin
  Result := ReadZip(AUrl, MimeType);
end;

function TZipFileSystem.ReadZip(const AUrl: string; out MimeType: string): TStream;
var
  I: Integer;
  FileInZip: String;
  RelUrl: String;
begin
  Result := nil;

  FileInZip := PrefixRemove('/', URIDeleteProtocol(AUrl), false);

  if fZipFiles.Find(FileInZip, I) then
    begin
      { If the requested file hasn't been extracted yet then do so }
      if fZipFiles.Objects[I] = nil then
        begin
          fUnzip.UnZipFile(FileInZip);
        end;

      { We now have an Stream object - best double check anyway...}
      if not(fZipFiles.Objects[I] = nil) then
        begin
          MimeType := URIMimeType(FileInZip);
          Result := TMemoryStream(fZipFiles.Objects[I]);
        end;
    end
  else
    begin
      raise EZipError.CreateFmt('Can''t locate %s in %s', [
        FileInZip,
        fZipFile
      ]);
    end;
end;

constructor TZipFileSystem.Create(AOwner: TComponent; const AUrl: string);
begin
  Create(AOwner);
  ZipFile := AUrl;
end;

constructor TZipFileSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fUnzip := TUnZipper.Create;
  fUnzip.OnCreateStream := @DoCreateOutZipStream;
  fUnzip.OnDoneStream := @DoDoneOutZipStream;
  fUnzip.OnStartFile := @DoStartZipFile;
  fUnzip.OnEndFile := @DoEndZipFile;
  fUnZip.OnOpenInputStream := @DoOpenInputStream;
  fUnZip.OnCloseInputStream := @DoCloseInputStream;
end;

destructor TZipFileSystem.Destroy;
begin
  FreeAndNil(fZipFiles);
  FreeAndNil(fUnzip);
  WriteLnLog('UnRegistering Protocol ' + fProtocol);
  UnregisterUrlProtocol(fProtocol);
  inherited;
end;

end.
