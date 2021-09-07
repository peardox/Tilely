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
  CastleStringUtils,
  CastleURIUtils;

type
  EZipError = class(Exception);

  TZipFileSystem = class
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
    procedure SetZipFile(const AUrl: String);
  public
    function ReadUrl(const AUrl: string; out MimeType: string): TStream;
    function getProtocol: String;
    constructor Create;
    constructor Create(const AUrl: string);
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
  E: SizeInt;
begin
  if not(fZipFile = AUrl) then
    begin
      fZipFile := URIToFilenameSafe(AUrl);

      if fZipFile = EmptyStr then
        raise EZipError.Create('Attempt to open an unnamed Zip File');

      P := LowerCase(ExtractFileName(fZipFile));
      E := P.IndexOf('.');
      if E = 0 then
        P := Random(1000000).ToString
      else if E > 0 then
        P := P.Remove(E);
      fFriendlyName := P;

      P := 'zip-data';

//      if not(RegisteredProtocols.Find(P) = nil) then
      if False then
        begin
          raise EZipError.CreateFmt('Attempt to use already defined protocol %s for %s', [P, fZipFile]);
        end
      else
      begin
        fProtocol := P;
        RegisterUrlProtocol(fProtocol, @ReadUrl, nil);
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
            WriteLnLog('File : ' + fUnzip.Entries[I].ArchiveFileName);
            fZipFiles.AddObject(fUnzip.Entries[I].ArchiveFileName, nil);
          end;
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
      WriteLnLog('DoCreateOutZipStream : Can''t locate ' + AItem.ArchiveFileName + ' in ' + fZipFile);
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

function TZipFileSystem.ReadUrl(const AUrl: string; out MimeType: string): TStream;
var
  I: Integer;
  AStream: TMemoryStream;
  FileInZip: String;
  RelUrl: String;
begin
  Result := nil;

  WriteLnLog('Url = ' + AUrl);
  RelUrl := PrefixRemove('/', URIDeleteProtocol(AUrl), false);
  WriteLnLog('NP Url = ' + RelUrl);
  RelUrl := RawURIDecode(fProtocol + ':/' + RelUrl);
  WriteLnLog('RelUrl = ' + RelUrl);
  RelUrl := PrefixRemove('/', URIDeleteProtocol(RelUrl), false);
  WriteLnLog('Combining ' + fProtocol + ':/ and ' + RelUrl);
  RelUrl := CombineURI(fProtocol + ':/', RelUrl);
  WriteLnLog('Combined = ' + RelUrl);
  FileInZip := PrefixRemove('/', URIDeleteProtocol(RelUrl), false);
  WriteLnLog('Target = ' + FileInZip);

  if fZipFiles.Find(FileInZip, I) then
    begin
      { If the requested file hasn't been extracted yet then do so }
      if fZipFiles.Objects[I] = nil then
        begin
          WriteLnLog('ReadUrl : Extract ' + RelUrl + LineEnding + '  ' + FileInZip);
          fUnzip.UnZipFile(FileInZip);
        end
      else
        WriteLnLog('ReadUrl : We already have ' + FileInZip);
      { We now have an Stream object - best double check anyway...}
      if not(fZipFiles.Objects[I] = nil) then
        begin
          MimeType := URIMimeType(FileInZip);
          AStream := TMemoryStream(fZipFiles.Objects[I]);
          WriteLnLog('Requested : ' + FileInZip + ', Loaded : ' + fZipFiles[i] + ', MimeType = ' + MimeType);
          WriteLnLog('Stream : Size = ' + IntToStr(AStream.Size) + ', Position = ' + IntToStr(AStream.Position));
          Result := AStream;
        end;
    end
  else
    begin
      WriteLnLog('ReadUrl : Can''t locate ' + FileInZip + ' in ' + fZipFile);
      raise EZipError.CreateFmt('Can''t locate %s in %s', [
        FileInZip,
        fZipFile
      ]);
    end;
end;

constructor TZipFileSystem.Create(const AUrl: string);
begin
  Create;
  ZipFile := AUrl;
end;

constructor TZipFileSystem.Create;
begin
  inherited Create;
  fUnzip := TUnZipper.Create;
  fUnzip.OnCreateStream := @DoCreateOutZipStream;
  fUnzip.OnDoneStream := @DoDoneOutZipStream;
  fUnzip.OnStartFile := @DoStartZipFile;
  fUnzip.OnEndFile := @DoEndZipFile;

  WritelnLog('Created UnZip');
end;

destructor TZipFileSystem.Destroy;
begin
  WritelnLog('Destroying UnZip');
  FreeAndNil(fZipFiles);
  FreeAndNil(fUnzip);
  inherited;
end;

end.
