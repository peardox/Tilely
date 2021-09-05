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
  CastleFilesUtils, 
  CastleStringUtils,
  CastleURIUtils;

type
  TPackedDataReader = class
  private
    fSourceZipFileName: String;
    Unzip: TUnZipper;
    fZipFiles: TStringList;
    procedure DoStartZipFile(Sender: TObject; const AFile: string);
    procedure DoEndZipFile(Sender: TObject; const Ratio: Double);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure SetSourceZipFileName(const AUrl: String);
  public
    function ReadUrl(const Url: string; out MimeType: string): TStream;
    constructor Create;
    destructor Destroy; override;
    property SourceZipFileName: String read fSourceZipFileName write SetSourceZipFileName;
  end;

implementation

{ TPackedDataReader ---------------------------------------------------------- }

procedure TPackedDataReader.SetSourceZipFileName(const AUrl: String);
var
  I: Integer;
begin
  if not(fSourceZipFileName = AUrl) then
    begin
      fSourceZipFileName := URIToFilenameSafe(AUrl);
      Unzip.FileName := fSourceZipFileName;
      if not(fZipFiles = nil) then
        FreeAndNil(fZipFiles);
      fZipFiles := TStringList.Create;
      fZipFiles.Sorted := True;
      fZipFiles.Duplicates := dupError;
      Unzip.Examine;
      for I := 0 to UnZip.Entries.Count - 1 do
        begin
          WriteLnLog('File : ' + UnZip.Entries[I].ArchiveFileName);
          fZipFiles.AddObject(UnZip.Entries[I].ArchiveFileName, nil);
        end;
    end;
end;

procedure TPackedDataReader.DoStartZipFile(Sender: TObject; const AFile: string);
begin
  WritelnLog('UnZip : DoStartZipFile : AFile = ' + AFile);
end;

procedure TPackedDataReader.DoEndZipFile(Sender: TObject; const Ratio: Double);
begin
  WritelnLog('UnZip : DoEndZipFile : Ratio = ' + FloatToStr(Ratio));
end;

procedure TPackedDataReader.DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
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
    raise Exception.CreateFmt('Can''t locate %s in %s', [
      AItem.ArchiveFileName,
      fSourceZipFileName
    ]);

end;

procedure TPackedDataReader.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  WritelnLog('UnZip : DoDoneOutZipStream : ' + AItem.ArchiveFileName);
  AStream.Position:=0;
end;

function TPackedDataReader.ReadUrl(const Url: string; out MimeType: string): TStream;
var
  I: Integer;
  AStream: TMemoryStream;
  FileInZip: String;
begin
  Result := nil;

  FileInZip := PrefixRemove('/', URIDeleteProtocol(Url), false);

  if fZipFiles.Find(FileInZip, I) then
    begin
      { If the requested file hasn't been extracted yet then do so }
      if fZipFiles.Objects[I] = nil then
        begin
          WriteLnLog('ReadUrl : Extract ' + Url + ' -> ' + FileInZip);
          Unzip.UnZipFile(FileInZip);
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
    raise Exception.CreateFmt('Can''t locate %s in %s', [
      FileInZip,
      fSourceZipFileName
    ]);
end;

constructor TPackedDataReader.Create;
begin
  inherited;
  Unzip := TUnZipper.Create;
  Unzip.OnCreateStream := @DoCreateOutZipStream;
  Unzip.OnDoneStream := @DoDoneOutZipStream;
  Unzip.OnStartFile := @DoStartZipFile;
  Unzip.OnEndFile := @DoEndZipFile;

  WritelnLog('Created UnZip');
end;

destructor TPackedDataReader.Destroy;
begin
  WritelnLog('Destroying UnZip');
  FreeAndNil(fZipFiles);
  FreeAndNil(Unzip);
  inherited;
end;

end.
