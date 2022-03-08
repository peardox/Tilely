unit DataForest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TSpriteFile }
  TSpriteFile = class(TPersistent)
    private
      fName: String;
      fAbsoluteName: String;
      fLastModified: Int64;
      fSize: Int64;
    public
      property Name: String read fName write fName;
      property AbsoluteName: String read fAbsoluteName write fAbsoluteName;
      property LastModified: Int64 read fLastModified write fLastModified;
      property Size: Int64 read fSize write fSize;
      constructor Create(const AName: String; const AAbsName: String; const ALastModified: Int64; const ASize: Int64);
  end;

  { TDirectoryFile }
  PDirectoryFile = ^TDirectoryFile;
  PDirectoryFileArray = Array of PDirectoryFile;

  TDirectoryFile = class(TPersistent)
    private
      fParent: PDirectoryFile;
      fChildren: PDirectoryFileArray;
      fCount: Integer;
      fName: String;
      fHash: String;
      fLastModified: Int64;
      fSize: Int64;
      fIsDirectory: Boolean;
    public
      property Parent: PDirectoryFile read fParent write fParent;
      property Children: PDirectoryFileArray read fChildren write fChildren;
      property Count: Integer read fCount write fCount;
      property Name: String read fName write fName;
      property Hash: String read fHash write fHash;
      property LastModified: Int64 read fLastModified write fLastModified;
      property Size: Int64 read fSize write fSize;
      property IsDirectory: Boolean read fIsDirectory write fIsDirectory;
      constructor Create;
      constructor Create(const AParent: PDirectoryFile);
      function AddFile(const AName: String; const AHash: String; const ALastModified: Int64; const ASize: Int64): TDirectoryFile;
      function AddFolder(const AName: String; const ALastModified: Int64): TDirectoryFile;
  end;


implementation

constructor TSpriteFile.Create(const AName: String; const AAbsName: String; const ALastModified: Int64; const ASize: Int64);
begin
  inherited Create;
  fName := AName;
  fLastModified := ALastModified;
  fSize := ASize;
  fAbsoluteName := AAbsName;
end;

constructor TDirectoryFile.Create;
begin
  inherited Create;
  fParent := Nil;
end;

constructor TDirectoryFile.Create(const AParent: PDirectoryFile);
begin
  inherited Create;
  fParent := AParent;
end;

function TDirectoryFile.AddFile(const AName: String; const AHash: String; const ALastModified: Int64; const ASize: Int64): TDirectoryFile;
var
  AFile: TDirectoryFile;
begin
  AFile := TDirectoryFile.Create(fParent);
  AFile.fName := AName;
  AFile.fLastModified := ALastModified;
  AFile.fSize := ASize;
  AFile.fHash := AHash;
  AFile.fIsDirectory := False;
  SetLength(fChildren, fCount + 1);
  fChildren[fCount] := @AFile;
  Inc(fCount);
  Inc(fSize);

  Result := AFile;
end;

function TDirectoryFile.AddFolder(const AName: String; const ALastModified: Int64): TDirectoryFile;
var
  AFile: TDirectoryFile;
begin
  AFile := TDirectoryFile.Create(fParent);
  AFile.fName := AName;
  AFile.fLastModified := ALastModified;
  AFile.fSize := 0;
  AFile.fHash := '';
  AFile.fIsDirectory := True;
  SetLength(fChildren, fCount + 1);
  fChildren[fCount] := @AFile;
  Inc(fCount);
  Inc(fSize);

  Result := AFile;
end;

// https://spritely.co.uk/sheetchild.php

end.

