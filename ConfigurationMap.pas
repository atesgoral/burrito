{ Version 1.0.2002.04.06 }

unit ConfigurationMap;

interface

uses
  Classes, Registry, SysUtils, Windows;

const
  EXC_NAME = 'Mapping not found';

type
  TMapType = (mtInteger, mtString, mtBoolean);

  TMapEntry = record
    MapType: TMapType;
    P: Pointer;
    Modified: Boolean;
  end;

  PMapEntry = ^TMapEntry;

  TConfigurationMap = class
    constructor Create(const KeyPath: String; RootKey: HKEY); overload;
    constructor Create(const KeyPath: String); overload;
    destructor Destroy; override;
  private
    FRootKey: HKEY;
    FKeyPath: String;
    Map: TStringList;
    Modified: Boolean;
    procedure AddMapping(const Name: String; _P: Pointer; _MapType: TMapType); overload;
    procedure SetValue(const Name: String; _P: Pointer; _MapType: TMapType); overload;
  public
    property RootKey: HKEY read FRootKey write FRootKey;
    property KeyPath: String read FKeyPath write FKeyPath;

    procedure AddMapping(const Name: String; var V: Integer); overload;
    procedure AddMapping(const Name: String; var V: String); overload;
    procedure AddMapping(const Name: String; var V: Boolean); overload;

    procedure RemoveMapping(const Name: String);

    procedure SetValue(const Name: String; V: Integer); overload;
    procedure SetValue(const Name: String; const V: String); overload;
    procedure SetValue(const Name: String; V: Boolean); overload;

    procedure LoadValues;
    procedure SaveChanges;
  end;

implementation

{ TConfigurationMap }

constructor TConfigurationMap.Create(const KeyPath: String; RootKey: HKEY);
begin
  Map := TStringList.Create;
  FKeyPath := KeyPath;
  FRootKey := RootKey;
end;

constructor TConfigurationMap.Create(const KeyPath: String);
begin
  Create(KeyPath, HKEY_CURRENT_USER);
end;

destructor TConfigurationMap.Destroy;
var
  Idx: Integer;

begin
  for Idx := 0 to Map.Count - 1 do
    Dispose(PMapEntry(Map.Objects[Idx]));
  Map.Free;
  inherited;
end;

{ Mapping }

procedure TConfigurationMap.AddMapping(const Name: String; _P: Pointer; _MapType: TMapType);
var
  Idx: Integer;
  MapEntry: PMapEntry;

begin
  Idx := Map.IndexOf(Name);
  if (Idx <> -1) then
    raise Exception.Create('A mapping with that name already exists');
  New(MapEntry);
  with MapEntry^ do
    begin
      MapType := _MapType;
      P := _P;
      Modified := True;
    end;
  with Map do
    begin
      Append(Name);
      Objects[Count - 1] := TObject(MapEntry);
      Modified := True;
    end;
end;

procedure TConfigurationMap.AddMapping(const Name: String; var V: Integer);
begin
  AddMapping(Name, @V, mtInteger);
end;

procedure TConfigurationMap.AddMapping(const Name: String; var V: String);
begin
  AddMapping(Name, @V, mtString);
end;

procedure TConfigurationMap.AddMapping(const Name: String; var V: Boolean);
begin
  AddMapping(Name, @V, mtBoolean);
end;

procedure TConfigurationMap.RemoveMapping(const Name: String);
var
  Idx: Integer;

begin
  Idx := Map.IndexOf(Name);
  if (Idx = -1) then
    raise Exception.Create(EXC_NAME);
  Map.Delete(Idx);
end;

{ SetValue }

procedure TConfigurationMap.SetValue(const Name: String; _P: Pointer; _MapType: TMapType);
var
  Idx: Integer;

begin
  Idx := Map.IndexOf(Name);
  if (Idx = -1) then
    raise Exception.Create(EXC_NAME);
  with PMapEntry(Map.Objects[Idx])^ do
    begin
      if (MapType <> _MapType) then
        raise Exception.Create('Type mismatch');
      case _MapType of
        mtInteger: Modified := (PInteger(P)^ <> PInteger(_P)^);
        mtString: Modified := (PString(P)^ <> PString(_P)^);
        mtBoolean: Modified := (PBoolean(P)^ <> PBoolean(_P)^);
      end;
      if Modified then
        begin
          case _MapType of
            mtInteger: PInteger(P)^ := PInteger(_P)^;
            mtString: PString(P)^ := PString(_P)^;
            mtBoolean: PBoolean(P)^ := PBoolean(_P)^;
          end;
          Self.Modified := True;
        end;
    end;
end;

procedure TConfigurationMap.SetValue(const Name: String; V: Integer);
begin
  SetValue(Name, @V, mtInteger);
end;

procedure TConfigurationMap.SetValue(const Name, V: String);
begin
  SetValue(Name, @V, mtString);
end;

procedure TConfigurationMap.SetValue(const Name: String; V: Boolean);
begin
  SetValue(Name, @V, mtBoolean);
end;

{ Load / Save }

procedure TConfigurationMap.LoadValues;
var
  Reg: TRegistry;
  Idx: Integer;

begin
  Reg := TRegistry.Create;
  with Reg do
    try
      RootKey := FRootKey;
      if OpenKeyReadOnly(FKeyPath) then
        begin
          for Idx := 0 to Map.Count - 1 do
            with PMapEntry(Map.Objects[Idx])^ do
              if Reg.ValueExists(Map[Idx]) then
                begin
                  case MapType of
                    mtInteger: PInteger(P)^ := ReadInteger(Map[Idx]);
                    mtString: PString(P)^ := ReadString(Map[Idx]);
                    mtBoolean: PBoolean(P)^ := ReadBool(Map[Idx]);
                  end;
                  Modified := False;
                end;
          CloseKey;
        end;
    finally
      Free;
    end;
  Modified := False;
end;

procedure TConfigurationMap.SaveChanges;
var
  Reg: TRegistry;
  Idx: Integer;

begin
  if not Modified then
    Exit;
  Reg := TRegistry.Create;
  with Reg do
    try
      RootKey := FRootKey;
      if OpenKey(FKeyPath, True) then
        begin
          for Idx := 0 to Map.Count - 1 do
            with PMapEntry(Map.Objects[Idx])^ do
              begin
                case MapType of
                  mtInteger: WriteInteger(Map[Idx], PInteger(P)^);
                  mtString: WriteString(Map[Idx], PString(P)^);
                  mtBoolean: WriteBool(Map[Idx], PBoolean(P)^);
                end;
                Modified := False;
              end;
          CloseKey;
        end;
    finally
      Free;
    end;
  Modified := False;
end;

end.
