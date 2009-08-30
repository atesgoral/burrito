unit Config;

interface

uses
  Classes;

type
  TBinding = record
    Host: String;
    Port: Integer;
  end;

  PBinding = ^TBinding;

procedure SaveConfig;
procedure SavePosition(Left, Top: Integer);

procedure AddBinding(const Host: String; Port: Integer);
procedure RemoveBinding(Binding: PBinding);

var
  { Global configuration data }
  Cfg_WindowLeft: Integer = -1;
  Cfg_WindowTop: Integer;

  Cfg_ManualBindings: Boolean = False;
  Cfg_Bindings: TList = nil;

  { Change flags }
  Chg_WindowLeft: Boolean = False;
  Chg_WindowTop: Integer;

  Chg_ManualBindings: Boolean = False;
  Chg_Bindings: Boolean;

implementation

uses
  Registry;

const
  KEYPATH = '\Software\Magnetiq\Burrito';

  RN_WINDOWLEFT = 'WindowLeft';
  RN_WINDOWTOP = 'WindowTop';
  RN_MANUALBINDINGS = 'ManualBindings';

  RN_HOST = 'Host';
  RN_PORT = 'Port';

  KN_BINDINGS = '\Bindings';

procedure LoadConfig;
var
  Reg: TRegistry;
  Path: String;
  Bindings: TStringList;
  Idx: Integer;

begin
  Reg := TRegistry.Create;
  with Reg do
    try
      if OpenKeyReadOnly(KEYPATH) then
        begin
          Cfg_WindowLeft := ReadInteger(RN_WINDOWLEFT);
          Cfg_WindowTop := ReadInteger(RN_WINDOWTOP);

          CloseKey;
          Path := KEYPATH + KN_BINDINGS;
          if OpenKeyReadOnly(Path) then
            begin
              Bindings := TStringList.Create;
              GetKeyNames(Bindings);
              CloseKey;
              for Idx := 0 to Bindings.Count - 1 do
                if OpenKeyReadOnly(Path + '\' + Bindings[Idx]) then
                  begin
                    AddBinding(ReadString(RN_HOST), ReadInteger(RN_PORT));
                    CloseKey;
                  end;
            end;
        end;
    finally
      Free;
    end;
end;

procedure SaveConfig;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create;
  with Reg do
    try
      if OpenKey(KEYPATH, True) then
        begin
          Cfg_ManualBindings := ReadBool(RN_MANUALBINDINGS);
          CloseKey;
        end;
    finally
      Free;
    end;
end;

procedure SavePosition(Left, Top: Integer);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(KEYPATH, True) then
      begin
        Reg.WriteInteger(RN_WINDOWLEFT, Left);
        Reg.WriteInteger(RN_WINDOWTOP, Top);
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;

procedure AddBinding(const Host: String; Port: Integer);
var
  Binding: PBinding;

begin
  New(Binding);
  Binding.Host := Host;
  Binding.Port := Port;
  Cfg_Bindings.Add(Binding);
end;

procedure RemoveBinding(Binding: PBinding);
begin
  Cfg_Bindings.Remove(Binding);
end;

initialization
  Cfg_Bindings := TList.Create;
  LoadConfig;

finalization
  with Cfg_Bindings do
    begin
      while (Count > 0) do
        begin
          Dispose(Items[Count - 1]);
          Cfg_Bindings.Delete(Count - 1);
        end;
      Free;
    end;

end.
