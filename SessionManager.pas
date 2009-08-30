unit SessionManager;

interface

uses
  Classes, SysUtils, Windows, SyncObjs, Messages, Math,
  IdPOP3, IdMessage,
  FilenameFormat;

const
  DEF_PORT = 110;
  DEF_USERSERVERSEP = '\';

  WM_LED = WM_USER + 123;

type
  TMsg = class
  public
    MsgIndex: Integer;
    MsgFilename: String;
    MsgSize: Int64;
    MsgDate: TDateTime;
    Data: TStringList;
  end;

  TMsgList = class(TList)
    destructor Destroy; override;
  private
    function MsgAt(Index: Integer): TMsg;
  public
    function CreateMsg: TMsg;
    procedure DestroyMsg(Msg: TMsg);
    function IndexOf(const Filename: String): Integer;
    property Messages[Index: Integer]: TMsg read MsgAt; default;
  end;

  TMailSession = class;
  TMailSessions = class;

  TFTPSession = class
    constructor Create;
    destructor Destroy; override;
  private
    Children: TMailSessions;
    FCurrentMailSession: TMailSession;
  public
    function OpenMailSessions: Boolean;
    property CurrentMailSession: TMailSession read FCurrentMailSession;
  end;

  TFTPSessions = class(TList)
  private
    function SessionAt(Index: Integer): TFTPSession;
  public
    property Sessions[Index: Integer]: TFTPSession read SessionAt; default;
  end;

  TMailAccount = record
    VirtualFolder: String;
    Host: String;
    Port: Word;
    Username, Password: String;
  end;

  TMailSession = class
    constructor Create(const Account: TMailAccount); virtual;
    destructor Destroy; override;
  private
    FAccount: TMailAccount;
    Parents: TFTPSessions;
    FMsgList: TMsgList;
    IsOpen: Boolean;
    function GetMsgList: TMsgList; virtual; abstract;
    procedure _Retrieve(Msg: TMsg); virtual; abstract;
    procedure _Delete(Msg: TMsg); virtual; abstract;
  public
    function Open: Boolean; virtual; abstract;
    procedure Close; virtual; abstract;
    function Retrieve(Idx: Integer): TStringList;
    procedure Delete(Idx: Integer);
    property Account: TMailAccount read FAccount;
    property MsgList: TMsgList read GetMsgList;
  end;

  TMailSessions = class(TList)
  private
    function SessionAt(Index: Integer): TMailSession;
  public
    property Sessions[Index: Integer]: TMailSession read SessionAt; default;
  end;

  TPOP3Session = class(TMailSession)
    constructor Create(const Account: TMailAccount); override;
  private
    POP3Cmp: TIdPOP3;
    function GetMsgList: TMsgList; override;
    procedure _Retrieve(Msg: TMsg); override;
    procedure _Delete(Msg: TMsg); override;
  public
    function Open: Boolean; override;
    procedure Close; override;
  end;

  TSessionManager = class
    constructor Create;
    destructor Destroy; override;
  private
    FTPSessions: TFTPSessions;
    MailSessions: TMailSessions;
    function GetFTPSessionCount: Integer;
    function GetMailSessionCount: Integer;
  public
    function CreateFTPSession(const User, Pass: String): TFTPSession;
    procedure DestroyFTPSession(FTPSession: TFTPSession);
    procedure Associate(FTPSession: TFTPSession; MailSession: TMailSession);
    procedure Dissociate(FTPSession: TFTPSession; MailSession: TMailSession);
    procedure Reset;
    property MailSessionCount: Integer read GetMailSessionCount;
    property FTPSessionCount: Integer read GetFTPSessionCount;
  end;

var
  Gbl_SM: TSessionManager;
  Gbl_UserServerSep: Char = DEF_USERSERVERSEP;
  Gbl_FilenameFmt: String;
  Gbl_Owner: TComponent = nil;

  // Hacky
  Gbl_LogLines: TStringList;
  Gbl_LogCS: TCriticalSection;

  Gbl_LedHnd: HWND;

implementation

{ Utils }

procedure Split(const S: String; Separator: Char; var First, Second: String);
var
  SepPos: Integer;

begin
  SepPos := Pos(Separator, S);
  if (SepPos > 0) then
    begin
      First := Copy(S, 1, SepPos - 1);
      Second := Copy(S, SepPos + 1, MaxInt);
    end
  else
    begin
      First := S;
      Second := '';
    end;
end;

function ParseAccount(const User, Pass: String): TMailAccount;
var
  ServerPart, PortPart: String;
  IntPort, Code: Integer;

begin
  with Result do
    begin
      Split(User, Gbl_UserServerSep, Username, ServerPart);
      // check validity
      Split(ServerPart, ':', Host, PortPart);

      if (Length(PortPart) = 0) then
        IntPort := DEF_PORT
      else
        begin
          Val(PortPart, IntPort, Code);
          if (Code <> 0) or (IntPort < 0) or (IntPort > 65535) then
            raise Exception.Create('Invalid port: ' + PortPart);
        end;
      Username := Lowercase(Username);
      Host := Lowercase(Host);
      Port := IntPort;
      Password := Pass;
    end;
end;

function AccountsMatch(const Acc1, Acc2: TMailAccount): Boolean;
begin
  Result := (Acc1.Host = Acc2.Host) and (Acc1.Port = Acc2.Port) and
    (Acc1.Username = Acc2.Username) and (Acc1.Password = Acc2.Password);
end;

{ Logging }

procedure Log(const S: String);
begin
  Gbl_LogCS.Enter;
  Gbl_LogLines.Append(FormatDateTime('hh:nn:ss ', Time) + S);
  Gbl_LogCS.Leave;
end;

{ Led }

procedure FlashLed;
begin
  PostMessage(Gbl_LedHnd, WM_LED, 0, 0);
end;

{ TMsgList }

destructor TMsgList.Destroy;
var
  Idx: Integer;

begin
  for Idx := 0 to Count - 1 do
    MsgAt(Idx).Free;
//    Self[Idx].Free; // ?
  inherited Destroy;
end;

function TMsgList.MsgAt(Index: Integer): TMsg;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('Index out of bounds');
  Result := TMsg(Items[Index]);
end;

function TMsgList.IndexOf(const Filename: String): Integer;
begin
  Result := 0;
  while (Result < Count) and (Messages[Result].MsgFilename <> Filename) do
    inc(Result);
  if (Result = Count) then
    Result := -1;
end;

function TMsgList.CreateMsg: TMsg;
begin
  Result := TMsg.Create;
  Add(Result);
end;

procedure TMsgList.DestroyMsg(Msg: TMsg);
begin
  Remove(Msg);
  Msg.Data.Free;
  Msg.Free;
end;

{ TFTPSession }

constructor TFTPSession.Create;
begin
  Children := TMailSessions.Create;
  FCurrentMailSession := nil;
end;

destructor TFTPSession.Destroy;
begin
  Children.Free;
  inherited;
end;

function TFTPSession.OpenMailSessions: Boolean;
var
  Idx: Integer;
  MailSession: TMailSession;

begin
  Result := False;
  Idx := Children.Count;
  while (Idx > 0) do
    begin
      dec(Idx);
      MailSession := Children[Idx];
      FlashLed;
      if MailSession.Open then
        Result := True
      else
        Gbl_SM.Dissociate(Self, MailSession);
    end;
end;

{ TFTPSessions }

function TFTPSessions.SessionAt(Index: Integer): TFTPSession;
begin
  Result := TFTPSession(Items[Index]);
end;

{ TMailSession }

constructor TMailSession.Create(const Account: TMailAccount);
begin
  FAccount := Account;
  Parents := TFTPSessions.Create;
  IsOpen := False;
  FMsgList := nil;
end;

destructor TMailSession.Destroy;
begin
  Close;
  Parents.Free;
  FMsgList.Free;
  inherited;
end;

function TMailSession.Retrieve(Idx: Integer): TStringList;
var
  Msg: TMsg;

begin
  Msg := MsgList[Idx];
  if (Msg.Data = nil) then
    begin
      Msg.Data := TStringList.Create;
      _Retrieve(Msg);
    end;
  Result := Msg.Data;
end;

procedure TMailSession.Delete(Idx: Integer);
var
  Msg: TMsg;

begin
  Msg := MsgList[Idx];
  _Delete(Msg);
  MsgList.DestroyMsg(Msg);
end;

{ TMailSessions }

function TMailSessions.SessionAt(Index: Integer): TMailSession;
begin
  Result := TMailSession(Items[Index]);
end;

{ TPOP3Session }

constructor TPOP3Session.Create(const Account: TMailAccount);
begin
  POP3Cmp := nil;
  inherited;
end;

function TPOP3Session.Open: Boolean;
begin
  Result := True;
  if not IsOpen then
    begin
      POP3Cmp := TIdPOP3.Create(Gbl_Owner);
      POP3Cmp.Host := FAccount.Host;
      POP3Cmp.Port := FAccount.Port;
      POP3Cmp.Username := FAccount.Username;
      POP3Cmp.Password := FAccount.Password;
//      POP3Cmp.OnDisconnected := 
      try
        Log('Opening POP3 connection...');
        FlashLed;
        POP3Cmp.Connect;
        Log('POP3 connection established');
        IsOpen := True;
      except
        on E: Exception do
          begin
            Log('POP3 connection failed: ' + Trim(E.Message));
            Result := False;
          end;
      end;
    end;
end;

procedure TPOP3Session.Close;
begin
  if IsOpen then
    begin
      POP3Cmp.Disconnect;
      Log('POP3 connection closed');
      POP3Cmp.Free;
      IsOpen := False;
    end;
end;

function TPOP3Session.GetMsgList: TMsgList;
var
  MsgCnt, MsgIdx, MsgsLeft: Integer;
  IdMsg: TIdMessage;
  Plural: String;
  FileName, DupFileName: String;
  PerPos, DupCnt, Prec: Integer;
  Fmt, FilePart, ExtPart: String;

begin
  if (FMsgList = nil) then
    begin
      FlashLed;
      MsgCnt := POP3Cmp.CheckMessages;
      FMsgList := TMsgList.Create;
      if (MsgCnt > 0) then
        begin
          if (MsgCnt > 1) then
            Plural := 's'
          else
            Plural := '';
          Log(Format('Parsing %d message%s...', [MsgCnt, Plural]));
          for MsgIdx := 1 to MsgCnt do
            begin
              FlashLed;
              IdMsg := TIdMessage.Create(Gbl_Owner);
              POP3Cmp.RetrieveHeader(MsgIdx, IdMsg);

              DupCnt := 0;
              FileName := GenerateFilename(Gbl_FilenameFmt, Trim(IdMsg.From.Name),
                Trim(IdMsg.From.Address), Trim(IdMsg.Subject), MsgIdx, MsgCnt);

              if (FMsgList.IndexOf(FileName) <> -1) then
                begin
                  PerPos := Length(FileName);
                  while (PerPos > 0) and (FileName[PerPos] <> '.') do
                    dec(PerPos);
                  if (PerPos > 0) then
                    begin
                      FilePart := Copy(FileName, 1, PerPos - 1);
                      ExtPart := Copy(FileName, PerPos, MaxInt);
                    end
                  else
                    begin
                      FilePart := FileName;
                      ExtPart := '';
                    end;
                  Prec := Trunc(Log10(MsgCnt - 1)) + 1;
                  Fmt := Format('%%s(%%.%dd)%%s', [Prec]);
                  repeat
                    Inc(DupCnt);
                    DupFileName := Format(Fmt, [FilePart, DupCnt, ExtPart]);
                  until (FMsgList.IndexOf(DupFileName) = -1);
                  FileName := DupFileName;
                end;

              with FMsgList.CreateMsg do
                begin
                  MsgIndex := MsgIdx;
                  MsgFilename := FileName;
                  MsgSize := POP3Cmp.RetrieveMsgSize(MsgIdx);
                  MsgDate := IdMsg.Date;
                  Data := nil;
                end;
              IdMsg.Free;
              MsgsLeft := MsgCnt - MsgIdx;
              if (MsgsLeft > 0) and (MsgsLeft and $1f = 0) then
                Log(Format('%d messages left', [MsgsLeft]));
            end;
          Log('Parsing complete');
        end
      else
        Log('No messages');
    end;
  Result := FMsgList;
end;

procedure TPOP3Session._Retrieve(Msg: TMsg);
begin
  FlashLed;
  POP3Cmp.RetrieveRaw(Msg.MsgIndex, Msg.Data);
end;

procedure TPOP3Session._Delete(Msg: TMsg);
begin
  FlashLed;
  POP3Cmp.Delete(Msg.MsgIndex);
end;

{ TSessionManager }

constructor TSessionManager.Create;
begin
  FTPSessions := TFTPSessions.Create;
  MailSessions := TMailSessions.Create;
end;

destructor TSessionManager.Destroy;
begin
// Remove all sessions first!
  FTPSessions.Free;
  MailSessions.Free;
  inherited Destroy;
end;

function TSessionManager.CreateFTPSession(const User, Pass: String): TFTPSession;
var
  Account: TMailAccount;
  Scan, MailSession: TMailSession;
  Idx: Integer;

begin
  Result := TFTPSession.Create;
  FTPSessions.Add(Result);
  // if User is an alias for multiple accounts then do this for all accounts
  Account := ParseAccount(User, Pass);

  Idx := 0;
  MailSession := nil;
  while (Idx < MailSessions.Count) do
    begin
      Scan := MailSessions[Idx];
      if AccountsMatch(Scan.Account, Account) then
        begin
          MailSession := Scan;
          Break;
        end;
      inc(Idx);
    end;

  if not Assigned(MailSession) then
    begin
      MailSession := TPOP3Session.Create(Account); // hard-coded POP3
      MailSessions.Add(MailSession);
    end;

  Associate(Result, MailSession);

  // hack
  Result.FCurrentMailSession := MailSession;
end;

procedure TSessionManager.DestroyFTPSession(FTPSession: TFTPSession);
var
  Idx: Integer;

begin
  for Idx := 0 to FTPSession.Children.Count - 1 do
    Dissociate(FTPSession, FTPSession.Children[Idx]);
  FTPSessions.Remove(FTPSession);
  FTPSession.Free;
end;

procedure TSessionManager.Associate(FTPSession: TFTPSession; MailSession: TMailSession);
begin
  FTPSession.Children.Add(MailSession);
  MailSession.Parents.Add(FTPSession);
end;

procedure TSessionManager.Dissociate(FTPSession: TFTPSession; MailSession: TMailSession);
begin
  FTPSession.Children.Remove(MailSession);
  MailSession.Parents.Remove(FTPSession);
  if (MailSession.Parents.Count = 0) then // Can't survive without parents... :(
    begin
      MailSessions.Remove(MailSession);
      MailSession.Free;
    end;
end;

function TSessionManager.GetFTPSessionCount: Integer;
begin
  Result := FTPSessions.Count;
end;

function TSessionManager.GetMailSessionCount: Integer;
begin
  Result := MailSessions.Count;
end;

procedure TSessionManager.Reset;
begin
  while (FTPSessions.Count > 0) do
    DestroyFTPSession(FTPSessions.SessionAt(0));
end;

{ Global session manager object }

initialization
  Gbl_SM := TSessionManager.Create;
  Gbl_LogLines := TStringList.Create;
  Gbl_LogCS := TCriticalSection.Create;

finalization
  Gbl_SM.Free;
  Gbl_LogLines.Free;
  Gbl_LogCS.Free;

end.
