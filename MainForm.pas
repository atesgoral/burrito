unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Registry, ShellAPI,
  Dialogs, StdCtrls, IdTCPConnection, IdTCPClient, IdMessageClient,
  IdBaseComponent, IdComponent, IdTCPServer, IdFTPServer, IdFTPList,
  IdThreadMgr, IdThreadMgrPool, IdSocketHandle, IdGlobal, ComCtrls, ExtCtrls,
  SessionManager, FilenameFormat, JvTrayIcon, Menus, JvComponent;

const
  FORM_CAPTION = 'Burrito 1.0b';

  WM_SHOWSELF = WM_USER + 333;

  MAX_LOGLINES = 500;

  KEYPATH = '\Software\Magnetiq\Burrito';

  RN_WINDOWLEFT = 'WindowLeft';
  RN_WINDOWTOP = 'WindowTop';

  RN_COOKIE = 'Cookie';

  RN_USERSERVERSEP = 'UserServerSeparator';

  RN_FILENAMEFMT = 'FilenameFormat';

  RN_LISTENMODE = 'ListenMode';

  RN_BINDINGS = 'Bindings';

  LMS_ALL = 'ALL';
  LMS_LOCAL = 'LOCAL';
  LMS_MANUAL = 'MANUAL';

  SMP_USER = 'ates';
  SMP_SERVER = 'mail.magnetiq.com';
  SMP_NAME = 'Ates Goral';
  SMP_ADDR = 'ates@magnetiq.com';
  SMP_SUBJ = 'Hello there!';
  SMP_IDX = 3;

  CHECK_URL_FMT = 'http://www.magnetiq.com/burrito/checkver.php?v=%s&b=%s&c=%s';
  VERSION = '1.0.b';
  BUILD = '2002.10.17.23.48';

  COOKIE_LEN = 32;

  LED_INT = 150;

type
  TListenMode = (lmAll, lmLocal, lmManual);

  TFormMain = class(TForm)
    FTPS: TIdFTPServer;
    PageControl1: TPageControl;
    TabSheetOpt: TTabSheet;
    TabSheetSecurity: TTabSheet;
    TabSheetAbout: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ImageLogo: TImage;
    Label1: TLabel;
    StaticText2: TStaticText;
    PanelFTPSess: TPanel;
    TabSheetAliases: TTabSheet;
    BevelSepBindings: TBevel;
    ImageSqr: TImage;
    BevelSepAbout: TBevel;
    MemoLog: TMemo;
    PanelMailSess: TPanel;
    PanelLed: TPanel;
    ButtonCheckUpd: TButton;
    Bevel4: TBevel;
    BevelSepMisc: TBevel;
    BevelSepAlias: TBevel;
    TimerDisp: TTimer;
    TreeViewAliases: TTreeView;
    LabelAliases: TLabel;
    ButtonAddAlias: TButton;
    ButtonRemoveAlias: TButton;
    MemoInfo: TMemo;
    TrayIcon: TJvTrayIcon;
    PopupMenuMain: TPopupMenu;
    MenuItemOpen: TMenuItem;
    MenuItemSep: TMenuItem;
    MenuItemShutdown: TMenuItem;
    TimerLed: TTimer;
    GroupBox1: TGroupBox;
    RadioButtonAll: TRadioButton;
    RadioButtonLocal: TRadioButton;
    RadioButtonManual: TRadioButton;
    LabelListenHlp: TLabel;
    GroupBox2: TGroupBox;
    ListViewBindings: TListView;
    ButtonAddBinding: TButton;
    ButtonRemoveBinding: TButton;
    GroupBox3: TGroupBox;
    LabelSep: TLabel;
    EditSep: TEdit;
    LabelUserSmp: TLabel;
    EditUserSmp: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    ComboBoxFmt: TComboBox;
    LabelFileSmp: TLabel;
    EditFileSmp: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;

    procedure FTPSUserLogin(ASender: TIdFTPServerThread; AUsername,
      APassword: String; var AAuthenticated: Boolean);
    procedure FTPSListDirectory(ASender: TIdFTPServerThread;
      APath: String; ADirectoryListing: TIdFTPListItems);
    procedure FTPSDisconnect(AThread: TIdPeerThread);
    procedure FTPSRetrieveFile(ASender: TIdFTPServerThread;
      AFilename: String; var VStream: TStream);
    procedure FTPSDeleteFile(ASender: TIdFTPServerThread;
      const APathname: String);
    procedure FTPSStoreFile(ASender: TIdFTPServerThread;
      AFilename: String; AAppend: Boolean; var VStream: TStream);
    procedure FTPSChangeDirectory(ASender: TIdFTPServerThread;
      var VDirectory: String);
    procedure FTPSConnect(AThread: TIdPeerThread);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure EditSepChange(Sender: TObject);
    procedure ButtonAddBindingClick(Sender: TObject);
    procedure TimerDispTimer(Sender: TObject);
    procedure ListViewBindingsClick(Sender: TObject);
    procedure ButtonAddFormatClick(Sender: TObject);
    procedure MenuItemShutdownClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TrayIconDblClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonCheckUpdClick(Sender: TObject);
    procedure TimerLedTimer(Sender: TObject);
    procedure ListViewBindingsDblClick(Sender: TObject);
    procedure ComboBoxFmtChange(Sender: TObject);
    procedure RadioButtonListenModeClick(Sender: TObject);
    procedure PopupMenuMainPopup(Sender: TObject);
    procedure MemoLogDblClick(Sender: TObject);
    procedure ButtonRemoveBindingClick(Sender: TObject);
  private
    procedure FashionBindingsPage;
    procedure Log(const S: String);
    procedure WMLed(var Message: TWMChar); message WM_LED;
    procedure AddBinding(const IP: String; Port: Integer);
    function AllowChangeBindings: Boolean;
    function SetFTPActive(Active: Boolean): Boolean;
    procedure ApplicationMinimize(Sender: TObject);
    procedure ApplicationRestore(Sender: TObject);
    procedure AddBindings;
    procedure SaveBindings;
    procedure SelectListenModeRadio;
  protected
    procedure WMShowSelf(var Msg: TMessage); message WM_SHOWSELF;
  end;

var
  FormMain: TFormMain;
  Connections: TList;

implementation

{$R *.dfm}

uses
  ConfigurationMap, BindingForm, LogForm;

var
  ConfigMap: TConfigurationMap = nil;

  ListenMode: TListenMode;

  IsShuttingDown: Boolean = False;

  Cfg_WindowLeft: Integer = -1;
  Cfg_WindowTop: Integer;

  Cfg_Cookie: String;

  Cfg_UserServerSep: String;

  Cfg_ListenMode: String;

  Cfg_Bindings: String;

function GenerateCookie: String;
var
  GUID: TGUID;
  GUIDStr: String;
  Len, Idx: Integer;
  Ch: Char;

begin
  CreateGUID(GUID);
  GUIDStr := GUIDToString(GUID);
  Result := '';
  Len := Length(GUIDStr);
  for Idx := 1 to Len do
    begin
      Ch := GUIDStr[Idx];
      if not (Ch in ['{', '}', '-']) then
        Result := Result + Ch;
    end;
end;

procedure TFormMain.Log(const S: String);
var
  Cnt: Integer;

begin
  if not IsShuttingDown then
    begin
      with MemoLog.Lines do
        begin
          BeginUpdate;
          Append(FormatDateTime('hh:nn:ss ', Time) + S);
          if (Count > MAX_LOGLINES) then
            for Cnt := 1 to Trunc(MAX_LOGLINES / 10) do
              Delete(0);
          EndUpdate;
        end;
      PostMessage(MemoLog.Handle, EM_SCROLL, SB_PAGEDOWN, 0);
    end;
end;

{ FTP events }

procedure TFormMain.FTPSConnect(AThread: TIdPeerThread);
begin
  Log('FTP client connected');
  Connections.Add(AThread.Connection);
end;

procedure TFormMain.FTPSUserLogin(ASender: TIdFTPServerThread;
  AUsername, APassword: String; var AAuthenticated: Boolean);
var
  FTPSession: TFTPSession;

begin
  FTPSession := GBL_SM.CreateFTPSession(AUsername, APassword);
  AAuthenticated := FTPSession.OpenMailSessions;
  if AAuthenticated then
    ASender.Data := FTPSession
  else
    begin
      Gbl_SM.DestroyFTPSession(FTPSession);
    end;
end;

procedure TFormMain.FTPSListDirectory(ASender: TIdFTPServerThread;
  APath: String; ADirectoryListing: TIdFTPListItems);
const
  PERMISSIONS = 'r--';

var
  FTPSession: TFTPSession;
  MailSession: TMailSession;
  MsgList: TMsgList;
  Item: TIdFTPListItem;
  MsgIdx: Integer;

begin
  // APath parameter = '/'
  FTPSession := TFTPSession(ASender.Data);
  MailSession := FTPSession.CurrentMailSession;
  MsgList := MailSession.MsgList;
  for MsgIdx := 0 to MsgList.Count - 1 do
    begin
      Item := TIdFTPListItem.Create(ADirectoryListing);
      with Item, MsgList[MsgIdx] do
        begin
          ItemType := ditFile;
          FileName := MsgFileName;
          Size := MsgSize;
          ModifiedDate := MsgDate;
          OwnerPermissions := PERMISSIONS;
          GroupPermissions := PERMISSIONS;
          UserPermissions := PERMISSIONS;
          ItemCount := 1;
          OwnerName := MailSession.Account.Username;
          GroupName := OwnerName;
        end;
    end;
end;

procedure TFormMain.FTPSRetrieveFile(ASender: TIdFTPServerThread;
  AFilename: String; var VStream: TStream);
var
  FTPSession: TFTPSession;
  MailSession: TMailSession;
  MsgList: TMsgList;
  MsgIdx: Integer;
  Raw: TStringList;

begin
  FTPSession := TFTPSession(ASender.Data);
  MailSession := FTPSession.CurrentMailSession;
  MsgList := MailSession.MsgList;
  MsgIdx := MsgList.IndexOf(Copy(AFilename, 2, MaxInt));
  if (MsgIdx <> -1) then
    begin
      Raw := MailSession.Retrieve(MsgIdx);
      VStream := TStringStream.Create(Raw.Text);
    end
  else
    raise Exception.Create('File not found'); // What happens then?
end;

procedure TFormMain.FTPSDeleteFile(ASender: TIdFTPServerThread;
  const APathname: String);
var
  FTPSession: TFTPSession;
  MailSession: TMailSession;
  MsgList: TMsgList;
  MsgIdx: Integer;

begin
  FTPSession := TFTPSession(ASender.Data);
  MailSession := FTPSession.CurrentMailSession;
  MsgList := MailSession.MsgList;
  MsgIdx := MsgList.IndexOf(APathname);
  if (MsgIdx <> -1) then
    MailSession.Delete(MsgIdx)
  else
    raise Exception.Create('File not found'); // What happens then?
end;

procedure TFormMain.FTPSStoreFile(ASender: TIdFTPServerThread;
  AFilename: String; AAppend: Boolean; var VStream: TStream);
begin
// Not implemented; keep this comment
end;

procedure TFormMain.FTPSChangeDirectory(ASender: TIdFTPServerThread;
  var VDirectory: String);
begin
  VDirectory := '/';
end;

procedure TFormMain.FTPSDisconnect(AThread: TIdPeerThread);
var
  FTPSession: TFTPSession;

begin
  Log('FTP client disconnected');
  Connections.Remove(AThread.Connection);
  FTPSession := TFTPSession(AThread.Data);
  if Assigned(FTPSession) then
    begin
      Gbl_SM.DestroyFTPSession(FTPSession);
      AThread.Data := nil;
    end;
end;

{ ********************** }

procedure TFormMain.SelectListenModeRadio;
begin
  case ListenMode of
    lmAll: RadioButtonAll.Checked := True;
    lmLocal: RadioButtonLocal.Checked := True;
    lmManual: RadioButtonManual.Checked := True;
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  if (Tag = 0) then
    begin
      Tag := 1;
      Caption := FORM_CAPTION;
      MemoLog.SetFocus;
      FTPS.Greeting.Text.Text := 'Version ' + VERSION + ' build ' + BUILD +
        '. Your Burrito is ready!';
      if (Cfg_WindowLeft <> -1) then
        begin
          Left := Cfg_WindowLeft;
          Top := Cfg_WindowTop;
        end;
      EditSep.Text := Cfg_UserServerSep;

      SelectListenModeRadio;

      FashionBindingsPage;

      Gbl_Owner := Self;
      Gbl_LedHnd := Handle;

      case ListenMode of
        lmAll: AddBinding(IP_ALL, DEFAULT_PORT);
        lmLocal: AddBinding(IP_LOCAL, DEFAULT_PORT);
        lmManual: AddBindings;
      end;

      try
        if SetFTPActive(True) then
          Log('Burrito ready for serving!');
      except
        on E: Exception do Log('Error: ' + E.Message);
      end;
    end;
end;

procedure TFormMain.FashionBindingsPage;
begin
  case ListenMode of
    lmAll: LabelListenHlp.Caption := 'All incoming FTP connections from all computers are accepted. The default FTP port is used.';
    lmLocal: LabelListenHlp.Caption := 'Only local connections are accepted. Other machines on the same network or on the Internet cannot connect to Burrito. Listens on 127.0.0.1 aka "localhost".';
    lmManual: LabelListenHlp.Caption := 'Configure the IP and port bindings manually. This setting allows you to specify non-standard ports in case you already have an FTP server running on this machine.';
  end;

  ListViewBindings.Enabled := (ListenMode = lmManual);
  ButtonAddBinding.Enabled := ListViewBindings.Enabled;
  ButtonRemoveBinding.Enabled := ListViewBindings.Enabled and (ListViewBindings.ItemIndex <> -1);

  if ListViewBindings.Enabled then
    ListViewBindings.Color := clWindow
  else
    ListViewBindings.Color := clBtnFace;
end;

procedure TFormMain.EditSepChange(Sender: TObject);
begin
  if (Length(EditSep.Text) = 1) then
    begin
      ConfigMap.SetValue(RN_USERSERVERSEP, EditSep.Text);
      ConfigMap.SaveChanges;
      Gbl_UserServerSep := Cfg_UserServerSep[1];
      EditUserSmp.Text := SMP_USER + Gbl_UserServerSep + SMP_SERVER;
    end;
end;

procedure TFormMain.AddBinding(const IP: String; Port: Integer);
var
  Binding: TIdSocketHandle;

begin
  Binding := TIdSocketHandle.Create(FTPS.Bindings);
  Binding.IP := IP;
  Binding.Port := Port;
  with ListViewBindings.Items.Add do
    begin
      Data := Binding;
      Caption := IP;
      SubItems.Text := IntToStr(Port);
    end;
end;

procedure TFormMain.AddBindings;
var
  Bindings: TStringList;
  Idx, ColPos, Len, Port, Code: Integer;
  S, IP: String;

begin
  Bindings := TStringList.Create;
  Bindings.CommaText := Cfg_Bindings;
  for Idx := 0 to Bindings.Count - 1 do
    begin
      S := Bindings[Idx];
      ColPos := Pos(':', S);
      Len := Length(S);
      if (ColPos > 7) and (ColPos < Len) then
        begin
          Val(Copy(S, ColPos + 1, Len - ColPos), Port, Code);
          if (Code = 0) then
            begin
              IP := Copy(S, 1, ColPos -1);
              if IsValidIP(IP) then
                AddBinding(IP, Port);
            end;
        end;
    end;
  Bindings.Free;
end;

procedure TFormMain.ButtonAddBindingClick(Sender: TObject);
var
  SockHnd: TIdSocketHandle;
  ListItem: TListItem;

begin
  FormBinding := TFormBinding.Create(Self);
  if (ListViewBindings.ItemIndex = -1) then
    ListItem := nil
  else
    begin
      ListItem := ListViewBindings.Items[ListViewBindings.ItemIndex];
      SockHnd := TIdSocketHandle(ListItem.Data);
      FormBinding.SetEdit(SockHnd.IP, SockHnd.Port);
    end;
  if (FormBinding.ShowModal = mrOK) and AllowChangeBindings and SetFTPActive(False) then
    begin
      if (ListItem = nil) then
        begin
          ListItem := ListViewBindings.Items.Add;
          SockHnd := TIdSocketHandle.Create(FTPS.Bindings);
          ListItem.Data := SockHnd;
        end;
      ListItem.Caption := FormBinding.ComboBoxIP.Text;
      ListItem.SubItems.Text := FormBinding.EditPort.Text;
      SockHnd.IP := ListItem.Caption;
      SockHnd.Port := FormBinding.Port;

      SetFTPActive(True);
    end;
  FormBinding.Free;
end;

procedure TFormMain.TimerDispTimer(Sender: TObject);
begin
  PanelFTPSess.Caption := IntToStr(Gbl_SM.FTPSessionCount);
  PanelMailSess.Caption := IntToStr(Gbl_SM.MailSessionCount);
  if (Gbl_LogLines.Count > 0) and not IsShuttingDown then
    begin
      with MemoLog.Lines do
        begin
          BeginUpdate;
          Gbl_LogCS.Enter;
          AddStrings(Gbl_LogLines);
          Gbl_LogLines.Clear;
          Gbl_LogCS.Leave;
          EndUpdate;
        end;
      PostMessage(MemoLog.Handle, EM_SCROLL, SB_PAGEDOWN, 0);
    end;
end;

procedure TFormMain.ListViewBindingsClick(Sender: TObject);
begin
  ButtonRemoveBinding.Enabled := (ListViewBindings.ItemIndex <> -1);
  if ButtonRemoveBinding.Enabled then
    ButtonAddBinding.Caption := 'Edit...'
  else
    ButtonAddBinding.Caption := 'Add...';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Connections.Free;
  ConfigMap.Free;
end;

procedure TFormMain.ButtonAddFormatClick(Sender: TObject);
begin
end;

{ Application events }

procedure TFormMain.ApplicationMinimize(Sender: TObject);
begin
  TrayIcon.HideApplication;
end;

procedure TFormMain.ApplicationRestore(Sender: TObject);
begin
  TrayIcon.ShowApplication;
end;

{ Form events }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnMinimize := ApplicationMinimize;
  Application.OnRestore := ApplicationRestore;

  TimerLed.Interval := LED_INT;

  ConfigMap := TConfigurationMap.Create(KEYPATH);
  with ConfigMap do
    begin
      AddMapping(RN_WINDOWLEFT, Cfg_WindowLeft);
      AddMapping(RN_WINDOWTOP, Cfg_WindowTop);
      AddMapping(RN_COOKIE, Cfg_Cookie);
      AddMapping(RN_USERSERVERSEP, Cfg_UserServerSep);
      AddMapping(RN_LISTENMODE, Cfg_ListenMode);
      AddMapping(RN_FILENAMEFMT, Gbl_FilenameFmt);
      AddMapping(RN_BINDINGS, Cfg_Bindings);
      LoadValues;
      if (Length(Cfg_Cookie) < COOKIE_LEN) then
        SetValue(RN_COOKIE, GenerateCookie);
      if (Length(Cfg_UserServerSep) <> 1) then
        SetValue(RN_USERSERVERSEP, DEF_USERSERVERSEP);
      if (Length(Gbl_FilenameFmt) = 0) then
        SetValue(RN_FILENAMEFMT, ComboBoxFmt.Items[0]);

      if (Cfg_ListenMode = LMS_LOCAL) then
        ListenMode := lmLocal
      else if (Cfg_ListenMode = LMS_MANUAL) then
        ListenMode := lmManual
      else
        begin
          ListenMode := lmAll;
          SetValue(RN_LISTENMODE, LMS_ALL);
        end;

      ComboBoxFmt.Text := Gbl_FilenameFmt;
      ComboBoxFmtChange(Self);

      Gbl_UserServerSep := Cfg_UserServerSep[1];

      SaveChanges;
    end;

  Connections := TList.Create;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if IsShuttingDown then
    begin
      TimerDisp.Enabled := False;
      TimerLed.Enabled := False;
      with ConfigMap do
        begin
          SetValue(RN_WINDOWLEFT, Left);
          SetValue(RN_WINDOWTOP, Top);
          SaveChanges;
        end;
      Application.OnMinimize := nil;
      Application.OnRestore := nil;
      SetFTPActive(False);
    end
  else
    begin
      Application.Minimize;
      Action := caNone;
    end;
end;

procedure TFormMain.ButtonCheckUpdClick(Sender: TObject);
var
  URL: String;

begin
  URL := Format(CHECK_URL_FMT, [VERSION, BUILD, Cfg_Cookie]);
  ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

{ Tray icon actions }

procedure TFormMain.TrayIconMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  with Gbl_SM do
    TrayIcon.Hint := Format('Burrito [%d] [%d]', [FTPSessionCount, MailSessionCount]);
end;

procedure TFormMain.TrayIconDblClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MenuItemOpenClick(Sender);
end;

{ Tray icon pop-up }

procedure TFormMain.MenuItemShutdownClick(Sender: TObject);
begin
  if (Gbl_SM.FTPSessionCount > 0) then
    IsShuttingDown := (MessageDlg('There are active FTP connections.' + #10#10 +'Shutdown anyway?',
      mtConfirmation, mbOKCancel, 0) = mrOk)
  else
    IsShuttingDown := True;
  if IsShuttingDown then
    Close;
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
begin
  Application.Restore;
  Application.BringToFront;
end;

procedure TFormMain.WMLed(var Message: TWMChar);
begin
  if TimerLed.Enabled then
    TimerLed.Interval := LED_INT
  else
    begin
      PanelLed.Color := clRed;
      TimerLed.Enabled := True;
    end;
end;

procedure TFormMain.TimerLedTimer(Sender: TObject);
begin
  PanelLed.Color := clMaroon;
  TimerLed.Enabled := False;
end;

procedure TFormMain.ListViewBindingsDblClick(Sender: TObject);
begin
  if (ListViewBindings.ItemIndex <> -1) then
    ButtonAddBindingClick(Self);
end;

procedure TFormMain.ComboBoxFmtChange(Sender: TObject);
begin
  try
    EditFileSmp.Text := GenerateFilename(ComboBoxFmt.Text, SMP_NAME, SMP_ADDR, SMP_SUBJ, SMP_IDX,
      1);
    ConfigMap.SetValue(RN_FILENAMEFMT, ComboBoxFmt.Text);
    ConfigMap.SaveChanges;
    // Delayed write
  except
    on E: Exception do EditFileSmp.Text := 'ERROR: ' + E.Message;
  end;
  EditFileSmp.Hint := EditFileSmp.Text;
end;

function TFormMain.AllowChangeBindings: Boolean;
begin
  Result := True;
  if (Gbl_SM.FTPSessionCount > 0) then
    Result := (MessageDlg('There are active FTP connections. All connections' + #10 +
      'will be closed if the bindings are changed.' + #10#10 + 'Change bindings anyway?',
      mtConfirmation, mbOKCancel, 0) = mrOk)
end;

procedure TFormMain.RadioButtonListenModeClick(Sender: TObject);
var
  NewListenMode: TListenMode;
  NewLMS: String;

begin
  NewListenMode := TListenMode(TComponent(Sender).Tag);

  if (NewListenMode = ListenMode) then
    Exit;

  if (NewListenMode <> lmManual) and not AllowChangeBindings then
    begin
      SelectListenModeRadio;
      Exit;
    end;

  ListenMode := NewListenMode;
  case ListenMode of
    lmAll: NewLMS := LMS_ALL;
    lmLocal: NewLMS := LMS_LOCAL;
    lmManual: NewLMS := LMS_MANUAL;
  end;
  with ConfigMap do
    begin
      SetValue(RN_LISTENMODE, NewLMS);
      SaveChanges;
    end;

  if (ListenMode <> lmManual) and SetFTPActive(False) then
    begin
      ListViewBindings.Clear;

      while (FTPS.Bindings.Count > 0) do
        FTPS.Bindings.Delete(0);

      case ListenMode of
        lmAll: AddBinding(IP_ALL, DEFAULT_PORT);
        lmLocal: AddBinding(IP_LOCAL, DEFAULT_PORT);
      end;

      SetFTPActive(True);
    end;

  FashionBindingsPage;
end;

procedure TFormMain.PopupMenuMainPopup(Sender: TObject);
begin
  SetForegroundWindow(Handle);
end;

procedure TFormMain.WMShowSelf(var Msg: TMessage);
begin
  MenuItemOpenClick(Self);
end;

procedure TFormMain.MemoLogDblClick(Sender: TObject);
begin
  FormLog := TFormLog.Create(Self);
  FormLog.MemoLog.Lines.Assign(MemoLog.Lines);
  FormLog.ShowModal;
  FormLog.Free;
end;

function TFormMain.SetFTPActive(Active: Boolean): Boolean;
var
  Idx: Integer;

begin
  Result := False;

  if Active then
    SaveBindings
  else
    for Idx := 0 to Connections.Count - 1 do
      TIdTCPServerConnection(Connections[Idx]).Disconnect;

  while Connections.Count > 0 do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;

  if not (Active and (FTPS.Bindings.Count = 0)) then
    try
      if (FTPS.Active xor Active) then
        begin
          FTPS.Active := Active;
          if Active then
            Log('FTP server started')
          else
            Log('FTP server stopped');
        end;
      Result := True;
    except
      on E: Exception do Log('Error: ' + E.Message);
    end
  else
    Log('Warning: No bindings');
end;

procedure TFormMain.ButtonRemoveBindingClick(Sender: TObject);
begin
  with ListViewBindings do
    if AllowChangeBindings and SetFTPActive(False) then
      begin
        FTPS.Bindings.Delete(ItemIndex);
        Items.Delete(ItemIndex);

        ListViewBindingsClick(Self);

        SetFTPActive(True);
      end;
end;

procedure TFormMain.SaveBindings;
var
  Bindings: TStringList;
  Idx: Integer;

begin
  Bindings := TStringList.Create;
  for Idx := 0 to ListViewBindings.Items.Count - 1 do
    with ListViewBindings.Items[Idx] do
      Bindings.Add(Caption + ':' + Trim(SubItems.Text));
  ConfigMap.SetValue(RN_BINDINGS, Bindings.CommaText);
  ConfigMap.SaveChanges;
  Bindings.Free;
end;

end.
