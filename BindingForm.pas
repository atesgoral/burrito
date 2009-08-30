unit BindingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  IdStack, IdGlobal;

const
  IP_ALL = '0.0.0.0';
  IP_LOCAL = '127.0.0.1';
  DEFAULT_PORT = 21;

type
  TFormBinding = class(TForm)
    Panel1: TPanel;
    ComboBoxIP: TComboBox;
    LabelIP: TLabel;
    EditPort: TEdit;
    LabelPort: TLabel;
    ButtonOK: TButton;
    LabelColon: TLabel;
    ButtonCancel: TButton;
    LabelBindingHelp: TLabel;
    procedure EditPortKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBoxIPKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure EditPortChange(Sender: TObject);
    procedure ComboBoxIPChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    HaveValidIP, HaveValidPort: Boolean;
    procedure PopulateIPCombo;
    procedure CheckOKEnabled;
  public
    Port: Integer;
    procedure SetEdit(const IP: String; Port: Integer);
  end;

var
  FormBinding: TFormBinding;

implementation

{$R *.dfm}

procedure TFormBinding.EditPortKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key > #31) and (Key <  #128) then
    if not (Key in ['0'..'9']) then
      Key := #0;
end;

procedure TFormBinding.ComboBoxIPKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key > #31) and (Key <  #128) and (Key <> '.') then
    if not (Key in ['0'..'9']) then
      Key := #0;
end;

procedure TFormBinding.PopulateIPCombo;
var
  Stack: TIdStack;

begin
  Stack := GStackClass.Create;
  with ComboBoxIP.Items do
    begin
      BeginUpdate;
      Append(IP_ALL);
      Append(IP_LOCAL);
      AddStrings(Stack.LocalAddresses);
      EndUpdate;
    end;
  Stack.Free;
end;

procedure TFormBinding.SetEdit(const IP: String; Port: Integer);
begin
  Caption := 'Edit Binding';
  Self.Port := Port;
  ComboBoxIP.Text := IP;
  EditPort.Text := IntToStr(Port);
end;

procedure TFormBinding.FormCreate(Sender: TObject);
begin
  PopulateIPCombo;
  Port := DEFAULT_PORT;
end;

procedure TFormBinding.EditPortChange(Sender: TObject);
var
  Code: Integer;

begin
  Val(EditPort.Text, Port, Code);
  HaveValidPort := (Code = 0) and (Port > 0) and (Port < $10000);
  CheckOKEnabled;
end;

procedure TFormBinding.CheckOKEnabled;
begin
  ButtonOK.Enabled := (HaveValidIP and HaveValidPort);
end;

procedure TFormBinding.ComboBoxIPChange(Sender: TObject);
begin
  HaveValidIP := (ComboBoxIP.ItemIndex = 0) or IsValidIP(ComboBoxIP.Text);
  CheckOKEnabled;
end;

procedure TFormBinding.FormActivate(Sender: TObject);
begin
  if (Tag = 0) then
    begin
      Tag := 1;
      ComboBoxIPChange(Self);
      EditPortChange(Self);
      CheckOKEnabled;
    end;
end;

procedure TFormBinding.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

end.
