program Burrito;

uses
  Forms,
  Windows,
  MainForm in 'MainForm.pas' {FormMain},
  BindingForm in 'BindingForm.pas' {FormBinding},
  SessionManager in 'SessionManager.pas',
  FilenameFormat in 'FilenameFormat.pas',
  ConfigurationMap in 'ConfigurationMap.pas',
  LogForm in 'LogForm.pas' {FormLog};

{$R *.res}

var
  Handle: HWnd;

begin
  Handle:= FindWindow(PChar('TFormMain'), FORM_CAPTION);
//  GetWindowLong(DWL_USER
  if (Handle = 0) then
    begin
      Application.Initialize;
      Application.Title := 'Burrito';
      Application.CreateForm(TFormMain, FormMain);
  Application.Run;
    end
  else
    PostMessage(Handle, WM_SHOWSELF, 0, 0);
end.
