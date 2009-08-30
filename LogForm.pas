unit LogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormLog = class(TForm)
    MemoLog: TMemo;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLog: TFormLog;

implementation

{$R *.dfm}

procedure TFormLog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Close;
end;

end.
