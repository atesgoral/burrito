unit FilenameFormat;

interface

uses
  SysUtils, Math;

function GenerateFilename(const Fmt, Name, Address, Subject: String; Index, Total: Integer): String;

implementation

function SafeFilename(S: String): String;
var
  Len, Idx: Integer;
  Ch: Char;

begin
  Len := Length(S);
  Result := '';
  for Idx := 1 to Len do
    begin
      Ch := S[Idx];
      if not (Ch in ['\', '/', ':', '*', '?', '"', '<', '>', '|']) then
        Result := Result + Ch;
    end;
end;

function GenerateFilename(const Fmt, Name, Address, Subject: String; Index, Total: Integer): String;
var
  Len, Idx, Start: Integer;
  Ch: Char;
  NameFix, Prec: String;

begin
  Len := Length(Fmt);
  Idx := 1;
  Result := '';
  if (Length(Name) > 0) then
    NameFix := Name
  else
    NameFix := Address;
  while (Idx <= Len) do
    begin
      Ch := Fmt[Idx];
      inc(Idx);
      if (Ch <> '/') then
        Result := Result + Ch
      else if (Idx <= Len) then
        begin
          Ch := Lowercase(Fmt[Idx])[1];
          if (Ch in ['n', 'a', 's', 'i']) then
            begin
              inc(Idx);
              if (Idx < Len) and (Fmt[Idx] = ':') then
                begin
                  Start := Idx + 1;
                  repeat
                    inc(Idx);
                  until (Idx > Len) or not (Fmt[Idx] in ['0'..'9']);
                  Prec := '.' + Copy(Fmt, Start, Idx - Start);
                end
              else if (Ch = 'i') then
                Prec := Format('.%d', [Trunc(Log10(Total)) + 1])
              else
                Prec := '';
              case Ch of
                'n': Result := Result + Format('%' + Prec + 's', [NameFix]);
                'a': Result := Result + Format('%' + Prec + 's', [Address]);
                's': Result := Result + Format('%' + Prec + 's', [Subject]);
                'i': Result := Result + Format('%' + Prec + 'd', [Index]);
              end;
            end
          else
            raise Exception.Create('Invalid format variable: /' + Ch);
        end;
    end;
  Result := SafeFilename(Result);
end;

end.
