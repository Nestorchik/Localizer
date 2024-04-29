unit LocaliserUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, IniFiles,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, System.DateUtils;

type
  TLocalizerForm = class(TForm)
    FilesList: TMemo;
    En_stringsList: TMemo;
    Ru_stringsList: TMemo;
    ProceedButton: TButton;
    nFilesLabel: TLabel;
    nEn_stringsLabel: TLabel;
    nRu_stringsLabel: TLabel;
    RootPathProject: TEdit;
    russian: TCheckBox;
    CheckFilesButton: TButton;
    CheckFileLabel: TLabel;
    CheckMemo: TMemo;
    StatusBar: TStatusBar;
    procedure ProceedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TranslateFile(sFile: string; en_list, ru_list: TStrings);
    procedure CheckFilesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LocalizerForm: TLocalizerForm;
  workFolder, dataFolder: string;
  langIniName: string;

implementation

{$R *.dfm}

procedure TLocalizerForm.CheckFilesButtonClick(Sender: TObject);
var
  i, a: integer;
  tFile: textFile;
  str: string;
begin
  CheckFileLabel.Caption := '';
  a := 0;
  for i := 0 to (FilesList.Lines.Count - 1) do
  begin
    if FileExists(FilesList.Lines[i]) then
    begin
      inc(a);
      CheckFileLabel.Caption := IntToStr(a);
      AssignFile(tFile, FilesList.Lines[i]);
      Reset(tFile);
      while not Eof(tFile) do
      begin
        Readln(tFile, str);
      end;
      CloseFile(tFile);
      CheckMemo.Lines.Add(IntToStr(i) + ' - ' + FilesList.Lines[i]);
    end;
  end;
end;

procedure TLocalizerForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Extractfilepath(paramstr(0)) + 'Localizer.ini');
  with LocalizerForm do
  begin
    Width := ini.ReadInteger('WinSize', 'Width', 1000);
    Height := ini.ReadInteger('WinSize', 'Height', 680);
    Left := ini.ReadInteger('WinPosition', 'X', 100);
    Top := ini.ReadInteger('WinPosition', 'Y', 100);
    RootPathProject.Text := Utf8ToAnsi(ini.ReadString('ProjectPaths', 'WorkPath', ''));
    ProceedButton.Caption := Utf8ToAnsi(ini.ReadString('Langs', 'ProceedButtonCaption', 'Start'));
  end;
  ini.Free;

  workFolder := 'c:\6';
  dataFolder := 'c:\6';
  langIniName := 'ru.ini';
end;

procedure TLocalizerForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Extractfilepath(paramstr(0)) + 'Localizer.ini');
  with LocalizerForm do
  begin
    ini.WriteInteger('WinSize', 'Width', Width);
    ini.WriteInteger('WinSize', 'Height', Height);
    ini.WriteInteger('WinPosition', 'X', Left);
    ini.WriteInteger('WinPosition', 'Y', Top);
    ini.WriteString('ProjectPaths', 'WorkPath', RootPathProject.Text);
  end;
  ini.Free;
end;

procedure TLocalizerForm.TranslateFile(sFile: string; en_list, ru_list: TStrings);
var
  i, a: integer;
  tFile: textFile;
  str: string;
begin
  CheckFileLabel.Caption := '';
  a := 0;
  for i := 0 to (FilesList.Lines.Count - 1) do
  begin
    if FileExists(FilesList.Lines[i]) then
    begin
      AssignFile(tFile, FilesList.Lines[i]);
      Readln(tFile, str);
      CloseFile(tFile);
      inc(a);
      CheckFileLabel.Caption := IntToStr(a);
    end;
  end;
end;

procedure TLocalizerForm.ProceedButtonClick(Sender: TObject);
var
  i, a: integer;
  str: string;
  langINI: TIniFile;
begin
  langINI := TIniFile.Create(dataFolder + '\' + langIniName);
  With langINI do
  begin
    ReadSections(FilesList.Lines);
    for a := 0 to (FilesList.Lines.Count - 1) do
    begin
      ReadSection(FilesList.Lines[a], En_stringsList.Lines);
      ReadSectionValues(FilesList.Lines[a], Ru_stringsList.Lines);
      for i := 0 to (En_stringsList.Lines.Count - 1) do
      begin
        str := Ru_stringsList.Lines[i];
        Delete(str, 1, Pos('=', str));
        Application.ProcessMessages;
        Ru_stringsList.Lines[i] := str;
        with TStringList.Create do
        begin
          begin
            LoadFromFile(FilesList.Lines[a]);
            // for i := 0 to (En_stringsList.Lines.Count - 1) do
            begin
              if russian.Checked then
                Text := StringReplace(Text, En_stringsList.Lines[i], Ru_stringsList.Lines[i], [rfReplaceAll])
              else
                Text := StringReplace(Text, Ru_stringsList.Lines[i], En_stringsList.Lines[i], [rfReplaceAll]);
            end;
            SaveToFile(FilesList.Lines[a]);
            sleep(3);
          end;
          Free;
        end;
        // CheckMemo.Lines.Add(IntToStr(a) + ' - ' + IntToStr(i) + ' : ' + FilesList.Lines[a] + ' ==== ' + str);
      end;

    end;

  end;
  langINI.Free;
  nFilesLabel.Caption := IntToStr(FilesList.Lines.Count);
  nEn_stringsLabel.Caption := IntToStr(En_stringsList.Lines.Count);
  nRu_stringsLabel.Caption := IntToStr(Ru_stringsList.Lines.Count);

end;

end.
