unit LocaliserUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON, Vcl.Dialogs, Vcl.Buttons, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls,
  Vcl.Graphics, IniFiles,  Vcl.Forms, Vcl.ExtCtrls, System.DateUtils;

type
  TLocalizerForm = class(TForm)
    FilesList: TMemo;
    En_stringsList: TMemo;
    Ru_stringsList: TMemo;
    nFilesLabel: TLabel;
    nEn_stringsLabel: TLabel;
    nRu_stringsLabel: TLabel;
    RootPathProject: TEdit;
    langLabel: TCheckBox;
    CheckFilesButton: TButton;
    CheckFileLabel: TLabel;
    CheckMemo: TMemo;
    StatusBar: TStatusBar;
    ComfyRootButton: TBitBtn;
    ProceedButton: TBitBtn;
    CurrentIniButton: TBitBtn;
    CurrentIniFile: TEdit;
    OpenIniDialog: TOpenDialog;
    procedure ProceedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TranslateFile(sFile: string; en_list, ru_list: TStrings);
    procedure CheckFilesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComfyRootButtonClick(Sender: TObject);
    procedure CurrentIniButtonClick(Sender: TObject);
    procedure CurrentIniFileChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LocalizerForm: TLocalizerForm;
  workFolder: string;
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

procedure TLocalizerForm.ComfyRootButtonClick(Sender: TObject);
var
  OpenDialog: TFileOpenDialog;
  SelectedFolder: string;
begin
  OpenDialog := TFileOpenDialog.Create(LocalizerForm);
  try
    OpenDialog.Options := OpenDialog.Options + [fdoPickFolders];
    if not OpenDialog.Execute then
      Abort;
    SelectedFolder := OpenDialog.FileName;
    // here check code to real folder ConfyUI or another///
    RootPathProject.Text := (SelectedFolder);
  finally
    OpenDialog.Free;
  end;
end;

procedure TLocalizerForm.CurrentIniButtonClick(Sender: TObject);
var
  SelectedFile: string;
begin
  try
    if not OpenIniDialog.Execute then
      Abort;
    SelectedFile := OpenIniDialog.FileName;
    // here check code thet it is real ini-file
    CurrentIniFile.Text := (SelectedFile);
  finally
  end;
end;

procedure TLocalizerForm.CurrentIniFileChange(Sender: TObject);
begin
  langIniName:= CurrentIniFile.Text;
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
    RootPathProject.Text := ini.ReadString('ProjectPaths', 'WorkPath', 'Нет данных...');
    CurrentIniFile.Text := ini.ReadString('ProjectPaths', 'CurrentIni', 'Нет данных...');
    ProceedButton.Caption := ini.ReadString('Langs', 'ProceedButtonCaption', 'Начать перевод');
    ComfyRootButton.Caption :=ini.ReadString('Langs', 'ComfyRootButton', 'Корень проекта');
    CurrentIniButton.Caption :=ini.ReadString('Langs', 'CurrentIniButton', 'Файл перевода');
    OpenIniDialog.Title:=ini.ReadString('Langs', 'OpenIniDialogTitle', 'Открыть файл перевода');
    langLabel.Caption:=ini.ReadString('Langs', 'langLabelCaption', 'русский');
  end;
  ini.Free;
  workFolder := RootPathProject.Text ;
  langIniName := CurrentIniFile.Text ;
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
    ini.WriteString('ProjectPaths', 'CurrentIni', CurrentIniFile.Text);
    ini.WriteString('Langs', 'ProceedButtonCaption', ProceedButton.Caption);
    ini.WriteString('Langs', 'ComfyRootButton', ComfyRootButton.Caption);
    ini.WriteString('Langs', 'CurrentIniButton', CurrentIniButton.Caption);
    ini.WriteString('Langs', 'OpenIniDialogTitle', OpenIniDialog.Title);
    ini.WriteString('Langs', 'OpenIniDialogTitle', langLabel.Caption);
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
  langINI := TIniFile.Create(langIniName);
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
        Ru_stringsList.Lines[i] := utf8ToAnsi(str);
        with TStringList.Create do
        begin
          begin
            LoadFromFile(FilesList.Lines[a]);
            // for i := 0 to (En_stringsList.Lines.Count - 1) do
            begin
              if langLabel.Checked then
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
