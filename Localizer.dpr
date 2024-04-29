program Localizer;

uses
  Vcl.Forms,
  LocaliserUnit in 'LocaliserUnit.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TLocalizerForm, LocalizerForm);
  Application.Run;
end.
