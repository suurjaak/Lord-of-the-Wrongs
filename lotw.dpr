program lotw;

{$RESOURCE *.RES}                    // Include lotw.res, has icon
{$RESOURCE 'images.res' 'images.rc'} // Include other image resources

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Globals in 'Globals.pas',
  DataClasses in 'DataClasses.pas',
  Imaging in 'Imaging.pas',
  Persistence in 'Persistence.pas';

begin
  Application.Initialize;
  Application.Title := 'The Lord of the Wrongs';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
