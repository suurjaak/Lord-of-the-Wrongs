(**
 * Lord of the Wrongs card creator application.
 *
 * @author    Erki Suurjaak
 * @created   12.20.2003
 * @modified  11.11.2011
 *)
program lotw;

{$RESOURCE *.RES}                    // Include lotw.res, has icon
{$RESOURCE 'images.res' 'images.rc'} // Include other image resources

uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  Main in 'Main.pas' {MainForm},
  Globals in 'Globals.pas',
  DataClasses in 'DataClasses.pas',
  Imaging in 'Imaging.pas',
  Persistence in 'Persistence.pas',
  BrowseForFolderU in 'BrowseForFolderU.pas';

begin
  Application.Initialize;
  Application.Title := 'The Lord of the Wrongs';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
