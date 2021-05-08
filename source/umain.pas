unit umain;
{
@name(umain)
@shorttitle(Main unit of the project)
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(20190201)
@lastmod(see uconstants)
@note(Under Ubuntu ensure to run sudo apt-get install libsqlite3-dev)
}
{$mode objfpc}{$H+}

{ TODO 1 -cToolbar : Change toolbarmain by panel with speedbuttons to allow checkboxes, comboboxes etc. }
{ TODO 1 -cApplication : Use Lazarus icons from folder c:\Prog\lazarus\images\. }
{ DONE 3 -cEditor : Option to show the synedit gutter. }
{ DONE 3 -cEditor : Option to set the font size of synedit. }
{ DONE 3 -cApplication : Add menustructure File,Edit,Help. }

interface

uses
 Classes, SysUtils, FileUtil,
 SynEdit, SynEditKeyCmds, SynHighlighterAny,
 SynHighlighterCpp, SynHighlighterXML, SynHighlighterPython,
 Forms, Controls,
 Graphics, Dialogs, ComCtrls, ActnList, DBGrids, ExtCtrls, Menus, IniFiles,
 // project units
 uconstants, umisc, usettings
 // ,udownload = ON HOLD
 , LCLType, DB, Grids;

type

  { TFormMain }

  TFormMain = class(TForm)
     ActionToolsEditorGutter: TAction;
     ActionToolsEditorFontSize: TAction;
     ActionEditSelectAll: TAction;
     ActionEditPaste: TAction;
     ActionEditCopy: TAction;
     ActionEditCut: TAction;
     ActionEditFilterReset: TAction;
     ActionEditFilter: TAction;
     ActionFileLoad: TAction;
     ActionFileExportEachScriptToFile: TAction;
     ActionEditCopyScript: TAction;
     ActionToolsSettings: TAction;
    ActionHelpAbout: TAction;
    ActionFileExportAllToHTML: TAction;
    ActionFileExportAllToText: TAction;
    ActionFileRefresh: TAction;
    ActionFileExit: TAction;
    ActionListMain: TActionList;
    DBGridDomoticz: TDBGrid;
    ImageListMain: TImageList;
    MenuFileExportSingleFile: TMenuItem;
    MenuEditCopyScript: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuEditCute: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditPaste: TMenuItem;
    MenuEditSelectAll: TMenuItem;
    MenuItem3: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    MenuToolsEditorGutter: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuToolsSettings: TMenuItem;
    MenuTools: TMenuItem;
    MenuMain: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileRefresh: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuFileExport: TMenuItem;
    MenuFileExportText: TMenuItem;
    MenuFileExportHTML: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    PanelDBGrid: TPanel;
    SaveDialogMain: TSaveDialog;
    SelectDirectoryDialogMain: TSelectDirectoryDialog;
    SplitterDBGrid: TSplitter;
    StatusBarMain: TStatusBar;
    SynLuaSyn: TSynAnySyn;
    SynEditScript: TSynEdit;
    SynPythonSyn: TSynPythonSyn;
    SynXMLSyn: TSynXMLSyn;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton8: TToolButton;
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditCopyScriptExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditFilterExecute(Sender: TObject);
    procedure ActionEditFilterResetExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionFileExportEachScriptToFileExecute(Sender: TObject);
    procedure ActionFileExportAllToTextExecute(Sender: TObject);
    procedure ActionFileExportAllToHTMLExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionFileRefreshExecute(Sender: TObject);
    procedure ActionFileLoadExecute(Sender: TObject);
    procedure ActionToolsEditorFontSizeExecute(Sender: TObject);
    procedure ActionToolsEditorGutterExecute(Sender: TObject);
    procedure ActionToolsSettingsExecute(Sender: TObject);
    procedure DBGridDomoticzDrawColumnCell(Sender: TObject; const {%H-}Rect: TRect;
      {%H-}DataCol: Integer; {%H-}Column: TColumn; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormShow(Sender: TObject);
   private
     FFirstTime: Boolean;               // Flag used to check if database to be loaded
     FSettingsFile: TIniFile;           // Inifile in the application folder
     FSettings: TSettingsData;          // Settings defined in a record
     procedure LoadSettings();
     procedure SaveSettings();
     procedure SetSynEditSyntaxHighligher;
   public
     // Used by the DBMgr
     procedure SetSynEditXMLStatement(AText: String);
 end;

var
 FormMain: TFormMain;

implementation

{$R *.lfm}
uses
 udbmgr;

procedure TFormMain.FormCreate(Sender: TObject);
begin
 FFirstTime := True;
 // Read the settings to be able to read the domoticz database
 LoadSettings();
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
 Caption := VERSION;
 PanelDBGrid.Width := 240;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
 // FormMain.Refresh;
 ActionFileRefreshExecute(Sender);
 SetSyneditSyntaxHighligher;
 FFirstTime := False;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 DBMgr.CloseDatabase;
 SaveSettings;
end;

{REGION ACTIONS}
procedure TFormMain.ActionFileExitExecute(Sender: TObject);
begin
 Close;
end;

// Download the domoticz database using a shared folder on the rpi domoticz server
(*
The shared folder uses samba and is configured with:
sudo nano /etc/samba/smb.conf
[DoProDomoticz]
Comment = Raspberry Pi Domoticz Production folder
Path = /home/pi/domoticz
Browseable = yes
Writeable = Yes
only guest = no
create mask = 0777
directory mask = 0777
Public = yes
Guest ok = yes

Notes on the settings:
[DoProDomoticz]	= Name of the share
comment = The text is displayed as Comments in shares detail view
path = Specifies the folder that contains the files to be shared
browsable = yes|no - Set this share to be visible when running the net view command and also when you browse the network shares.
writable = yes|no - Allows user to add/modify files and folders in this share. By the default samba shares are readonly
guest ok = yes|no - Allows non authenticated users access the share

After configuration:
Ensure to set the directory mask for the domoticz folder to 0777:
sudo chmod 777 /home/pi/domoticz

Then restart samba:
sudo /etc/init.d/samba restart

And restart domoticz:
sudo service domoticz.sh restart

In windows connect to the shared folder using the Windows Explorer > Connect Network:
Path for drive Z: is \\domoticz-ip\DoProDomoticz
(DoProDomoticz is defined in the samba configuration)

After this prework, the domoticz database domoticz.db can be copied using CopyFile.
(simple solution using the standard lazarus fileutil functions).
*)
procedure TFormMain.ActionFileRefreshExecute(Sender: TObject);
const
  MSGTITLE = 'Refresh Database';
var
 bCopyDatabase: Boolean;
begin
 // Clear the dbgrid columns
 DBGridDomoticz.Columns.Clear;
 DBGridDomoticz.Visible := false;

 // Close the database first to avoid being locked
 DBMgr.CloseDatabase();

 bCopyDatabase := True;
 if FFirstTime and Not FSettings.RefreshAtStart then begin
   bCopyDatabase:= False;
 end;

 if bCopyDatabase or not FileExists(FSettings.DatabaseFile) then begin
  StatusBarMain.SimpleText := 'Please wait, copying the Domoticz database from the server...';
  StatusBarMain.Refresh;
  SynEditScript.Text := StatusBarMain.SimpleText;
  SynEditScript.Refresh;
  // If flag is set, then Download the database file from the domotic server
  // Example: z:\domoticz.db, domoticz.db
  if CopyFile(FSettings.SharedDrive+FSettings.DatabaseFile, FSettings.DatabaseFile) then begin
    StatusBarMain.SimpleText:='Domoticz database successfully copied from the Domoticz server.';
  end
  else begin
    StatusBarMain.SimpleText:='[Error] Domoticz database not copied from the Domoticz server.';
    ShowError(MSGTITLE,
      StatusBarMain.SimpleText
      +LineEnding
      +SysErrorMessage(GetLastOSError)
      +LineEnding+LineEnding
      +'Current database will be reloaded.');
    ActionFileLoadExecute(Sender);
    Exit;
  end;
 end;
 ActionFileLoadExecute(Sender);
end;

procedure TFormMain.ActionFileLoadExecute(Sender: TObject);
begin
  DBGridDomoticz.Visible := True;
  // Clear the dbgrid columns
 DBGridDomoticz.Columns.Clear;
 // Close the database first to avoid being locked
 DBMgr.CloseDatabase();
 if not FileExists(FSettings.DatabaseFile) then begin
    StatusBarMain.SimpleText:='[Error] Domoticz database not found.';
    ShowError('Load Database',
      StatusBarMain.SimpleText + LineEnding + 'Action: Refresh or copy the database to the application folder.');
 end;
 // Open the database, select the records, build the dbgrid
 // Hide the XMLStatement column
 if DBMgr.OpenDatabase(FSettings.DatabaseFile,
                       ExtractFilePath(Application.ExeName),
                       FSettings.SQLQuery) then begin
   DBGridDomoticz.Columns.Items[0].Width := 200;       // Name
   DBGridDomoticz.Columns.Items[1].Width := 100;       // Interpreter
   DBGridDomoticz.Columns.Items[2].Visible := False;   // XML-Statement
   DBGridDomoticz.Columns.Items[3].Width := 75;        // State
   DBGridDomoticz.Columns.Items[3].Field.Alignment := taLeftJustify;
   DBMgr.SetSQLQueryFilter(FSettings.Filter);
   StatusBarMain.SimpleText:='Domoticz database opened.';
 end
 else begin
   StatusBarMain.SimpleText:='[Error] Domoticz database not opened.';
   ShowError('Load Database',
     StatusBarMain.SimpleText + LineEnding+DBMgr.StatusMsg);
 end;
end;

procedure TFormMain.ActionToolsEditorFontSizeExecute(Sender: TObject);
var
  // InputResult: Boolean;
  InputString: String;
  FontSize: Integer;
begin
  InputString := IntToStr(SynEditScript.Font.Size);
  if InputQuery('Editor Font Size', 'Enter Size:', False, InputString) then begin
     try
       FontSize := StrToInt(InputString);
       SynEditScript.Font.Size := FontSize;
     except on E: Exception do begin
         ShowError('Editor Font Size', Format('Wrong font size entered: %s. Try again.',[InputString]));
       end;
     end;
  end;
end;

procedure TFormMain.ActionToolsEditorGutterExecute(Sender: TObject);
begin
  SynEditScript.Gutter.Visible := ActionToolsEditorGutter.Checked;
end;

procedure TFormMain.ActionEditCutExecute(Sender: TObject);
begin
    SynEditScript.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFormMain.ActionEditCopyExecute(Sender: TObject);
begin
    SynEditScript.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TFormMain.ActionEditPasteExecute(Sender: TObject);
begin
  SynEditScript.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFormMain.ActionEditSelectAllExecute(Sender: TObject);
begin
  SynEditScript.CommandProcessor(TSynEditorCommand(ecSelectAll), ' ', nil);
end;

// Copy the content of the selected script to the clipboard
procedure TFormMain.ActionEditCopyScriptExecute(Sender: TObject);
begin
 CopyToClipboard(SynEditScript.Text);
end;

procedure TFormMain.ActionEditFilterExecute(Sender: TObject);
begin
 // Ask for the query without masking (=hide text)
 if InputQuery(
      'Set Filter Condition',
      'Enter (Example:Interpreter="dzVents"; Empty field: filter reset):',
      False,
      FSettings.Filter) then begin
   DBMgr.SetSQLQueryFilter(FSettings.Filter);
   StatusBarMain.SimpleText:='Filter set: '  + FSettings.Filter;
  end;
end;

procedure TFormMain.ActionEditFilterResetExecute(Sender: TObject);
begin
  DBMgr.SetSQLQueryFilter('');
end;

// Export the listed scripts to text file
procedure TFormMain.ActionFileExportAllToTextExecute(Sender: TObject);
begin
  with SaveDialogMain do begin
    FileName := 'domoticzinternalscripts.txt';
    if Execute then begin
     DBGridDomoticz.BeginUpdate;
     DBMgr.ExportScriptsText(FileName);
     DBGridDomoticz.EndUpdate;
     if DBMgr.Status then begin
      StatusBarMain.SimpleText := Format(
        'Exported scripts to text file %s.',
        [FileName]);
      ShowInfo('Export Scripts Text File', StatusBarMain.SimpleText)
     end
     else begin
       StatusBarMain.SimpleText := Format(
         '[ERROR] Export scripts text file: %s.',
         [DBMgr.StatusMsg]);
       ShowError('Export Scripts Text File', StatusBarMain.SimpleText)
     end;
    end;
  end;
end;

procedure TFormMain.ActionFileExportAllToHTMLExecute(Sender: TObject);
begin
  with SaveDialogMain do begin
    FileName := 'domoticzinternalscripts.html';
    if Execute then begin
     DBGridDomoticz.BeginUpdate;
     DBMgr.ExportScriptsHTML(FileName);
     DBGridDomoticz.EndUpdate;
     if DBMgr.Status then begin
      StatusBarMain.SimpleText := Format(
        'Exported scripts to HTML file %s.',
        [FileName]);
      ShowInfo('Export Scripts HTML File', StatusBarMain.SimpleText)
     end
     else begin
       StatusBarMain.SimpleText := Format(
         '[ERROR] Export scripts HTML file: %s.',
         [DBMgr.StatusMsg]);
       ShowError('Export Scripts HTML File', StatusBarMain.SimpleText)
     end;
    end;
  end;
end;

procedure TFormMain.ActionFileExportEachScriptToFileExecute(Sender: TObject);
begin
 SelectDirectoryDialogMain.InitialDir := GetCurrentDir;
  if SelectDirectoryDialogMain.Execute then begin
   DBGridDomoticz.BeginUpdate;
   DBMgr.ExportScriptsTextEach(SelectDirectoryDialogMain.FileName);
   DBGridDomoticz.EndUpdate;
   if DBMgr.Status then begin
     StatusBarMain.SimpleText:='Scripts exported to folder ' + SelectDirectoryDialogMain.FileName;
     ShowInfo('Export Each Script Text File', StatusBarMain.SimpleText)
   end
   else begin
     StatusBarMain.SimpleText := DBMgr.StatusMsg;
     ShowError('Export Each Script Text File', StatusBarMain.SimpleText);
   end;
  end;
end;

procedure TFormMain.ActionToolsSettingsExecute(Sender: TObject);
var
 SQLQuery: String;
begin
 SqlQuery := FSettings.SQLQuery;
 Settings.SettingsData := FSettings;
 if Settings.ShowModal = mrOK then begin
   FSettings := Settings.SettingsData;
   // (Re)load the database in case the sql statement has changed
   if SQLQuery <> FSettings.SQLQuery then
      ActionFileLoadExecute(Sender);
 end;
end;

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
 ShowInfo(VERSION, COPYRIGHT);
end;
{ENDREGION ACTIONS}

{REGION DBGRID}
(*
  if a row in the dbgrid is selected, set the synedit syntax highligher.
*)
procedure TFormMain.DBGridDomoticzDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if gdSelected in State then begin
   Case DBGridDomoticz.DataSource.DataSet.FieldByName(FIELDINTERPRETER).AsString of
      INTERPRETERBLOCKLY: SynEditScript.Highlighter := SynXMLSyn;
      INTERPRETERDZVENTS: SynEditScript.Highlighter := SynLuaSyn;
      INTERPRETERPYTHON: SynEditScript.Highlighter := SynPythonSyn;
   end;
   StatusBarMain.SimpleText := Format('Selected: %s (%s)',
     [DBGridDomoticz.DataSource.DataSet.FieldByName(FIELDNAME).AsString,
      DBGridDomoticz.DataSource.DataSet.FieldByName(FIELDINTERPRETER).AsString]);
  end;
end;
{ENDREGION DBGRID}

{REGION SYNEDIT}
(*
  Used by the DBMgr
*)
procedure TFormMain.SetSynEditXMLStatement(AText: String);
begin
 AText := AText.Replace('><', '>'+LineEnding + '<');
 SynEditScript.Text := AText;               // DBGridDomoticz.Columns[2].Field.AsString;
end;
{ENDREGION SYNEDIT}

{REGION SETTINGS}
// Read the settings from the ini file located in the application folder
procedure TFormMain.LoadSettings();
begin
  FSettingsFile := TINIFile.Create(INIFILE);
  with FSettingsFile do begin
    with FSettings do begin
       SharedDrive    := ReadString(INISECTIONGENERAL,'shareddrive', SHAREDDRIVE);
       DatabaseFile   := ReadString(INISECTIONGENERAL,'databasefile', DATABASEFILE);
       RefreshAtStart := ReadBool(INISECTIONGENERAL,'refreshatstart', REFRESHATSTART);
       SQLQuery       := ReadString(INISECTIONGENERAL,'sqlquery', SQLQUERY);
       Filter         := ReadString(INISECTIONGENERAL,'filter', FILTER);
    end;
    // Editor
    ActionToolsEditorGutter.Checked := ReadBool(INISECTIONSYNEDIT,'gutter', False);
    SynEditScript.Gutter.Visible := ActionToolsEditorGutter.Checked;
    SynEditScript.Font.Size := ReadInteger(INISECTIONSYNEDIT,'fontsize', 9);
    SynLuaSyn.Constants.AddCommaText(ReadString(INISECTIONSYNEDIT, 'contants', ''));
    SynLuaSyn.Objects.AddCommaText(ReadString(INISECTIONSYNEDIT, 'objects', ''));
    SynLuaSyn.KeyWords.AddCommaText(ReadString(INISECTIONSYNEDIT, 'keywords', ''));
  end;
end;

// Save the settings to the ini file located in the application folder
procedure TFormMain.SaveSettings();
begin
  with FSettingsFile do begin
    with FSettings do begin
      WriteString(INISECTIONGENERAL,'shareddrive',SharedDrive);
      WriteString(INISECTIONGENERAL,'databasefile',DatabaseFile);
      WriteBool(INISECTIONGENERAL,'refreshatstart',RefreshAtStart);
      WriteString(INISECTIONGENERAL,'sqlquery',SQLQuery);
      WriteString(INISECTIONGENERAL,'filter',Filter);
    end;
    WriteBool(INISECTIONSYNEDIT,'gutter', ActionToolsEditorGutter.Checked);
    WriteInteger(INISECTIONSYNEDIT,'fontsize', SynEditScript.Font.Size);
  end;
end;

// Set syntax highlighter as (kind of) dzVents highlighter
procedure TFormMain.SetSyneditSyntaxHighligher;
begin
  SynLuaSyn.Constants.AddCommaText('DATA,DEVICES,HTTPRESPONSES,LOGGING,TIMER');
  SynLuaSyn.Objects.AddCommaText('LOCAL');
  SynLuaSyn.KeyWords.AddCommaText('ACTIVE,CUSTOMEVENTS,DATA,DEVICES,DUMP,ELSE,END,EXECUTE,FALSE,FILTER,FIND,FOR,FOREACH,FORMAT,FUNCTION,GROUPS,HELPERS,HTTPRESPONSES,IF,IN,LEVEL,LOG,LOGGING,MARKER,NOTIFY,ON,REDUCE,RETURN,SCENES,SECURITY,SHELLCOMMANDRESPONSES,SWITCHOFF,SWITCHON,SYSTEM,THEN,TIME,TIMER,TRUE,VARIABLES');
end;
{ENDREGION SETTINGS}

end.

