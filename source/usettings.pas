unit usettings;
{
@name(usettings)
@shorttitle(Settings for the project)
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(20190201)
@lastmod(see uconstants)
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  // Settings record populated from inifile
  TSettingsData = Record
    SharedDrive: String;
    DatabaseFile: String;
    RefreshAtStart: Boolean;
    SQLQuery: String;
    Filter: String;
    EditorGutter: Boolean;
    EditorFontSize: Integer;
  end;

  { TSettings }

  TSettings = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxRefreshAtStart: TCheckBox;
    EditFilter: TEdit;
    EditSharedDrive: TEdit;
    EditDatabaseFile: TEdit;
    GroupBoxVarious: TGroupBox;
    GroupBoxSQLQuery: TGroupBox;
    GroupBoxSharedDrive: TGroupBox;
    GroupBoxDatabaseFile: TGroupBox;
    LabelFilter: TLabel;
    LabelSyntaxHighlighting: TLabel;
    LabelSyntaxHighlightingHint: TLabel;
    LabelSqlQuery: TLabel;
    LabelSharedDrive: TLabel;
    LabelDatabaseFile: TLabel;
    LabelRefreshAtStart: TLabel;
    MemoSqlQuery: TMemo;
    PanelButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
  public
    SettingsData: TSettingsData;
  end;

var
   Settings: TSettings;

implementation

{$R *.lfm}

{ TSettings }

procedure TSettings.FormCreate(Sender: TObject);
begin
  //
end;

// Assign the latest settings to the components
procedure TSettings.FormShow(Sender: TObject);
begin
  with SettingsData do begin
    EditSharedDrive.Text           := SharedDrive;
    EditDatabaseFile.Text          := DatabaseFile;
    CheckBoxRefreshAtStart.Checked := RefreshAtStart;
    MemoSqlQuery.Text              := SQLQuery;
    EditFilter.Text                := Filter;
  end;
end;

procedure TSettings.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;
  if ModalResult = mrOK then begin
    with SettingsData do begin
      SharedDrive    := EditSharedDrive.Text;
      DatabaseFile   := EditDatabaseFile.Text;
      RefreshAtStart := CheckBoxRefreshAtStart.Checked;
      SQLQuery       := MemoSqlQuery.Text;
      Filter         := EditFilter.Text;
    end;
  end;
end;

end.

