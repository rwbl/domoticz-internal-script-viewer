unit udbmgr;
{
@name(udbmgr)
@shorttitle(Domiticz SQLite3 Database operations, mainly SELECT)
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(20190201)
@lastmod(see uconstants)
}
{REGION NOTES}
{
From the domoticz sqlite3 database, domoticz.db - in the application folder, the dzVents scripts are selected.
Table Name:
EventMaster
Table Structure Event Master:
CREATE TABLE [EventMaster] ([ID] INTEGER PRIMARY KEY,  [Name] VARCHAR(200) NOT NULL, [XMLStatement] TEXT NOT NULL, [Status] INTEGER DEFAULT 0, [Interpreter] VARCHAR(10) DEFAULT 'Blockly', [Type] VARCHAR(10) DEFAULT 'All');
Fields Used:
Name,Interpreter,XMLStatement,Status
SQL Select Statement Examples:
SELECT Name,Interpreter,XMLStatement,Status FROM EventMaster WHERE Interpreter="dzVents" ORDER BY Interpreter,Name;
SELECT Name,Interpreter,XMLStatement,Status FROM EventMaster WHERE Interpreter = "dzVents" AND Status > -1 ORDER BY Interpreter,Name;
}
{ENDREGION NOTES}

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, sqldb, db, sqlite3conn,
 strutils,
 LazFileUtils,
 Dialogs,  //ShowMessage for debugging
 umain;

const
  FIELDINTERPRETER       = 'Interpreter';
  FIELDNAME              = 'Name';
  FIELDXMLSTATEMENT      = 'XMLStatement';
  FIELDSTATUS            = 'Status';
  INTERPRETERDZVENTS     = 'dzVents';
  INTERPRETERPYTHON      = 'Python';
  STATUSOK               = 'OK';
  STATUSERR              = 'ERR';

type

 { TDBMgr }

 TDBMgr = class(TDataModule)
  DataSourceDomoticz: TDataSource;
  SQLite3Connection1: TSQLite3Connection;
  SQLQueryDomoticz: TSQLQuery;
  SQLTransaction1: TSQLTransaction;
  procedure DataModuleCreate(Sender: TObject);
  procedure DataSourceDomoticzDataChange(Sender: TObject; {%H-}Field: TField);
 private
  FSQLCompleted: Boolean;
  FSQLStatement: String;
  // SQL
  function  SQLOpen(ADBAction: String; ASQLQuery: TSQLQuery; ASQLStatement: String): Boolean;
  function  SQLExec(ADBAction: String; ASQLQuery: TSQLQuery; ASQLStatement: String): Boolean;
  // Helpers
  function  SetStatus(AAction: String; AStatus: Boolean; AStatusMsg: String):Boolean;
  function  GetDateTime(): String;
 public
  DatabaseName: String;	    // Short name of the database. Should not contain blanks.
  DatabaseFolder: String;	  // Application folder +  database name.
  DatabaseFile: String;	    // Full path of the database file.
  Status: Boolean;          // Status true | false returned from functions/procedures.
  StatusMsg: String;        // Status message returned from functions/procedures.
  Connected: Boolean;       // Flag used to check if connected to a database.
  // Database
  function   SetDatabaseProperties(AName: String; AFolder: String): Boolean;
  function   OpenDatabase(AName: String; AFolder: String; ASQLStatement: String): Boolean;
  function   CloseDatabase: Boolean;
  function   SetSQLQueryFilter(AFilter: String):Boolean;
  procedure  ExportScriptsText(FileName:String);
  procedure  ExportScriptsHTML(FileName:String);
  procedure  ExportScriptsTextEach(AFolder: String);
  // Various
  function   SQLiteVersion:String;
  function   TableInfo(ATableName: String): String;
 end;

var
 DBMgr: TDBMgr;

implementation

{$R *.lfm}
procedure TDBMgr.DataModuleCreate(Sender: TObject);
begin
  // No Action
end;

{REGION DATABASE}
(*
  Get the sqlite version, i.e. 3.8.6
*)
function TDBMgr.SQLiteVersion:String;
begin
  SQLiteVersion :='Unknown';
  FSQLStatement := 'SELECT sqlite_version()';
  if SQLOpen('SQLiteVersion', SQLQueryDomoticz, FSQLStatement) then begin
    SQLiteVersion := SQLQueryDomoticz.Fields[0].AsString;
    SetStatus('SQLiteVersion',True,SQLiteVersion);
  end else begin
    SetStatus('SQLiteVersion',False,SQLiteVersion);
  end;
end;

// Get the fields for a table.
// NOT USED for now
function TDBMgr.TableInfo(ATableName: String): String;
begin
  Exit;
  TableInfo := '';
  if NOT Connected then Exit;
  FSQLStatement := 'PRAGMA table_info(' + ATableName + ')';
  if SQLOpen('TableInfo', SQLQueryDomoticz, FSQLStatement) then begin
    while not SQLQueryDomoticz.EOF do begin
      TableInfo := TableInfo
      + Format('%s,%s',
          [DBMgr.SQLQueryDomoticz.FieldByName('name').AsString,
           DBMgr.SQLQueryDomoticz.FieldByName('type').AsString])
      + LineEnding;
      SQLQueryDomoticz.Next;
    end;
    SetStatus('TableInfo',True,TableInfo);
  end
  else begin
    SetStatus('TableInfo',False,'Database is not connected.');
  end;
end;

// Set the database properties DatabaseName,DatabaseFolder,DatabaseFile(fullpath)
// Name - Name of the database
// Folder - Name of the folder containing the database
// Result: True=OK, False=Error.
function TDBMgr.SetDatabaseProperties(AName: String; AFolder: String): Boolean;
begin
  if AName = '' Then begin
     SetDatabaseProperties := SetStatus(
       'SetDatabaseProperties',
       False,
       'No database name specified.');
     Exit;
  end;
  DatabaseName := AName;
  // The database folder is the application folder
  DatabaseFolder := AppendPathDelim(ExtractFilePath(AFolder));
  // The database file contains the fullpath
  DatabaseFile := AppendPathDelim(DatabaseFolder) + DatabaseName;
  SetDatabaseProperties := SetStatus(
    'SetDatabaseProperties',
    True,
    Format(
      'Folder:%s,File:%s,Database:%s',
      [DatabaseFolder,DatabaseFile,DatabaseName]));
end;

// Connect to an existing sqlite3 db file located in the a subfolder of the application folder
// DBFile - DB filename holding the full path
// Result: True=OK, False=Error.
function TDBMgr.OpenDatabase(AName: String; AFolder: String; ASQLStatement: String): Boolean;
begin
  // Set the databse properties prior opening
  If NOT SetDatabaseProperties(AName, AFolder) then begin
    OpenDatabase := SetStatus(
      'OpenDatabase',
      False,
      Format('Database %s properties are not set.',[AName]));
    Exit;
  end;
  // Check if the db exists
  if NOT FileExistsUTF8(DatabaseFile) then begin
    OpenDatabase := SetStatus(
      'OpenDatabase',
      False,
      Format('Database %s not found.',[AName]));
  	Exit;
  end;
  // Disconnect if already connected
  if Connected then begin
    DBMgr.SQLite3Connection1.Connected := False;
    DBMgr.SQLTransaction1.Active := False;
    Connected := False;
  end;
   // Select the db located in a subfolder of the exe folder
  DBMgr.SQLite3Connection1.DatabaseName := DatabaseFile;
  // Open the connection
  try
     DBMgr.SQLite3Connection1.Open;
     Connected := True;
     // TableInfo('EventMaster');
     SQLOpen('SelectEvents', SQLQueryDomoticz, ASQLStatement);
  except
    // will only be executed in case of an exception
    on E: EDatabaseError do begin
      OpenDatabase := SetStatus(
        'OpenDatabase',
        False,
        'Database error: '+ E.ClassName + ','+ E.Message);
        Exit;
    end;
    on E: Exception do begin
      OpenDatabase := SetStatus(
        'OpenDatabase',
        False,
        'Error: '+ E.ClassName + ','+ E.Message);
        Exit;
    end;
  end;
  OpenDatabase := SetStatus('OpenDatabase',Connected,'');
end;

// Close the db in a proper way.
// Result: True=OK, False=Error.
function TDBMgr.CloseDatabase: Boolean;
begin
  if NOT Connected then begin
    CloseDatabase := SetStatus('CloseDatabase',False,'Database is not connected.');
  	Exit;
  end;
  try
     with DBMgr do begin
         SQLQueryDomoticz.Close;
         SQLTransaction1.Active := False;
         SQLite3Connection1.Connected := False;
     end;
     Connected := False;
  except
    // will only be executed in case of an exception
    on E: EDatabaseError do begin
      CloseDatabase := SetStatus(
        'CloseDatabase',
        False,
        'Database error: '+ E.ClassName +','+ E.Message);
    	Exit;
    end;
    on E: Exception do begin
      CloseDatabase := SetStatus(
        'CloseDatabase',
        False,
        'Error: '+ E.ClassName +','+ E.Message);
        Exit;
    end;
  end;
  CloseDatabase := SetStatus(
    'CloseDatabase',
    Connected,
    'Database '+DatabaseName+' closed.');
end;

(*
  Set filter on SQLQuery.
  DataSource = DBMgr.DataSourceDomoticz, DataSet = DBMgr.SQLQueryDomoticz
  Examples Filter:
  Filter := 'Interpreter = "dzVents"';
*)
function TDBMgr.SetSQLQueryFilter(AFilter: String):Boolean;
begin
  if Length(AFilter.Trim) > 0 then begin
    DataSourceDomoticz.DataSet.Filtered := False;
    DataSourceDomoticz.DataSet.Filter   := AFilter;
    DataSourceDomoticz.DataSet.Filtered := True;
    SetSQLQueryFilter := True;
  end
  else begin
    DataSourceDomoticz.DataSet.Filtered := False;
    SetSQLQueryFilter := False;
  end;
  DataSourceDomoticz.DataSet.Refresh;
end;

(*
  Export all scripts to a single text file seperated by a *** line.
*)
procedure TDBMgr.ExportScriptsText(FileName:String);
var
 fSL: TStringList;
 Line: String = '';
begin
 try
   Line := AddChar('*',Line,50);
   fSL := TStringList.Create;
   if FileName.Length = 0 then FileName := 'domoticzscripts.txt';
   DBMgr.SQLQueryDomoticz.First;
   while not DBMgr.SQLQueryDomoticz.EOF do begin
      fSL.Add('Script Name:' + LineEnding + DBMgr.SQLQueryDomoticz.FieldByName(FIELDNAME).AsString);
      fSL.Add('Script Code:' + LineEnding + DBMgr.SQLQueryDomoticz.FieldByName(FIELDXMLSTATEMENT).AsString);
      fSL.Add(Line);
      SQLQueryDomoticz.Next;
   end;
   DBMgr.SQLQueryDomoticz.First;
   fSL.SaveToFile(FileName);
   fSL.Destroy;
   SetStatus('ExportScriptsText',True,'Scripts exported to text file '+Filename);
 except on E: Exception do
   SetStatus('ExportScriptsText',False,E.Message);
 end;
end;

(*
  Export each scripis to its own file with extension .dzvents or .py
*)
procedure TDBMgr.ExportScriptsTextEach(AFolder: String);
var
 fSL: TStringList;
 Line: String = '';
 FileName: String = '';
 FileExt: String = '';
begin
 try
    // Expand folder name. If empty, current folder taken. Append delim / or \.
    AFolder := ExpandFileName(AFolder);
    AFolder := AppendPathDelim(AFolder);
    Line := AddChar('*',Line,50);
    fSL := TStringList.Create;
    DBMgr.SQLQueryDomoticz.First;
    while not DBMgr.SQLQueryDomoticz.EOF do begin
      // Set the filename
      if DBMgr.SQLQueryDomoticz.FieldByName(FIELDINTERPRETER).AsString = INTERPRETERDZVENTS then
         FileExt := '.dzvents';
      if DBMgr.SQLQueryDomoticz.FieldByName(FIELDINTERPRETER).AsString = INTERPRETERPYTHON then
         FileExt := '.py';
      FileName := AFolder + DBMgr.SQLQueryDomoticz.FieldByName(FIELDNAME).AsString + FileExt;
      FileName := FileName.Replace(' ', '_', [rfReplaceAll]);
      fSL.Clear;
      fSL.Add(DBMgr.SQLQueryDomoticz.FieldByName(FIELDXMLSTATEMENT).AsString);
      fSL.SaveToFile(FileName);
      SQLQueryDomoticz.Next;
    end;
    DBMgr.SQLQueryDomoticz.First;
    fSL.Destroy;
    SetStatus('ExportScriptsTextEach',True,'Each scripts exported to a text file.');
 except on E: Exception do
   SetStatus('ExportScriptsTextEach',False,E.Message);
 end;
end;

(*
  Export all scripts to a single HTML file.
*)
procedure TDBMgr.ExportScriptsHTML(FileName:String);
const
  TITLE: String = 'Domoticz dzVents Scripts';
var
 fSL: TStringList;
 s: String;
begin
 try
   fSL := TStringList.Create;
   fSL.Add('<!DOCTYPE html><html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><title>'+TITLE+'</title></head>'+LineEnding);
   fSL.Add('<body>'+LineEnding);
   fSL.Add('<H1>'+TITLE+'</H1><HR>' + LineEnding);
   if FileName.Length = 0 then FileName := 'domoticzscripts.html';
   DBMgr.SQLQueryDomoticz.First;
   while not DBMgr.SQLQueryDomoticz.EOF do begin
     fSL.Add('<H3>' + DBMgr.SQLQueryDomoticz.FieldByName(FIELDNAME).AsString + '</H3>' + LineEnding);
     s := DBMgr.SQLQueryDomoticz.FieldByName(FIELDXMLSTATEMENT).AsString.Replace(#10,'<BR/>').Replace(Chr(32), '&nbsp;').Replace(Chr(9), '&nbsp;&nbsp;');
     fSL.Add('<CODE>' + s + '</CODE><HR>');
     SQLQueryDomoticz.Next;
   end;
   DBMgr.SQLQueryDomoticz.First;
   fSL.Add('</body></html>'+LineEnding);
   fSL.SaveToFile(FileName);
   fSL.Destroy;
   SetStatus('ExportAllScriptsHTML',True,'Scripts exported to HTML file '+Filename);
 except on E: Exception do
   SetStatus('ExportAllScriptsHTML',False,E.Message);
 end;
end;
{ENDREGION DATABASE}

{REGION DATASOURCE}
// If datasource record changes,then update the XMLStatement SynEdit Component in the form
// The SQLQuery is used instead of the Field parameter (requires to select the filed and also nil case)
procedure TDBMgr.DataSourceDomoticzDataChange(Sender: TObject; Field: TField);
begin
  FormMain.SetSynEditXMLStatement(SQLQueryDomoticz.Fields[2].AsString);
end;
{ENDREGION DATASOURCE}

{REGION SQLOPERATIONS}
// Execute sql select statement
function TDBMgr.SQLOpen(ADBAction: String; ASQLQuery: TSQLQuery; ASQLStatement: String): Boolean;
begin
  if NOT Connected then begin
    SQLOpen := SetStatus(ADBAction,False,'Not connected to the database.');
  	Exit;
  end;
  FSQLCompleted := False;
  FSQLStatement := ASQLStatement;
  Try
     ASQLQuery.Close;
     ASQLQuery.SQL.Text := FSQLStatement;
     SQLite3Connection1.Connected := True;
     SQLTransaction1.Active := True;
     ASQLQuery.Open;
     FSQLCompleted := True;
     SQLOpen := SetStatus(ADBAction,True,ASQLStatement);
  except
    // will only be executed in case of an exception
    on E: EDatabaseError do begin
      SQLOpen := SetStatus(ADBAction,False, '[ERROR] Database: '+ E.ClassName + ', ' + E.Message);
    end;
    on E: Exception do begin
      SQLOpen := SetStatus(ADBAction,False, '[ERROR] '+ E.ClassName + ', ' + E.Message);
    end;
  end;
end;

// Execute SQL insert,update,delete statement
function TDBMgr.SQLExec(ADBAction: String; ASQLQuery: TSQLQuery; ASQLStatement: String): Boolean;
begin
  if NOT Connected then begin
    SQLExec := SetStatus(ADBAction,False,'Not connected to the database.');
  	Exit;
  end;
  FSQLCompleted := False;
  FSQLStatement := ASQLStatement;
  Try
     ASQLQuery.Close;
     SQLite3Connection1.Connected := True;
     SQLTransaction1.Active := True;
     ASQLQuery.SQL.Text := FSQLStatement;
     ASQLQuery.ExecSQL;
     SQLTransaction1.Commit;
     FSQLCompleted := True;
     SQLExec := SetStatus(ADBAction,True, ASQLStatement);
  except
    // will only be executed in case of an exception
    on E: EDatabaseError do begin
      SQLExec := SetStatus(ADBAction,False, '[ERROR] Database: '+ E.ClassName + ', ' + E.Message);
    end;
    on E: Exception do begin
      SQLExec := SetStatus(ADBAction,False, '[ERROR] '+ E.ClassName + ', ' + E.Message);
    end;
  end;
end;
{ENDREGION SQLOPERATIONS}

{REGION HELPERS}
function TDBMgr.SetStatus(AAction: String; AStatus: Boolean; AStatusMsg: String):Boolean;
begin
  Status := AStatus;
  StatusMsg := AStatusMsg;
  if (Status) and (AStatusMsg = '') then StatusMsg := STATUSOK;
  if (Not(Status)) and (AStatusMsg = '') then StatusMsg := STATUSERR;
  StatusMsg := Format('%s - %s',[AAction, AStatusMsg]);
  SetStatus := Status;
end;

// Get the actual date & time
// The formatting as set in umain.formcreate is used, i.e.
// DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd HH:MM';
// Noneed to use the timetostr
function TDBMgr.GetDateTime(): String;
begin
  Result := DateToStr(Now);
  //Result := DateToStr(Now) + ' ' + TimeToStr(Now);
end;
{ENDREGION HELPERS}
end.

