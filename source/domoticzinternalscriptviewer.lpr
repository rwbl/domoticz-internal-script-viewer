program domoticzinternalscriptviewer;
{
@name(DomoticzInternalScriptViewer)
@shorttitle(Offline viewer for Domoticz internal scripts with focus on dzVents [Easy Events])
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(see umain)
@lastmod(see umain)
}
{REGION NOTES}
{
DomoticzInternalScriptViewer - Copyright (C) 2019-2021 Robert W.B. Linn
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

Developed using Lazarus 2.0.12 (http://www.lazarus-ide.org).
}
{ENDREGION NOTES}

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, umain, udbmgr, uconstants, usettings, udownload;

{$R *.res}

begin
 RequireDerivedFormResource:=True;
  Application.Scaled:=True;
 Application.Initialize;
 // Create the forms in this order!!!
 Application.CreateForm(TDBMgr, DBMgr);
 // Create the mainform after dbmgr
 Application.CreateForm(TFormMain, FormMain);
 //
 Application.CreateForm(TSettings, Settings);
 Application.CreateForm(TFormDownload, FormDownload);
 Application.Run;
end.

