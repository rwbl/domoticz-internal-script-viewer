unit uconstants;
{
@name(uconstants)
@shorttitle(Constants used for the project)
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(20190201)
@lastmod(see uconstants)
}

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils;

const
  VERSION    = 'Domoticz Internal Script Viewer 1.1.0 (Build 20210507)';
  COPYRIGHT  = ''
    +'List the internal scripts from the Domoticz database (table EventMaster) and view the source code.'+LineEnding
    +'The Domoticz database is copied from the Domoticz server by using a shared drive (i.e. Z:).'+LineEnding+LineEnding
    +'(c) 2019-2021 Robert W.B. Linn, Pinneberg, Germany'+LineEnding+LineEnding
    +'Developed with Lazarus 2.0.12 and FPC 3.2.0.'+LineEnding
    +'Database SQLite 3.31.0.'+LineEnding+LineEnding
    +'Application for personal use only under the GNU GENERAL PUBLIC LICENSE Version 3.';

  // Application inifile
  INIFILE              = 'domoticzinternalscriptviewer.ini';
  INISECTIONGENERAL    = 'general';
  INISECTIONSYNEDIT    = 'synedit';

  // Defaults for the domoticz database location and name
  SHAREDDRIVE          = 'Z:\';
  DATABASEFILE         = 'domoticz.db';
  REFRESHATSTART       = False;
  SQLQUERY             = 'SELECT Name,Interpreter,XMLStatement,Status FROM EventMaster WHERE Interpreter="dzVents" ORDER BY Interpreter,Name;';
  FILTER               = 'Interpreter = "dzVents" and Status = 1';

  INTERPRETERBLOCKLY   = 'Blockly';
  INTERPRETERDZVENTS   = 'dzVents';
  INTERPRETERPYTHON    = 'Python';

implementation

end.

