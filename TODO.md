# Todo domoticz-internal-script-viewer

### UPD: Syntax Highligher dzVents
The syntax highlighter used for dzVents (Lua) scripts is based on the SynEdit SysAnySyn component.
It is not a pure Lua highlighter, but just a workaround to show some highlighting.
Add more dzVents constants, keywords & object.

To create a full dzVents (Lua) Highligher another syntax highlighter component is required.
#### Status
Started exploring by creating a component added to the project as unit which is instantiate.

### NEW: Create Ubuntu Version
Create a version running under Ubuntu 20.04.
#### Status
In progress and almost done.
Open is how to handle the database refresh action, which uses under Windows a shared drive (from the Domoticz server).
Workaround is to copy the Domoticz database using Ubuntu file manger after accessing the shared drive (connect).

### NEW: HTTP Download Domoticz Database
Alternative solution to access the latest version of the Domoticz database.
So far the database is accessed using shared drive. 
Explore how to HTTP download from the Domoticz parent folder, i.e. http://domoticz-ip:8080/../domoticz.db.
This is required because http://domoticz-ip:8080 points to the domoticz/www folder which does not contain the Domoticz database.
The Domoticz database is in folder /home/pi/domoticz.
#### Status
Not found a final solution.
A workaround could be to copy once a day the file domoticz.db to the domoticz/www folder.
Then download via http://domoticz-ip:8080/domoticz.db.
