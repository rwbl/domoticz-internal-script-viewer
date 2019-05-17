# Domoticz Internal Script Viewer

# Objectives
* To view offline the Domoticz internal scripts (focus on dzVents scripts), which are created using the web ui event editor
* To export the scripts to the clipboard, text file or HTML file
* To develop the application in Lazarus running on a Windows 10 device

![domoticzinternalscriptviewer-s](https://user-images.githubusercontent.com/47274144/53641943-bab48400-3c30-11e9-9790-5772cf0c17b1.png)

## Solution
On the Domoticz Server (Raspberry Pi 3B+) the folder /home/pi/domoticz is defined as shared folder(samba).
In Windows a network drive is defined (Z: as an example) using the Domoticz shared folder.
The viewer copies, from drive Z:, the Domoticz database, domoticz.db, to the local folder in which the viewer is running.
From the database, table EventMaster, the all records with fields Name, Interpreter, XMLStatement and Status are selected and displayed in a grid.
Selecting a script shows the script code (field XMLStatement) in a textarea.

## Hints
The applications enables to download the Domoticz database from the Domoticz server. It is an option to backup the Domoticz database.

## Software
* Raspberry Pi Raspian Linux 4.14.79-v7+ #1159.
* Domoticz Home Automation System V4.10717
* FPC 3.0.4 and Lazarus 2.0.2

## Install
Create and connect to the shared folder (see next how to).
Install the application in a folder of choice and start ***domoticzinternalscriptviewer.exe***.

#### Domoticz Server Shared Folder
The Raspberry Pi shared folder uses samba and is configured with:
```sudo nano /etc/samba/smb.conf```
Content:
```
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
```
After the samba configuration:
* Ensure to set the directory mask for the domoticz folder to 0777: ```sudo chmod 777 /home/pi/domoticz```
* Restart samba: ```sudo /etc/init.d/samba restart```
* Restart domoticz: ```sudo service domoticz.sh restart```

#### Windows Network Drive Connect to Shared Folder
In Windows connect to the Domoticz server shared folder using the Windows Explorer > Connect Network:
Path for drive Z: is \\192.nnn.n.nn\DoProDomoticz
(DoProDomoticz is defined in the samba configuration)

After this prework, the domoticz database domoticz.db can be copied using CopyFile.
(simple solution using the standard lazarus fileutil functions).

## Settings
The viewer uses following settings(stored in domoticzinternalscriptviewer.set):
* ***shareddrive*** - The Windows shared drive. Default=Z:\.
* ***databasefile*** - Domoticz database file copied and stored locally in the application folder. Default=domoticz.db.
* ***refreshatstart*** - Flag to copy the database file at application start. Default=0.
* ***sqlquery*** - SQL SELECT statement to read the records from table EventMaster. Default=````SELECT Name,Interpreter,XMLStatement,Status FROM EventMaster WHERE Interpreter="dzVents" ORDER BY Interpreter,Name;````

## Disclaimer
THIS APPLICATION IS PROVIDED BY THE AUTHOR “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.
