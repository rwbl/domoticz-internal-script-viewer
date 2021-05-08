unit umisc;
{
@name(umisc)
@shorttitle(Global  helper routines)
@author(Robert W.B. Linn)
@seealso(umain, readme.txt)
@created(see umain)
@lastmod(see umain)
}
{REGION NOTES}
{
More options in progress for a next version of Look4How.
}
{ENDREGION NOTES}

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,
 typinfo,
 Clipbrd,
 comctrls,
 strutils,
 Graphics,            // TColor
 Forms,               // Application.MessageBox
 LCLIntf,             // PredefinedClipboardFormat(
 LCLType              // MB_nnnnn
 ;

{REGION FUNCIONS}
function BoolToString(AState: Boolean): String;
function BoolToYN(AState: Boolean): String;
function BoolToText(AState: Boolean; ATrueText, AFalseText: String): String;
function TabToSpace(AString: String; ANumSpaces: Integer): String;

function ColorToHTML(const AColor: TColor): String;

procedure CopyToClipboard(AString: String);
function PasteFromClipboard: String;
procedure ListViewToClipboard(AListView: TListView);
function FileToClipboard(AFileName:String; ADelete:Boolean):Boolean;

procedure StringToStringList(const ADelimiter: Char; AInput: string; const AStrings: TStrings);
function ReplaceAllIgnoreCase(AString, AOldValue, ANewValue: String): String;

procedure ShowInfo(ACaption, AMessage: String);
procedure ShowWarning(ACaption, AMessage: String);
procedure ShowError(ACaption, AMessage: String);
{ENDREGION FUNCIONS}

implementation

{REGION CONVERSION}
// Convert a boolean True|False to text string True|False
// Return True|False
function BoolToString(AState: Boolean): String;
begin
  Result := 'True';
  if NOT AState then Result := 'False';
end;

// Convert a boolean True|False to text string Yes|No
// Return Yes|No
function BoolToYN(AState: Boolean): String;
begin
  Result := 'Yes';
  if NOT AState then Result := 'No';
end;

// Convert a boolean True|False to text string TrueText|FalseText
// Set the text in the parameter TrueText|FalseText
// Return TrueText|FalseText
function BoolToText(AState: Boolean; ATrueText, AFalseText: String): String;
begin
  Result := ATrueText;
  if NOT AState then Result := AFalseText;
end;

// Convert a string to a stringlist
// Example:
// vContent := vContent.Replace(LineEnding, #10, [rfReplaceAll]);
// StringToStringList(#10, vContent, Lines);
// Returns a StringList
procedure StringToStringList(const ADelimiter: Char; AInput: string; const AStrings: TStrings);
begin
 Assert(Assigned(AStrings)) ;
 AStrings.Clear;
 AStrings.StrictDelimiter := true;
 AStrings.Delimiter := ADelimiter;
 AStrings.DelimitedText := AInput;
end;

// Replace all occurencies ignoring case
// Return Converted string
function ReplaceAllIgnoreCase(AString, AOldValue, ANewValue: String): String;
begin
 Result := AString.Replace(AOldValue, ANewValue, [rfReplaceAll, rfIgnoreCase]);
end;

// Convert tab characters to spaces
// Return Converted string
function TabToSpace(AString: String; ANumSpaces: Integer): String;
begin
  Result := Tab2Space(AString, ANumSpaces);
end;

{ENDREGION CONVERSION}

{REGION COLORS}
// Convert TColor to HTML HEX string #RRGGBB
// Returns #RRGGBB
function ColorToHTML(const AColor: TColor): String;
begin
  Result := '#' + HexStr(ColorToRGB(AColor), 6);
end;
{ENDREGION COLORS}

{REGION CLIPBOARD}
// Copy text string to the clipboard.
// The clipboard content is cleared prior copy.
procedure CopyToClipboard(AString: String);
begin
  Clipboard.Clear;
  Clipboard.SetTextBuf(PChar(AString));
end;

// Paste text string from the clipboard.
// The clipboard content is checked prior pasting.
// Returns the clipboard content as a string else empty string
function PasteFromClipboard: String;
begin
  Result := '';
  if not Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) then
  begin
    Result := 'The clipboard content has no text format.';
    Exit;
  end;
  Result := Clipboard.AsText;
end;

procedure ListViewToClipboard(AListView: TListView);
var
  StringList: TStringList;
  RowNr: Integer;
  ListItem : TListItem;
  TempStr: String;
  i: Integer;
begin
  // Do nothing if no items
  if AListView.Items.Count = 0 then Exit;
  // Build the stringlist
  StringList := TStringList.Create;
  // Loop over the rwos of the listview
  for RowNr := 0 to AListView.Items.Count - 1 do
  begin
    // Get the listitem
    ListItem := AListView.Items[RowNr];
    // Build tempstring containing the caption and subitems
    TempStr := ListItem.Caption;
    for i := 0 to ListItem.SubItems.Count - 1 do
    begin
      TempStr := TempStr + ','+ ListItem.SubItems[i];
    end;
    // Add the tempstring to the list
    StringList.Add(TempStr);
  end;
  // Copy to the clipboard
  CopyToClipboard(StringList.Text);
end;

// Copy  content of a text file to the clipboard.
// Option to delete the file.
function FileToClipboard(AFileName:String; ADelete:Boolean):Boolean;
var
  Strings: TStringList;
begin
  Result := False;
  try
    Strings := TStringList.Create;
    Strings.LoadFromFile(AFileName);
    CopyToClipboard(Strings.Text);
    if ADelete then
    begin
      DeleteFile(AFileName);
    end;
    Result := True;
  except
    on E: Exception do
    begin
      // E.Message
      Exit;
    end;
  end;
end;
{ENDREGION CLIPBOARD}

{REGION MESSAGES}
procedure ShowInfo(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONINFORMATION)
end;

procedure ShowWarning(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONWARNING)
end;

procedure ShowError(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONERROR)
end;
{ENDREGION MESSAGES}

end.

