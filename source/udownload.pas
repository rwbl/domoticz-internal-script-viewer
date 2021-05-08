unit udownload;
{
 HTTP Download.
 Developed & Credits to forum member GetMem - https://forum.lazarus.freepascal.org/index.php?topic=40812.0

 Example usage in main form:
 procedure TFormMain.ToolButton16Click(Sender: TObject);
 begin
      FormDownload.Show
 end;

 Status: ON HOLD - see next ToDo.
}
{ TODO 3 -cDownload : Explore how to download from parent folder, i.e. http://domoticz-ip:8080/../domoticz.db. This is required because http://domoticz-ip:8080 points to the domoticz/www folder which does not contain the Domoticz database. }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fphttpclient;

const
  DOWNLOAD_URL = 'http://192.168.1.60:6144/domoticz.db';
  DOWNLOAD_TO  = 'domoticz.db';

type

  { TFormDownload }

  TFormDownload = class(TForm)
    ButtonDownload: TButton;
    LabelStatus: TLabel;
    procedure ButtonDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHTTPClient: TFPHTTPClient;
    procedure Download(AFrom, ATo: String);
    procedure DoOnWriteStream(Sender: TObject; APosition: Int64);
    function FormatSize(Size: Int64): String;
  public

  end;

var
  FormDownload: TFormDownload;

implementation

{$R *.lfm}


type

  { TDownloadStream }

  TOnWriteStream = procedure(Sender: TObject; APos: Int64) of object;
  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    procedure DoProgress;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
  end;

{ TFormDownload }


{ TDownloadStream }

constructor TDownloadStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FStream.Position := 0;
end;

destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDownloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

function TDownloadStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TDownloadStream.DoProgress;
begin
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Self.Position);
end;

{ TFormDownload }

procedure TFormDownload.Download(AFrom, ATo: String);
var
  DS: TDownloadStream;
begin
  DS := TDownloadStream.Create(TFileStream.Create(ATo, fmCreate));
  try
    DS.FOnWriteStream := @DoOnWriteStream;
    try
      FHTTPClient.HTTPMethod('GET', AFrom, DS, [200]);
    except
      on E: Exception do
      begin
        ShowMessage(e.Message)
      end;
    end;
  finally
    DS.Free
  end;
end;

function TFormDownload.FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;


procedure TFormDownload.DoOnWriteStream(Sender: TObject; APosition: Int64);
begin
  LabelStatus.Caption := 'Downloaded: ' + FormatSize(APosition);
  Application.ProcessMessages;
end;

procedure TFormDownload.FormCreate(Sender: TObject);
begin
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

procedure TFormDownload.FormDestroy(Sender: TObject);
begin
  FHTTPClient.Free;;
end;

procedure TFormDownload.ButtonDownloadClick(Sender: TObject);
begin
  Download(DOWNLOAD_URL, DOWNLOAD_TO);
  LabelStatus.Caption := 'Done';
end;


end.


