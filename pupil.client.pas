{
  pupil-fpc
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0)

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Pupil.Client;

{$mode objfpc}{$H+}

// {$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, SimpleMsgPack, ZMQ.Client, Pupil.Types,
  session.constants;

type

  { TPupilClient }

  TPupilClient = class(TZMQReqThread)
    private
      FSerializer : TSimpleMsgPack;
      FSubPort : string;
      FLocalIP : string;
      FZMQSubThread : TZMQSubThread;
      FOnReceiveReply : TNotifyReply;
      {$IFDEF DEBUG}
      procedure DumpDictionary(DecodedMessagePackage : TSimpleMsgPack);
      {$ENDIF}
      procedure ReceiveSubPort(AResponse: string);
      procedure ReceivePubPort(AResponse: string);
      procedure ReceiveReply(Sender: TObject; ARequest, AReply: string);
      procedure SubscriberTerminated(Sender : TObject);
      procedure Subscribe(ASub : string);
      procedure UnSubscribe(ASub : string);
    private
      function GetOnCalibrationFailed: TNotifyEvent;
      function GetOnCalibrationStopped: TNotifyEvent;
      function GetOnCalibrationSuccessful: TNotifyEvent;
      function GetOnGazeOnSurface: TGazeOnSurfaceEvent;
      function GetOnRecordingStarted: TNotifyEvent;
      procedure SetOnCalibrationFailed(AValue: TNotifyEvent);
      procedure SetOnCalibrationStopped(AValue: TNotifyEvent);
      procedure SetOnCalibrationSuccessful(AValue: TNotifyEvent);
      procedure SetOnGazeOnSurface(AValue: TGazeOnSurfaceEvent);
      procedure SetOnRecordingStarted(AValue: TNotifyEvent);
    public
      constructor Create(AHost : string = DefaultPupilAddress; CreateSuspended: Boolean = True);
      destructor Destroy; override;
      procedure Close; override;
      procedure StartSubscriber(Blocking : Boolean = True);
      procedure Request(AReq : string; Blocking : Boolean = False);
    public
      property OnCalibrationFailed : TNotifyEvent
        read GetOnCalibrationFailed write SetOnCalibrationFailed;
      property OnCalibrationSuccessful : TNotifyEvent
        read GetOnCalibrationSuccessful write SetOnCalibrationSuccessful;
      property OnCalibrationStopped : TNotifyEvent
        read GetOnCalibrationStopped write SetOnCalibrationStopped;
      property OnRecordingStarted : TNotifyEvent
        read GetOnRecordingStarted write SetOnRecordingStarted;
      property OnReplyReceived : TNotifyReply
        read FOnReceiveReply write FOnReceiveReply;
      property OnGazeOnSurface : TGazeOnSurfaceEvent
        read GetOnGazeOnSurface write SetOnGazeOnSurface;
  end;

var
  PupilClient : TPupilClient;

implementation

uses Forms;

const
  // return the current publisher's port of the IPC Backbone
  REQ_PUB_PORT = 'PUB_PORT';

  // return the current subscriber's port of the IPC Backbone
  REQ_SUB_PORT = 'SUB_PORT';

{ TPupilClient }

constructor TPupilClient.Create(AHost: string; CreateSuspended: Boolean);
begin
  FSerializer := TSimpleMsgPack.Create;
  FLocalIP := Copy(AHost,1, pos(':', AHost));
  FSubPort := '';
  FreeOnTerminate := True;
  inherited Create(AHost, CreateSuspended);
  OnReceiveReply := @ReceiveReply;
end;

destructor TPupilClient.Destroy;
begin
  if Assigned(FZMQSubThread) then begin
    FZMQSubThread.Close;
  end;
  FSerializer.Free;
  inherited Destroy;
end;

procedure TPupilClient.Close;
begin
  OnGazeOnSurface := nil;
  FZMQSubThread.Close;
  inherited Close;
end;

procedure TPupilClient.Request(AReq: string; Blocking: Boolean);
begin
  SendRequest(AReq, Blocking);
end;

procedure TPupilClient.Subscribe(ASub: string);
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(ASub);
end;

procedure TPupilClient.StartSubscriber(Blocking: Boolean);
begin
  Request(REQ_SUB_PORT, Blocking);
end;

procedure TPupilClient.UnSubscribe(ASub: string);
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(ASub);
end;

{$IFDEF DEBUG}
procedure TPupilClient.DumpDictionary(DecodedMessagePackage: TSimpleMsgPack);
var j : integer;
begin
  with DecodedMessagePackage do
    for j := 0 to Count -1 do
      begin;
        WriteLn('[debug]', #32, Items[j].Key, ':', Items[j].Value);
      end;
end;
{$ENDIF}

procedure TPupilClient.ReceiveSubPort(AResponse: string);
var SubHost : string;
begin
  if FSubPort = '' then
    begin
      SubHost := FLocalIP + AResponse;
      {$ifdef DEBUG}
        WriteLn('[debug]', #32, 'SubHost:', #32, SubHost);
      {$endif};
      FZMQSubThread := TZMQSubThread.Create(SubHost);
      with FZMQSubThread do begin
        OnTerminate := @SubscriberTerminated;
        Start;
      end;
      FSubPort := AResponse;
    end;
end;

procedure TPupilClient.ReceivePubPort(AResponse: string);
begin
  raise Exception.Create( ERROR_NOT_IMPLEMENTED + REQ_PUB_PORT + #32 + Self.ClassName + '#32' + AResponse);
  //SendRequest(REQUEST_PUB_PORT);
  { TODO: publish to the pupil ipc backbone }
end;

procedure TPupilClient.ReceiveReply(Sender: TObject; ARequest, AReply: string);
begin
  {$IFDEF DEBUG}
  WriteLn('[debug]', #32, ARequest, #32, AReply);
  {$ENDIF}
  case ARequest of
    REQ_SHOULD_START_RECORDING, REQ_SHOULD_STOP_RECORDING,
    REQ_SHOULD_START_CALIBRATION, REQ_SHOULD_STOP_CALIBRATION,
    REQ_TIMESTAMP : if Assigned(OnReplyReceived) then OnReplyReceived(Self, ARequest, AReply);
    REQ_SUB_PORT : ReceiveSubPort(AReply);
    REQ_PUB_PORT : ReceivePubPort(AReply);
    else
      if Pos(REQ_SYNCHRONIZE_TIME, ARequest) <> 0 then begin
        if Assigned(OnReplyReceived) then
          OnReplyReceived(Self, ARequest, AReply);
      end else begin
        raise Exception.Create(
          ERROR_UNKNOWN_COMMAND + ARequest + #32 + Self.ClassName );
      end;
  end;
end;

procedure TPupilClient.SubscriberTerminated(Sender: TObject);
begin
  //FSubPort := '';
end;

function TPupilClient.GetOnCalibrationFailed: TNotifyEvent;
begin
  Result := FZMQSubThread.OnCalibrationFailed;
end;

function TPupilClient.GetOnCalibrationStopped: TNotifyEvent;
begin
  Result := FZMQSubThread.OnCalibrationStopped;
end;

function TPupilClient.GetOnCalibrationSuccessful: TNotifyEvent;
begin
  Result := FZMQSubThread.OnCalibrationSuccessful;
end;

function TPupilClient.GetOnGazeOnSurface: TGazeOnSurfaceEvent;
begin
  Result := FZMQSubThread.OnGazeOnSurface;
end;

function TPupilClient.GetOnRecordingStarted: TNotifyEvent;
begin
  Result := FZMQSubThread.OnRecordingStarted;
end;

procedure TPupilClient.SetOnCalibrationFailed(AValue: TNotifyEvent);
begin
  with FZMQSubThread do begin
    if OnCalibrationFailed = AValue then Exit;
    OnCalibrationFailed := AValue;
  end;

  if AValue <> nil then begin
    Subscribe(NOTIFY_CALIBRATION_FAILED);
  end else begin
    UnSubscribe(NOTIFY_CALIBRATION_FAILED);
  end;
end;

procedure TPupilClient.SetOnCalibrationStopped(AValue: TNotifyEvent);
begin
  with FZMQSubThread do begin
    if OnCalibrationStopped = AValue then Exit;
    OnCalibrationStopped := AValue;
  end;

  if AValue <> nil then begin
    Subscribe(NOTIFY_CALIBRATION_STOPPED);
  end else begin
    UnSubscribe(NOTIFY_CALIBRATION_STOPPED);
  end;
end;

procedure TPupilClient.SetOnCalibrationSuccessful(AValue: TNotifyEvent);
begin
  with FZMQSubThread do begin
    if OnCalibrationSuccessful = AValue then Exit;
    OnCalibrationSuccessful := AValue;
  end;

  if AValue <> nil then begin
    Subscribe(NOTIFY_CALIBRATION_SUCCESSFUL);
  end else begin
    UnSubscribe(NOTIFY_CALIBRATION_SUCCESSFUL);
  end;
end;

procedure TPupilClient.SetOnGazeOnSurface(AValue: TGazeOnSurfaceEvent);
var
  i: Integer;
begin
  with FZMQSubThread do begin
    if OnGazeOnSurface = AValue then Exit;
    if AValue <> nil then begin
      OnGazeOnSurface := AValue;
      Subscribe(SUB_SURFACES_EVENT);
    end else begin
      UnSubscribe(SUB_SURFACES_EVENT);
      for i := 0 to 10 do begin
        Application.ProcessMessages;
        Sleep(100);
      end;
      OnGazeOnSurface := AValue;
    end;
  end;
end;

procedure TPupilClient.SetOnRecordingStarted(AValue: TNotifyEvent);
begin
  with FZMQSubThread do begin
    if OnRecordingStarted = AValue then Exit;
    OnRecordingStarted := AValue;
  end;

  if AValue <> nil then begin
    Subscribe(NOTIFY_RECORDING_STARTED);
  end else begin
    UnSubscribe(NOTIFY_RECORDING_STARTED);
  end;
end;


end.
