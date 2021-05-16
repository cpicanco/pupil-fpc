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

uses SysUtils, ZMQ.Client, SimpleMsgPack;

type

  { TPupilMessage }

  TPupilMessage = record
    Topic : string;
    Payload : TSimpleMsgPack;
  end;

  { TNotifyMultipartMessage }

  TNotifyMultipartMessage = procedure(Sender: TObject; AMultiPartMessage : TPupilMessage) of object;

  { TNotifyReply }

  TNotifyReply = procedure(Sender: TObject; ARequest, AResponse: String) of object;

  { TPupilClient }

  TPupilClient = class(TZMQReqThread)
    private
      FSubPort : string;
      FLocalIP : string;
      FZMQSubThread : TZMQSubThread;
      {$IFDEF DEBUG}
      procedure DumpDictionary(DecodedMessagePackage : TSimpleMsgPack);
      {$ENDIF}
      procedure ReceiveSubPort(AResponse: string);
      procedure ReceivePubPort(AResponse: string);
      procedure ReceiveReply(ARequest, AReply: string);
      procedure ReceiveMultipartMessage(AMultipartMessage : TMultiPartMessage);
      procedure SubscriberTerminated(Sender : TObject);
    private
      FOnCalibrationFailed: TNotifyMultipartMessage;
      FOnCalibrationStopped: TNotifyMultipartMessage;
      FOnCalibrationSuccessful: TNotifyMultipartMessage;
      FOnRecordingStarted: TNotifyMultipartMessage;
      FOnReplyReceived : TNotifyReply;
      FOnMultipartMessageReceived : TNotifyMultipartMessage;
      procedure SetOnCalibrationFailed(AValue: TNotifyMultipartMessage);
      procedure SetOnCalibrationStopped(AValue: TNotifyMultipartMessage);
      procedure SetOnCalibrationSuccessful(AValue: TNotifyMultipartMessage);
      procedure SetOnMultiPartMessageReceived(AValue: TNotifyMultipartMessage);
      procedure SetOnRecordingStarted(AValue: TNotifyMultipartMessage);
      procedure SetOnReplyReceived(AValue: TNotifyReply);
    public
      constructor Create(AHost : string; CreateSuspended: Boolean = True);
      destructor Destroy; override;
      procedure StartSubscriber(Blocking : Boolean = True);
      procedure Subscribe(ASub : string);
      procedure Request(AReq : string; Blocking : Boolean = False);
      procedure UnSubscribe(ASub : string);
    public
      property OnCalibrationFailed : TNotifyMultipartMessage read FOnCalibrationFailed write SetOnCalibrationFailed;
      property OnCalibrationSuccessful : TNotifyMultipartMessage read FOnCalibrationSuccessful write SetOnCalibrationSuccessful;
      property OnCalibrationStopped : TNotifyMultipartMessage read FOnCalibrationStopped write SetOnCalibrationStopped;
      property OnRecordingStarted : TNotifyMultipartMessage read FOnRecordingStarted write SetOnRecordingStarted;
      property OnReplyReceived : TNotifyReply read FOnReplyReceived write SetOnReplyReceived;
      property OnMultiPartMessageReceived : TNotifyMultipartMessage read FOnMultiPartMessageReceived write SetOnMultiPartMessageReceived;
  end;

var
  PupilClient : TPupilClient;

const
  // start recording with auto generated session name
  // note: may append a string to session name, 'R [session name]'
  REQ_SHOULD_START_RECORDING  = 'R';

  // stop recording
  REQ_SHOULD_STOP_RECORDING = 'r';

  // start currently selected calibration
  REQ_SHOULD_START_CALIBRATION = 'C';

  // stop currently selected calibration
  REQ_SHOULD_STOP_CALIBRATION = 'c';

  // '[T][#32][time]' make pupil timestamps count from [time] on.
  REQ_SYNCHRONIZE_TIME = 'T';

  // get pupil capture timestamp returns a float as string.
  REQ_TIMESTAMP = 't';

  // request recording path
  REQ_RECORDING_PATH = 'P';

const
  SUB_GAZE_DATA = 'gaze';
  SUB_EYE_CAMERA_0 = 'pupil.0';
  SUB_EYE_CAMERA_1 = 'pupil.1';

  SUB_ALL_LOGGING = 'logging.';
  SUB_LOGGING_INFO = SUB_ALL_LOGGING + 'info';
  SUB_LOGGING_ERROR = SUB_ALL_LOGGING + 'error';
  SUB_LOGGING_WARNING = SUB_ALL_LOGGING + 'warning';

  SUB_TIME_SYNC = 'time_sync.';

  SUB_ALL_GROUPS = 'groups.';

const
  SUB_ALL_NOTIFICATIONS = 'notify.';

  NOTIFY_RECORDING_SHOULD_START = SUB_ALL_NOTIFICATIONS + 'recording.should_start';
  NOTIFY_RECORDING_SHOULD_STOP = SUB_ALL_NOTIFICATIONS + 'recording.should_stop';
  NOTIFY_RECORDING_STARTED = SUB_ALL_NOTIFICATIONS + 'recording.started';
  NOTIFY_RECORDING_STOPPED = SUB_ALL_NOTIFICATIONS + 'recording.stopped';

  NOTIFY_CALIBRATION_SHOULD_START = SUB_ALL_NOTIFICATIONS + 'calibration.should_start';
  NOTIFY_CALIBRATION_SHOULD_STOP = SUB_ALL_NOTIFICATIONS + 'calibration.should_stop';
  NOTIFY_CALIBRATION_STARTED = SUB_ALL_NOTIFICATIONS + 'calibration.started';
  NOTIFY_CALIBRATION_STOPPED = SUB_ALL_NOTIFICATIONS + 'calibration.stopped';
  NOTIFY_CALIBRATION_FAILED = SUB_ALL_NOTIFICATIONS + 'calibration.failed';
  NOTIFY_CALIBRATION_SUCCESSFUL = SUB_ALL_NOTIFICATIONS + 'calibration.successful';

  // 'notify.eye_process.stopped';
  // 'notify.eye_process.should_start.0'
  // 'notify.eye_process.should_start.1'

const
  KEY_SUBJECT = 'subject';
  KEY_RECORDING_PATH = 'rec_path';
  KEY_SESSION_NAME = 'session_name';
  KEY_RECORD_EYE = 'record_eye';
  KEY_COMPRESSION = 'compression';


resourcestring
  ERROR_UNKNOWN_COMMAND = 'Commando Pupil desconhecido:';
  ERROR_NOT_IMPLEMENTED = 'Commando Pupil não implementado:';
  //ERROR_UNKNOWN_COMMAND = 'Unknown Pupil command:';
  //ERROR_NOT_IMPLEMENTED = 'Unimplemented Pupil command:';

implementation

const
  // return the current publisher's port of the IPC Backbone
  REQ_PUB_PORT = 'PUB_PORT';

  // return the current subscriber's port of the IPC Backbone
  REQ_SUB_PORT = 'SUB_PORT';

{ TPupilClient }

constructor TPupilClient.Create(AHost: string; CreateSuspended: Boolean);
begin
  FLocalIP := Copy(AHost,1, pos(':', AHost));
  FSubPort := '';
  inherited Create(AHost, CreateSuspended);
  OnReceiveReply := @ReceiveReply;
end;

destructor TPupilClient.Destroy;
begin
  OnReplyReceived := nil;
  if Assigned(FZMQSubThread) then FZMQSubThread.Terminate;
  inherited Destroy;
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
      with FZMQSubThread do
        begin;
          OnTerminate := @SubscriberTerminated;
          OnMultiPartMessageReceived := @ReceiveMultipartMessage;
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

procedure TPupilClient.ReceiveReply(ARequest, AReply: string);
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
      if Pos(REQ_SYNCHRONIZE_TIME, ARequest) <> 0 then
        begin
          if Assigned(OnReplyReceived) then OnReplyReceived(Self, ARequest, AReply);
        end
      else raise Exception.Create( ERROR_UNKNOWN_COMMAND + ARequest + #32 + Self.ClassName );
  end;
end;

procedure TPupilClient.ReceiveMultipartMessage(AMultipartMessage: TMultiPartMessage);
var
  Serializer :  TSimpleMsgPack;
  PupilMessage : TPupilMessage;
begin
  PupilMessage.Topic := AMultipartMessage.MsgTopic;
  {$ifdef DEBUG}
    WriteLn('[debug]', #32, PupilMessage.Topic)
  {$endif};

  AMultipartMessage.MsgPackage.Position := 0;
  Serializer := TSimpleMsgPack.Create;
  try
    Serializer.Clear;
    Serializer.DecodeFromStream(AMultipartMessage.MsgPackage);
    {$ifdef DEBUG}
      WriteLn('[debug]', #32, 'TopicCount:', IntToStr(Serializer.Count));
      DumpDictionary(Serializer);
    {$endif};
    PupilMessage.Payload := Serializer;
    case PupilMessage.Topic of
      NOTIFY_RECORDING_STARTED :
        if Assigned(OnRecordingStarted) then OnRecordingStarted(Self, PupilMessage);
      NOTIFY_CALIBRATION_STOPPED :
        if Assigned(OnCalibrationStopped) then OnCalibrationStopped(Self, PupilMessage);
      NOTIFY_CALIBRATION_SUCCESSFUL :
        if Assigned(OnCalibrationSuccessful) then OnCalibrationSuccessful(Self, PupilMessage);
      NOTIFY_CALIBRATION_FAILED:
        if Assigned(OnCalibrationFailed) then OnCalibrationFailed(Self, PupilMessage);
    else
      if Assigned(OnMultiPartMessageReceived) then OnMultiPartMessageReceived(Self, PupilMessage);
    end;

  finally
    Serializer.Free;
    AMultipartMessage.MsgPackage.Free;
  end;
end;

procedure TPupilClient.SubscriberTerminated(Sender: TObject);
begin
  FSubPort := '';
end;

procedure TPupilClient.SetOnMultiPartMessageReceived(
  AValue: TNotifyMultipartMessage);
begin
  if FOnMultiPartMessageReceived = AValue then Exit;
  FOnMultiPartMessageReceived := AValue;
end;

procedure TPupilClient.SetOnCalibrationStopped(
  AValue: TNotifyMultipartMessage);
begin
  if FOnCalibrationStopped = AValue then Exit;
  FOnCalibrationStopped := AValue;
end;

procedure TPupilClient.SetOnCalibrationFailed(AValue: TNotifyMultipartMessage);
begin
  if FOnCalibrationFailed=AValue then Exit;
  FOnCalibrationFailed:=AValue;
end;

procedure TPupilClient.SetOnCalibrationSuccessful(
  AValue: TNotifyMultipartMessage);
begin
  if FOnCalibrationSuccessful=AValue then Exit;
  FOnCalibrationSuccessful:=AValue;
end;

procedure TPupilClient.SetOnRecordingStarted(
  AValue: TNotifyMultipartMessage);
begin
  if FOnRecordingStarted = AValue then Exit;
  FOnRecordingStarted := AValue;
end;

procedure TPupilClient.SetOnReplyReceived(AValue: TNotifyReply);
begin
  if FOnReplyReceived = AValue then Exit;
  FOnReplyReceived := AValue;
end;


end.
