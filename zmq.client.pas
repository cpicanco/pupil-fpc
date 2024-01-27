{
  pupil-fpc
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0)

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit ZMQ.Client;

{$mode objfpc}{$H+}

interface

uses SimpleMsgPack, Classes, Pupil.Types;

type

  { TZMQSubThread }

  TZMQSubThread = class(TThread)
  private
    FGazeEvent : TGazeOnSurface;
    FPupilPayload : TSimpleMsgPack;
    FMemoryStream : TMemoryStream;
    FContext : Pointer;
    FSubscriber : Pointer;
    FOnGazeOnSurface: TGazeOnSurfaceEvent;
    FOnCalibrationFailed: TNotifyEvent;
    FOnCalibrationStopped: TNotifyEvent;
    FOnCalibrationSuccessful: TNotifyEvent;
    FOnRecordingStarted: TNotifyEvent;
    FCriticalSection : TRTLCriticalSection;
    procedure GazeOnSurface;
    procedure CalibrationFailed;
    procedure CalibrationSuccessful;
    procedure CalibrationStopped;
    procedure RecordingStarted;
  protected
    procedure Execute; override;
  public
    constructor Create(ASubHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Subscribe(AFilter : string);
    procedure Unsubscribe(AFilter : string);
    procedure Close;
    property OnGazeOnSurface : TGazeOnSurfaceEvent read FOnGazeOnSurface write FOnGazeOnSurface;
    property OnCalibrationFailed : TNotifyEvent read FOnCalibrationFailed write FOnCalibrationFailed;
    property OnRecordingStarted : TNotifyEvent read FOnRecordingStarted write FOnRecordingStarted;
    property OnCalibrationStopped : TNotifyEvent read FOnCalibrationStopped write FOnCalibrationStopped;
    property OnCalibrationSuccessful : TNotifyEvent read FOnCalibrationSuccessful write FOnCalibrationSuccessful;
  end;

  { TZMQReqThread }

  TZMQReqThread = class(TThread)
  private
    FReply   : string;
    FRequest : string;
    FContext : Pointer;
    FRequester : Pointer;
    FRTLEvent: PRTLEvent;
    FOnReceiveReply: TNotifyReply;
    procedure ReceiveReplyEvent; inline;
  protected
    procedure Execute; override;
    procedure SendRequest(ARequest : string; Blocking : Boolean = False);
    property OnReceiveReply: TNotifyReply read FOnReceiveReply write FOnReceiveReply;
  public
    constructor Create(AHost : string; CreateSuspended: Boolean = True);
    procedure Close; virtual;
    destructor Destroy; override;
  end;

implementation

uses zmq, zmq.helpers;

{ TZMQSubThread }

procedure TZMQSubThread.GazeOnSurface;
var
  LEvent: TGazeOnSurface;
begin
  EnterCriticalSection(FCriticalSection);
  LEvent := FGazeEvent;
  LeaveCriticalSection(FCriticalSection);
  FOnGazeOnSurface(Self, LEvent);
end;

procedure TZMQSubThread.CalibrationFailed;
begin
  FOnCalibrationFailed(Self)
end;

procedure TZMQSubThread.CalibrationSuccessful;
begin
  FOnCalibrationSuccessful(Self)
end;

procedure TZMQSubThread.CalibrationStopped;
begin
  FOnCalibrationStopped(Self)
end;

procedure TZMQSubThread.RecordingStarted;
begin
  FOnRecordingStarted(Self)
end;

procedure TZMQSubThread.Execute;
var
  zmq_message : zmq_msg_t;
  LPupilTopic : string;

  function GetGazeOnSurface(APayload : TSimpleMsgPack) : TGazeOnSurface;
  var
    LGazeOnSurface : TSimpleMsgPack;
    LGaze : TSimpleMsgPack;
    i : integer;
  begin
    with Result do begin
      Name := APayload.S['name'];
      WordFrameTimestamp := APayload.D['timestamp'];
      LGazeOnSurface := APayload.O['gaze_on_surfaces'];
      SetLength(Gazes, LGazeOnSurface.Count);
      for i := Low(Gazes) to High(Gazes) do begin
        LGaze := LGazeOnSurface[i].O['norm_pos'];
        Gazes[i].X := LGaze[0].AsFloat;
        Gazes[i].Y := LGaze[1].AsFloat;
      end;
    end;
  end;
begin
  while not Terminated do begin
    zmq_message := Default(zmq_msg_t);
    zmq_msg_init(zmq_message);
    try
      // frame 1 is a string
      zmq_msg_recv(zmq_message, FSubscriber, 0);
      SetString(LPupilTopic, PAnsiChar(zmq_msg_data(zmq_message)), zmq_msg_size(zmq_message));

      // frame 2 is MsgPack serialization
      if zmq_msg_more(zmq_message) = 1 then
      begin
        zmq_msg_close(zmq_message);
        zmq_msg_init(zmq_message);
        zmq_msg_recv(zmq_message, FSubscriber, 0);
        FMemoryStream.Clear;
        FPupilPayload.Clear;

        FMemoryStream.WriteBuffer(zmq_msg_data(zmq_message)^, zmq_msg_size(zmq_message));
        FMemoryStream.Position := 0;
        FPupilPayload.DecodeFromStream(FMemoryStream);
        {$ifdef DEBUG}
          WriteLn('[debug]', #32, 'TopicCount:', IntToStr(FSerializer.Count));
          DumpDictionary(FSerializer);
        {$endif};
      end;

      case LPupilTopic of
        NOTIFY_RECORDING_STARTED : begin
          Queue(@RecordingStarted);
        end;

        NOTIFY_CALIBRATION_STOPPED : begin
          Queue(@CalibrationStopped);
        end;

        NOTIFY_CALIBRATION_SUCCESSFUL : begin
          Queue(@CalibrationSuccessful);
        end;

        NOTIFY_CALIBRATION_FAILED : begin
          Queue(@CalibrationFailed);
        end;

        SURFACES_SCREEN: begin
          EnterCriticalSection(FCriticalSection);
          FGazeEvent := GetGazeOnSurface(FPupilPayload);
          LeaveCriticalSection(FCriticalSection);
          Queue(@GazeOnSurface);
        end;
      end;
      //Synchronize(@MultipartMessageReceived);

    finally
      zmq_msg_close(zmq_message);
    end;
  end;
end;

constructor TZMQSubThread.Create(ASubHost: string; CreateSuspended: Boolean);
var
  host : PChar;
begin
  InitCriticalSection(FCriticalSection);
  FPupilPayload := TSimpleMsgPack.Create;
  FMemoryStream := TMemoryStream.Create;

  host := PChar('tcp://' + ASubHost);
  FreeOnTerminate := True;

  FContext := zmq_ctx_new;
  FSubscriber := zmq_socket(FContext, ZMQ_SUB);
  zmq_connect(FSubscriber, host);
  inherited Create(CreateSuspended);
end;

destructor TZMQSubThread.Destroy;
begin
  FPupilPayload.Free;
  FMemoryStream.Free;
  zmq_ctx_shutdown(FContext);
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TZMQSubThread.Subscribe(AFilter: string);
begin
  if AFilter = '' then
    zmq_setsockopt(FSubscriber, ZMQ_SUBSCRIBE, nil, 0)
  else
    zmq_setsockopt(FSubscriber, ZMQ_SUBSCRIBE, @AFilter[1], Length(AFilter));
end;

procedure TZMQSubThread.Unsubscribe(AFilter: string);
begin
  if AFilter = '' then
    zmq_setsockopt(FSubscriber, ZMQ_UNSUBSCRIBE, nil, 0)
  else
    zmq_setsockopt(FSubscriber, ZMQ_UNSUBSCRIBE, @AFilter[1], Length(AFilter));
end;

procedure TZMQSubThread.Close;
begin
  zmq_close(FSubscriber);
  Terminate;
end;

{ TZMQReqThread }

constructor TZMQReqThread.Create(AHost: string; CreateSuspended: Boolean);
var
  host : PChar;
begin
  host := PChar('tcp://' + AHost);
  FReply := '';
  FRequest := '';
  FreeOnTerminate := True;
  FRTLEvent := RTLEventCreate;

  FContext := zmq_ctx_new;
  FRequester := zmq_socket(FContext, ZMQ_REQ);
  zmq_connect(FRequester, host);
  inherited Create(CreateSuspended);
end;

procedure TZMQReqThread.Close;
begin
  FRequest := 'Terminate';
  RTLEventSetEvent(FRTLEvent);
  RTLEventDestroy(FRTLEvent);
  Terminate;
end;

destructor TZMQReqThread.Destroy;
begin
  zmq_close(FRequester);
  zmq_ctx_shutdown(FContext);
  inherited Destroy;
end;

procedure TZMQReqThread.ReceiveReplyEvent;
begin
  if Assigned(FOnReceiveReply) then FOnReceiveReply(Self, FRequest, FReply);
end;

procedure TZMQReqThread.Execute;
var
  ARequest: string;
begin
  while not Terminated do begin
    RTLeventWaitFor(FRTLEvent);

    // make a local copy to synchronize shared variables
    ARequest := FRequest;
    if ARequest = 'Terminate' then Exit;
    SendString(FRequester, ARequest);

    // wait for response
    FReply := RecvShortString(FRequester);

    {$IFDEF ZMQ_USE_QUEUES}
    // queued in the main thread (TApplication)
    Queue(@ReceiveReplyEvent); // returns immediately
    {$ELSE}
    Synchronize(@ReceiveReplyEvent);
    {$ENDIF}
  end;
end;

procedure TZMQReqThread.SendRequest(ARequest: string; Blocking: Boolean);
begin
  FRequest := ARequest;
  if Blocking then begin
    SendString(FRequester, ARequest);
    FReply := RecvShortString(FRequester);
    ReceiveReplyEvent;
  end else begin
    RTLeventSetEvent(FRTLEvent);
  end;
end;



end.
