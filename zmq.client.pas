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

uses Classes;

type
  { TMultiPartMessage }

  TMultiPartMessage = record
    MsgPackage : TMemoryStream;
    MsgTopic : string;
  end;

  { TMultiPartMessageReceived }

  TMultiPartMessageReceived = procedure(AResponse: TMultiPartMessage) of object;

  { TZMQSubThread }

  TZMQSubThread = class(TThread)
  private
    FMultipartMessage : TMultiPartMessage;
    FContext : Pointer;
    FSubscriber : Pointer;
    FOnMultipartMessageReceived: TMultiPartMessageReceived;
    procedure MultipartMessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(ASubHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Subscribe(AFilter : string);
    procedure Unsubscribe(AFilter : string);
    property OnMultiPartMessageReceived: TMultiPartMessageReceived
      read FOnMultipartMessageReceived write FOnMultipartMessageReceived;
  end;

  { TReceiveReplyEvent }

  TReceiveReplyEvent = procedure(ARequest, AReply: String) of object;

  { TZMQReqThread }

  TZMQReqThread = class(TThread)
  private
    FReply,
    FRequest : string;
    FContext : Pointer;
    FRequester : Pointer;
    FRTLEvent: PRTLEvent;
    FOnReceiveReply: TReceiveReplyEvent;
    procedure ReceiveReplyEvent; inline;
  protected
    procedure Execute; override;
    procedure SendRequest(ARequest : string; Blocking : Boolean = False);
    property OnReceiveReply: TReceiveReplyEvent read FOnReceiveReply write FOnReceiveReply;
  public
    constructor Create(AHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses zmq, zmq.helpers;

{ TZMQSubThread }

procedure TZMQSubThread.MultipartMessageReceived;
begin
  if Assigned(FOnMultipartMessageReceived) then FOnMultipartMessageReceived(FMultipartMessage);
end;

procedure TZMQSubThread.Execute;
var
  zmq_message : zmq_msg_t;
begin
  while not Terminated do
    begin
      zmq_message := Default(zmq_msg_t);
      zmq_msg_init(zmq_message);
      with FMultipartMessage do
      try
        // frame 1 is a string
        zmq_msg_recv(zmq_message, FSubscriber, 0);
        SetString(MsgTopic, PAnsiChar(zmq_msg_data(zmq_message)), zmq_msg_size(zmq_message));

        // frame 2 is MsgPack serialization
        if zmq_msg_more(zmq_message) = 1 then
        begin
          zmq_msg_close(zmq_message);
          zmq_msg_init(zmq_message);
          zmq_msg_recv(zmq_message, FSubscriber, 0);
          MsgPackage := TMemoryStream.Create;
          MsgPackage.WriteBuffer(zmq_msg_data(zmq_message)^, zmq_msg_size(zmq_message));
        end;

        // deserialization is done outside
        {$IFDEF ZMQ_USE_QUEUES}
        Queue(@MultipartMessageReceived);
        {$ELSE}
        Synchronize(@MultipartMessageReceived);
        {$ENDIF}
      finally
        zmq_msg_close(zmq_message);
      end;
    end;
end;

constructor TZMQSubThread.Create(ASubHost: string; CreateSuspended: Boolean);
var
  host : PChar;
begin
  host := PChar('tcp://' + ASubHost);
  FreeOnTerminate := True;

  FContext := zmq_ctx_new;
  FSubscriber := zmq_socket(FContext, ZMQ_SUB);
  zmq_connect(FSubscriber, host);
  inherited Create(CreateSuspended);
end;

destructor TZMQSubThread.Destroy;
begin
  zmq_close(FSubscriber);
  zmq_ctx_shutdown(FContext);
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

destructor TZMQReqThread.Destroy;
begin
  zmq_close(FRequester);
  zmq_ctx_shutdown(FContext);
  RTLEventDestroy(FRTLEvent);
  inherited Destroy;
end;

procedure TZMQReqThread.ReceiveReplyEvent;
begin
  if Assigned(FOnReceiveReply) then FOnReceiveReply(FRequest, FReply);
end;

procedure TZMQReqThread.Execute;
var
  ARequest: string;
begin
  while not Terminated do
    begin
      RTLeventWaitFor(FRTLEvent);

      // make a local copy to synchronize shared variables
      ARequest := FRequest;
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
  if Blocking then
    begin
      SendString(FRequester, ARequest);
      FReply := RecvShortString(FRequester);
      ReceiveReplyEvent;
    end
  else
    RTLeventSetEvent(FRTLEvent);
end;



end.
