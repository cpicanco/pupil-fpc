{
  pupil-fpc
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0)

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Pupil.Tasks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function PupilSubscriberTask(Args : Pointer) : PtrInt;

const
  forever = False;

var
  PupilRequestHost : PChar = 'tcp://127.0.0.1:50020';
  PupilSubscribeHost : string = 'tcp://127.0.0.1:';

implementation

uses SimpleMsgPack, Pupil.Client, zmq, zmq.helpers;

function PupilSubscriberTask(Args: Pointer): PtrInt;
var
  context, subscriber :  Pointer;
  zmq_message : zmq_msg_t;
  filter : string = SUB_ALL_NOTIFICATIONS;

  Serializer :  TSimpleMsgPack;
  MsgTopic : string;
  MsgPackage : TMemoryStream;
  PupilMessage : TPupilMessage;


  procedure DumpPupilMessage(PupilMessage: TPupilMessage);
  var j : integer;
  begin
    with PupilMessage do
    begin
      WriteLn('------------------------------------');
      WriteLn('Topic:', Topic);
      WriteLn('TopicCount:', IntToStr(Payload.Count));
      with Payload do
        for j := 0 to Count -1 do
          WriteLn(Items[j].Key, ':', Items[j].Value);
    end;
  end;

begin
  Result := PtrInt(0);
  try
    context := zmq_ctx_new;
    subscriber := zmq_socket(context, ZMQ_SUB);
    zmq_connect(subscriber, PChar(PupilSubscribeHost));
    zmq_setsockopt(subscriber, ZMQ_SUBSCRIBE, @filter[1], Length(filter));
    Serializer := TSimpleMsgPack.Create;
    MsgPackage := TMemoryStream.Create;
    repeat
      zmq_message := Default(zmq_msg_t);
      zmq_msg_init(zmq_message);
      zmq_msg_recv(zmq_message, subscriber, 0);
      try
        // frame 1 is a string
        SetString(MsgTopic, PAnsiChar(zmq_msg_data(zmq_message)), zmq_msg_size(zmq_message));
        PupilMessage.Topic := MsgTopic;

        // frame 2 is MsgPack serialization
        MsgPackage.Clear;
        if zmq_msg_more(zmq_message) = 1 then
        begin
          zmq_msg_close(zmq_message);
          zmq_msg_init(zmq_message);
          zmq_msg_recv(zmq_message, subscriber, 0);

          MsgPackage.WriteBuffer(zmq_msg_data(zmq_message)^, zmq_msg_size(zmq_message));
          MsgPackage.Position := 0;

          Serializer.Clear;
          Serializer.DecodeFromStream(MsgPackage);
          PupilMessage.Payload := Serializer;
        end;
        DumpPupilMessage(PupilMessage);
      finally
        zmq_msg_close(zmq_message);
      end;
    until forever;
  finally
    Serializer.Free;
    MsgPackage.Free;
    zmq_close(subscriber);
    zmq_ctx_shutdown(context);
  end;
end;

end.

