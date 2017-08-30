# pupil-fpc

Free pascal requester and subscriber clients for [pupil](https://github.com/pupil-labs/pupil).

## On ZMQ

The implementation uses pure ZMQ (no czmq) just to keep it simple. For simple req-rep, sub-pub and multipart messaging we don't need czmq.

## Dependencies

- libzmq as in 2017-08-30 master.
- SimpleMsgPack serializer/deserializer

For details, please see: https://github.com/cpicanco/pupil-fpc-examples

## Examples

Stand alone examples can be found here: https://github.com/cpicanco/pupil-fpc-examples

## Pupil Client for TApplication

The class `TPupilClient` (`pupil.client.pas`) can send any request to pupil, react to pupil replyes. You can also subscribe to pupil notifications and messages define in-app events to them.

TPupilClient was designed to (queue) receive replyes and messages in a thread-safe, event driven, non blocking way. Queues are executed in the TApplication main thread. TApplication is part of all standard [Lazarus](http://lazarus-ide.org/) applications. Standard lazarus applications are cross-platform for desktops and depends on the LCL package and its easy to use (yet complex) `Interfaces` and `Forms` units.

To create a requester client do:

```pascal
uses pupil.client;

{...}

// class instantiation
PupilClient := TPupilClient.Create(127.0.0.1:50020);

// events setup

// subscription messages:
// TNotifyMultipartMessage = procedure(Sender: TObject; AMultiPartMessage : TPupilMessage) of object;
PupilClient.OnCalibrationSuccessful := @YourStartCalibrarionEvent;      
PupilClient.OnCalibrationStopped := @YourStopCalibrarionEvent;
PupilClient.OnRecordingStarted := @YourStartRecordingEvent;
PupilClient.OnMultiPartMessageReceived := @YourMessageReceivedEvent;

// request-reply messages:
// TNotifyRequest = procedure(Sender: TObject; ARequest, AReply: String) of object;
PupilClient.OnReplyReceived := @ReplyReceived;

// fun
PupilClient.Start;
```

The following requests are available as constants (prefixed with `REQ_*`):

```pascal
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
```

You can send requests this way:

```pascal
PupilClient.Request(REQ_SHOULD_STOP_CALIBRATION);        // non blocking

{or}

PupilClient.Request(REQ_SHOULD_START_CALIBRATION, True); // blocking
```

To receive pupil messages you must start the subscriber first:

```pascal
PupilClient.StartSubscriber;        // blocking is recommended

{or}

PupilClient.StartSubscriber(False); // non blocking
```

After starting the subscriber, you must chose to which messages you want to receive. You can do this using filters. You can subscribe to any filter you wish. Many subscription filters are available as constants, they are prefixed with `SUB_*`. For instance, you can subscribe to all notification this way:

```pascal

PupilClient.Subscribe(SUB_ALL_NOTIFICATIONS);

```

Take a look at the `pupil.client.pas` for more sub constants.

## Pupil Tasks for anything

If TPupilClient and TApplication is too much for what you want, take a look at `pupil.tasks.pas` and the examples link. They show how to communicate to pupil in a console, application independent style.
