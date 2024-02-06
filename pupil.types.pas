unit Pupil.Types;

{$mode ObjFPC}{$H+}

interface

uses Math;

type

    { TNotifyReply }

    TNotifyReply = procedure(Sender: TObject; ARequest, AResponse: String) of object;

    TNormalizedGaze = record
      X : Float;
      Y : Float;
    end;

    TNormalizedGazes = array of TNormalizedGaze;

    TGazeOnSurface = record
      Name : string;
      Gazes : TNormalizedGazes;
      WordFrameTimestamp : Float;
    end;

    TGazeOnSurfaceEvent = procedure (Sender : TObject; AGazeOnSurface: TGazeOnSurface) of object;

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

const
  SUB_SURFACES_EVENT = 'surfaces.';
  SURFACES_SCREEN = 'surfaces.screen';
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

end.

