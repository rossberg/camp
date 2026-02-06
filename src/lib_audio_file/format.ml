type path = string

type t =
{
  codec : string;
  channels : int;
  depth : int;
  rate : int; (* Hz *)
  bitrate : float; (* b/s *)
  time : float; (* s *)
  size : int; (* B *)
}

let unknown =
{
  codec = "";
  channels = 0;
  depth = 0;
  rate  = 0;
  bitrate = 0.0;
  time  = 0.0;
  size = 0;
}


let exts = [".mp3"; ".flac"; ".wav"; ".oga"; ".ogg"; ".opus"; ".mod"; ".xm"]

let is_known_ext path =
  List.mem (String.lowercase_ascii (Filename.extension path)) exts


let check x = if Float.is_finite x then x else 0.0

let read path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".mp3" ->
    let mp3 = Mp3.read_format path in
    let channels = match mp3.channels with Mono -> 1 | _ -> 2 in
    {
      codec = "MP3";
      channels = channels;
      depth = (try 1000 * mp3.bitrate / mp3.rate / channels with Division_by_zero -> 0);
      rate = mp3.rate;
      bitrate = float (1000 * mp3.bitrate);
      time = mp3.time;
      size = mp3.size;
    }

  | ".flac" ->
    let flac = Flac.read_format path in
    let time = float flac.samples /. float flac.rate in
    {
      codec = "FLAC";
      channels = flac.channels;
      depth = flac.depth;
      rate = flac.rate;
      bitrate = check (float (8 * flac.size) /. time);
      time = check time;
      size = flac.size;
    }

  | ".wav" ->
    let wav = Wav.read_format path in
    {
      codec = "WAV";
      channels = wav.channels;
      depth = wav.depth;
      rate = wav.rate;
      bitrate = float (wav.rate * wav.depth * wav.channels);
      time = float (try wav.size / wav.channels / (wav.depth / 8) / wav.rate with Division_by_zero -> 0);
      size = wav.size;
    }

  | ".ogg" | ".oga" ->
    let ogg = Ogg.read_format path in
    let time = float ogg.samples /. float ogg.rate in
    {
      codec = "OGG";
      channels = ogg.channels;
      depth = (try ogg.bitrate_nominal / ogg.rate / ogg.channels with Division_by_zero -> 0);
      rate = ogg.rate;
      bitrate = float ogg.bitrate_nominal;
      time = check time;
      size = ogg.size;
    }

  | ".opus" ->
    let opus = Opus.read_format path in
    let time = float opus.samples /. float opus.rate in
    let bitrate = float (8 * 48000 * opus.size) /. float opus.samples in
    {
      codec = "OPUS";
      channels = opus.channels;
      depth = (try int_of_float bitrate / opus.rate / opus.channels with Division_by_zero -> 0);
      rate = opus.rate;
      bitrate = check bitrate;
      time = check time;
      size = opus.size;
    }

  | _ -> unknown
