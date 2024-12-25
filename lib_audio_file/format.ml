type t =
{
  name : string;
  channels : int;
  depth : int;
  rate : int; (* Hz *)
  bitrate : float; (* b/s *)
  time : float; (* s *)
  size : int; (* B *)
}

let unknown =
{
  name = "";
  channels = 0;
  depth = 0;
  rate  = 0;
  bitrate = 0.0;
  time  = 0.0;
  size = 0;
}

let read path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".mp3" ->
    let mp3 = Mp3.read_format path in
    let channels = match mp3.channels with Mono -> 1 | _ -> 2 in
    {
      name = "MP3";
      channels = channels;
      depth = 1000 * mp3.bitrate / mp3.rate / channels;
      rate = mp3.rate;
      bitrate = float (1000 * mp3.bitrate);
      time = mp3.time;
      size = mp3.size;
    }

  | ".flac" ->
    let flac = Flac.read_format path in
    let time = float flac.samples /. float flac.rate in
    {
      name = "FLAC";
      channels = flac.channels;
      depth = flac.depth;
      rate = flac.rate;
      bitrate = float (8 * flac.size) /. time;
      time = time;
      size = flac.size;
    }

  | ".wav" ->
    let wav = Wav.read_format path in
    {
      name = "WAV";
      channels = wav.channels;
      depth = wav.depth;
      rate = wav.rate;
      bitrate = float (wav.rate * wav.depth * wav.channels);
      time = float (wav.size / wav.channels / (wav.depth / 8) / wav.rate);
      size = wav.size;
    }

  | ".ogg" -> {unknown with name = "OGG"}  (* TODO *)
  | ".opus" -> {unknown with name = "OPUS"}  (* TODO *)

  | _ -> unknown
