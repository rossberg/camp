let sqr x = x *. x
let complex x = Complex.{re = x; im = 0.0}
let (+:), (-:), ( *:), cexp, norm = Complex.(add, sub, mul, exp, norm)


(* Adopted from https://rosettacode.org/wiki/Fast_Fourier_transform#C *)

let mipi = complex (-. Float.pi) *: Complex.i

let rec fft ampls =
  let freqs = Array.map complex ampls in
  fft' freqs (Array.copy freqs) 0 1;
  freqs

and fft' buf out off k =
  let n = Array.length buf in
  if k < n then
  (
    fft' out buf off (2 * k);
    fft' out buf (off + k) (2 * k);
    for j = 0 to n / (2 * k) - 1 do
      let i = j * 2 * k in
      let t = cexp (mipi *: complex (float i /. float n)) *: out.(off + k + i) in
      buf.(off + i/2) <- out.(off + i) +: t;
      buf.(off + (i + n)/2) <- out.(off + i) -: t;
    done
  )


(* Based on https://github.com/ProfJski/RayLib-Examples/blob/master/Audio1.cpp *)

let fft_samples = 2048
let hz_per_sample = 44100.0 /. float fft_samples

let fft_window = Array.init fft_samples  (* Hann window *)
  (fun i -> sqr (Float.sin (Float.pi *. float i /. float fft_samples)))

let bands wave n =
  let bands = Array.make n 0.0 in

  if Array.length wave >= fft_samples then
  (
    let ampls = Array.init fft_samples (fun i -> wave.(i) *. fft_window.(i)) in
    let freqs = fft ampls in

    let upto = fft_samples / 2 in
    let j, k = ref 0, ref 0 in
    for i = 0 to upto - 1 do
      let y = max 0.0 (log (norm freqs.(i))) in
(*
      (* Linear bands *)
      let j' = i * n / upto in
*)
      (* Logarithmic bands, starting 4 octaves below 440 Hz *)
      let bands_per_octave = float n /. 8.0 in
      let j' = int_of_float (bands_per_octave *.
        (4.0 +. log (float i *. hz_per_sample /. 440.0) /. log 2.0)) in
      let j' = max 0 (min (n - 1) j') in

      bands.(j') <- bands.(j') +. y;
      if j' = !j then incr k else
      (
        bands.(!j) <- bands.(!j) /. float !k;
        k := 1;
        j := j';
      )
    done;
    bands.(!j) <- bands.(!j) /. float !k;
  );

  bands
