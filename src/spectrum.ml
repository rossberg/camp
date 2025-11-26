let _sqr x = x *. x
let complex x = Complex.{re = x; im = 0.0}
let (+:), (-:), ( *:), cexp, norm = Complex.(add, sub, mul, exp, norm)
let mipi = complex (-1.0 *. Float.pi) *: Complex.i


(* Adopted from https://rosettacode.org/wiki/Fast_Fourier_transform#C *)

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
let _hz_per_band = 44100.0 /. float fft_samples
let fft_window = Array.init fft_samples (fun i ->  (* Hann window *)
  0.5 *. (1.0 +. Float.cos (2.0 *. Float.pi *. float i) /. float fft_samples))

let bands wave n =
  let bands = Array.make n 0.0 in

  if Array.length wave >= fft_samples then
  (
    let ampls = Array.init fft_samples (fun i -> wave.(i) *. fft_window.(i)) in
    let freqs = fft ampls in

    let upto = fft_samples / 3 in
    let j, k = ref 0, ref 1 in
    for i = 1 to upto - 1 do
      let y = max 0.0 (log (norm freqs.(i) (*/. float fft_samples*))) in
(*
      (* Inverse of f(x) = 440*2^(x-48)/12 to get note frequencies on scale
       * starting 4 octaves down from 440Hz *)
      let j' = min (n - 1) (max 0 (int_of_float (Float.trunc
        (12.0 *. log (float i *. hz_per_band /. 440.0) /. log 2.0 +. 48.0)))) in
*)
(*
      let j' = min (n - 1) (max 0 (int_of_float (log (float i /. float upto *. float n) /. log 2.0))) in
*)
(*
      let j' = min (n - 1) (if float !k > log (float n /. float i) /. log 2.0 then !j + 1 else !j) in
*)
(* *)
      let j' = i * n / upto in
(* *)
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
