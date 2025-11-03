type mode = Baseline | Sequential | Progressive | Lossless | Hierarchical
type coding = Huffman | Arithmetic
type differentiality = NonDifferential | Differential

type format =
{
  mode : mode;
  coding : coding;
  differentiality : differentiality;
  width : int;
  height : int;
  precision : int;
}

exception Format

val read_format : string -> format
