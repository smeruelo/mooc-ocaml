(*
let a = 0.320641569373584
and b = 2.77192342543023962
and c = 2.08203332204767833
and s = 0.596105228351817118

ccr a b c s = 0.812726147535
*)

let ccr =
  fun a -> let p_a = 8. *. cos (a /. 2.) in
    fun b -> let p_b = cos (b /. 2.) *. p_a in
      fun c -> let p_c = cos (c /. 2.) *. p_b in
        fun s -> s /. p_c
