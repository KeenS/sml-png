signature BYTE_ORDER = sig
    type t
    val read32: t * int-> word
    (* val write32: t * int * word -> unit *)
end
