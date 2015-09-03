functor BigEndian(X: sig
                      type t
                      val sub: t * int -> word8
                  end) = struct
    open X
    val op << = Word.<<
    val op >> = Word.>>

    fun read32(data, i) = let
        val t  = Word8.toLargeWord
        val b1 = sub(data, i)
        val b2 = sub(data, i + 1)
        val b3 = sub(data, i + 2)
        val b4 = sub(data, i + 3)
    in
        <<(t b1, 0w24) + <<(t b2, 0w16) + <<(t b3, 0w8) + (t 0w4)
    end
end

functor LittleEndian(X: sig
                      type t
                      val sub: t * int -> word8
                  end) = struct
    open X
    val op << = Word.<<
    val op >> = Word.>>

    fun read32(data, i) = let
        val t  = Word8.toLargeWord
        val b1 = sub(data, i)
        val b2 = sub(data, i + 1)
        val b3 = sub(data, i + 2)
        val b4 = sub(data, i + 3)
    in
        <<(t b4, 0w24) + <<(t b3, 0w16) + <<(t b2, 0w8) + (t b1)
    end
end

