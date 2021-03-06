structure BitReader =
struct
    type word8 = Word8.word
    type t = {data: word8 vector, i: int ref, bi: word8 ref}

    fun getBit word8 bi = 
        Word8.andb(Word8.>>(word8, Word8.toLargeWord bi), 0wx1) 
    fun getNBit word8 bi bn =
      let val word8 = Word8.<<(word8, bi)
      in Word8.>>(word8, 0wx8 - bn) end

    fun seek ({i = (iRef as ref i), bi = (biRef as ref bi), ...}: t) = let
        val ()   = biRef := (bi + 0wx1) mod 0wx8
        val ()   = iRef := (if (!biRef) = 0wx0
                           then i + 1
                           else i)
    in
        ()
    end
      

    fun bitReader vector (i:int) (bi:word8) = {data = vector, i = ref i, bi = ref bi}
    fun readBit (t:t as {data = data, i = ref i, bi = ref bi}) = let
        val byte = Vector.sub(data, i)
        val bit  = getBit byte bi
        val ()   = seek t
    in
        bit
    end
    fun readNBits t bn = let
        val bn = Word.fromInt bn
        (* :TODO: optimize *)
        fun loop n w = if n = bn
                       then w
                       else loop (n + 0w1) (Word8.<<((readBit t), n) + w)
    in
        loop 0w0 0w0
    end

    fun readNBitsHuffman t bn = let
        fun loop 0 w = w
          | loop i w = loop (i - 1) (Word8.<<(w, 0w1) + (readBit t))
    in
        loop bn 0w0
    end


    fun nextBoundary (t:t as {i = iRef as ref i, bi = biRef as ref bi, ...}) = 
      if bi = 0w0
      then ()
      else iRef := i + 1 before biRef := 0w0
    fun readByte t = readNBits t 8
    fun readNBytes (t:t as {data = data, i = iRef as ref i, bi = biRef as ref bi}) len = let
        (* intentionally ignoring bi *)
        val () = biRef := 0w0
        val ret = VectorSlice.vector (VectorSlice.slice(data, i, SOME(i + len)))
        val ()  = iRef := i + len
    in
        ret
    end
end
