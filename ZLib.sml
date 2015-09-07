structure ZLib = struct
    datatype compressionMethod = MDeflate of int | MReserved
    exception UnknownCompressMethod
    fun word8ToCompressMethod w8 = case Word8.andb(w8, 0wxff) of
                                       0w8 => MDeflate(Word.toInt(Word.>>(Word8.toLargeWord w8, 0w4)))
                                     | 0w15 => MReserved
                                     | _ => raise UnknownCompressMethod
    fun fcheck cmf f = (Word8.toLargeWord cmf) * 0w8 + (Word8.toLargeWord f) mod 0w31 =0w0
    fun fdict w8 = Word8.andb(w8, 0w20) <> 0w0
    fun flevel w8 = Word8.>>(w8, 0w6)
    datatype flags = FCHECK | FDICT | FLEVEL
    exception FCHECK
    fun parseZLib data = let
        val cmf = Vector.sub(data, 0)
        val flags = Vector.sub(data, 1)
        val _ = fcheck cmf flags orelse raise FCHECK
        val dictp = fdict flags
        val level = flevel flags
        val (dict, start) = if dictp
                            then (SOME(BigEndianVec.read32(data, 2)), 6)
                            else (NONE, 2)
        val len = Vector.length(data)
        val adler32 = BigEndianVec.read32(data, len - 4)
    in
        (dict, start)
    end
end

