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

structure Deflate = struct
    fun bitPart i = 0w8 - Word.fromLarge(Word.andb(i, 0wxf))
    fun bytePart i = Word.>>(i, 0w4 )
    fun getBit data i = let
        val bit = bitPart i
        val byte = bytePart i
    in
        Word8.>>(Vector.sub(data, byte), bit) = 0w1
    end
    datatype btype = No | Fixed | Dynamic | Reserved
    fun deflate data = let
        val buf = Array.new ()
        fun loop i = let
            val bfinal = getBit data i
            val i = i + 1
            val btype1 = getBit data i
            val i = i + 1
            val btype2 = getbit data i
            val btype = case (btype1, btype2) of
                            (false, false) => No
                          | (false, true) => Fixed
                          | (true, false) =>  Dynamic
                          | (true, treu) => reserved
        in
            if bfinal then ()
            else loop (i+1)
        end
        fun blockNo i = let
            val i = Word.andb(i + 0wxf, ~0wxf)
            val len = Vector.sub(data, i)
            val i = i + 1
            val nlen = Vector.sub(data,i)
        in
            push i (i + len)
        end
        
    in
    end
end
