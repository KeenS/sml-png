structure Deflate = struct
    structure BR = BitReader
(*
 Lit Value    Bits        Codes
---------    ----        -----
  0 - 143     8          00110000 through
                         10111111
144 - 255     9          110010000 through
                         111111111
256 - 279     7          0000000 through
                         0010111
280 - 287     8          11000000 through
                         11000111
 *) 
    val fixedHuffmanTree = let
        fun genTable s e bits codeOffset = List.tabulate((e+1 - s), (fn i => (codeOffset + (Word.fromInt i), bits, i + s)))
        val table1 = genTable   0 143 8 0wx30
        val table2 = genTable 144 255 9 0wx190
        val table3 = genTable 256 279 7 0wx0
        val table4 = genTable 280 287 8 0wxc0
    in
        Huffman.import (List.concat [table1, table2, table3, table4])
    end

    fun decodeVal br =
      let val b = Word8.toLargeWord(BR.readNBits br 7)
      in case Huffman.decode fixedHuffmanTree b 7 of
             SOME(v) =>  v
           | NONE => let val b = Word.<<(b, 0wx1) + Word8.toLargeWord(BR.readBit br)
                     in case Huffman.decode fixedHuffmanTree b 8 of
                            SOME(v) => v
                          | NONE => let val b = Word.<<(b, 0wx1) + Word8.toLargeWord(BR.readBit br)
                                    in case Huffman.decode fixedHuffmanTree b 9 of
                                           SOME(v) => v
                                         | NONE => raise Fail "Invalid Huffman encode"
                                    end
                     end
      end

    fun blockNo buf br = let
        val ()  = BR.nextBoundary br
        val len = Word8.toInt(BR.readByte br)
        val nlen = Word8.toInt(BR.readByte br)
        val true = len + nlen = 0xff
    in
        Buffer.extendVec buf (BR.readNBytes br len)
    end

    fun blockFixed buf br = let
        fun getLen v br =
          if 257 <= v andalso v <= 264
          then 1 * (v - 257) + 3
          else if 265 <= v andalso v <= 268
          then 2 * (v - 265) + 11 + (Word8.toInt(BR.readBit br))
          else if 269 <= v andalso v <= 272
          then 4 * (v - 269) + 19 + (Word8.toInt(BR.readNBits br 2))
          else if 273 <= v andalso v <= 276
          then 8 * (v - 273) + 35 + (Word8.toInt(BR.readNBits br 3))
          else if 277 <= v andalso v <= 280
          then 16 * (v - 277) + 67 + (Word8.toInt(BR.readNBits br 4))
          else if 281 <= v andalso v <= 284
          then 32 * (v - 281) + 115 + (Word8.toInt(BR.readNBits br 5))
          else 258
        fun getDist v br =
          if 0 = v andalso v = 1
          then v + 1
          else let
              val v_2 = v div 2
              val n = v_2 - 1
          in
              Word.toInt(Word.<<(Word.fromInt(v - v_2), Word.fromInt n)) + 3 + Word.toInt(Word.<<(0w1, Word.fromInt (n + 1))) + (Word8.toInt(BR.readNBits br n))
          end

        exception Exit
        fun pushVal v = if 0 <= v andalso v <= 255
                          then Buffer.push buf (Word8.fromInt v)
                          else if v = 256
                          then raise Exit
                          else let
                              val len = getLen v br
                              val dist = getDist (decodeVal br)  br
                              val i = Buffer.getPoint buf
                              val subseq = Buffer.subseq buf {si = i - dist, ei = i - dist + len}
                          in
                              Buffer.extend buf subseq
                          end
        fun loop () = let
            val v = decodeVal br
            val () = pushVal v
        in
            loop ()
        end
    in
        loop ()
        handle Exit => ()
    end
    fun blockDynamic buf br = ()
    fun blockReserved buf br = raise Fail "reserved block type can't be used."

    fun deflate data = let
        val buf = Buffer.make(0, 0w0: word8)
        val br  = BR.bitReader data 0 0w0
        fun loop () = let
            val bfinal = BR.readBit br
            val btype = BR.readNBits br 2
            val () = case btype of
                        0w0 => blockNo buf br
                      | 0w1 => blockFixed buf br
                      | 0w2 => blockDynamic buf br
                      | 0w3 => blockReserved buf br
                      | _   =>  raise Fail "logic flow"
        in
            if bfinal = 0wx1 then ()
            else loop ()
        end
        in
            loop ()
            handle Subscript => ();
            Buffer.array buf
        end
end
