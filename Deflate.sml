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

    fun decodeVal br table least most =  let
        fun loop n b = if n = most
                       then raise Fail "Invalid Huffman encode"
                       else let
                           val () = print ("called1 "^ (Int.toString least)^"\n")
                           val b = Word.<<(b, 0wx1) + Word8.toLargeWord(BR.readBit br)
                           val () = print ("called2 "^ (Int.toString n)^"\n")
                            in
                                case Huffman.decode table b n of
                                    SOME(v) => v
                                  | NONE => loop (n+1) b
                            end
        val b = Word8.toLargeWord(BR.readNBitsHuffman br (least - 1))
    in
        loop least b
    end

    fun blockNo buf br = let
        val ()  = BR.nextBoundary br
        val len = Word8.toLargeWord(BR.readByte br)
        val len = len + (Word8.toLargeWord(BR.readByte br) * 0w256)
        val nlen = Word8.toLargeWord(BR.readByte br)
        val nlen = nlen + (Word8.toLargeWord(BR.readByte br) * 0w256)
        val nnlen =  Word.andb(Word.notb(nlen),0wxffff)
        val () = print ((Word.toString len) ^ "\n")
        val () = print ((Word.toString nlen) ^ "\n")
        val () = print ((Word.toString nnlen) ^ "\n")
        val true = len = nnlen
        val len = Word.toInt len
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
                              val dist = getDist (decodeVal br fixedHuffmanTree 7 9) br
                              val i = Buffer.getPoint buf
                              val () = print ("i: " ^ (Int.toString (i)) ^ "\n")
                              val () = print ("start: " ^ (Int.toString (i - dist)) ^ "\n")
                              val () = print ("end: " ^ (Int.toString (i - dist + len)) ^ "\n")
                              val () = print ("length: " ^ (Int.toString len) ^ "\n")                                             
                              val subseq = Buffer.subseq buf {si = i - dist, ei = i - dist + len}
                          in
                              Buffer.extend buf subseq
                          end
        fun loop () = let
            val v = decodeVal br fixedHuffmanTree 7 9
            val () = print ("decoded: " ^ (Int.toString v) ^ "\n")
            val () = pushVal v
        in
            loop ()
        end
    in
        loop ()
        handle Exit => ()
    end

    fun importDynamic pairs = let
        val len = List.length pairs
        val maxBits = List.foldl (fn ((_,len), acc) => if acc < len then len else acc) 0 pairs
        val blCount = Array.array(maxBits+1, 0w0)
        val nextCode = Array.array(maxBits+1, 0w0)
        fun findSmallest i code = let
            val code = Word.<<(code + (Array.sub(blCount, i - 1)), 0w1)
            val () = Array.update(nextCode, i, code)
        in
            if i = maxBits then ()
            else findSmallest (i + 1) code
        end
        val () = List.app (fn (_, len) => Array.update(blCount, len, 0w1+Array.sub(blCount, len))) pairs
        val () = findSmallest 1 0w0
        val table = List.map (fn (v,len) => let val code = Array.sub(nextCode, len)
                                            in (code, len, v) before Array.update(nextCode, len, code+0w1)
                                            end) pairs
        val () = print (Show.showList (Show.show3Tuple (Word.toString, Int.toString, Int.toString)) table)
    in
       Huffman.import table
    end

    fun blockDynamic buf br = let
        val hlit = Word.toInt((Word8.toLargeWord(BR.readNBits br 5)) + 0w257)
        val hdist = BR.readNBits br 5
        val hclen = ((Word8.toInt(BR.readNBits br 4)) + 4)
        val order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
        fun collectLen 0 order acc = List.rev acc
          | collectLen hlen (v::vs) acc = let
              val codeLen = Word8.toInt (BR.readNBits br 3)
          in
              if codeLen > 0
              then collectLen (hlen - 1) vs ((v, codeLen)::acc)
              else collectLen (hlen - 1) vs acc
          end
          | collectLen _ [] _ = raise Fail "logic flaw"
        val tablePairs = collectLen hclen order []
        val tableTable = importDynamic tablePairs
        val () = print ("hlit"^ (Int.toString hlit)^"\n")
        fun loop n acc = if n = hlit then List.rev acc
                         else (print ("called"^ (Int.toString n)^"\n"); loop (n+1) ((n, decodeVal br tableTable 1 7)::acc))
        val pairs = loop 0 []
        val table = importDynamic pairs
    in
        ()
    end

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
                      | _   =>  raise Fail "logic flaw"
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

val _ = Deflate.deflate (Vector.fromList [0wxED, 0wxD9, 0wxB1, 0wxD, 0wx80, 0wx40, 0wx10, 0wxC4, 0wx40, 0wx1F, 0wxA2, 0wxFF, 0wx96, 0wx21, 0wxA0, 0wx82, 0wxF, 0wx2C, 0wx74, 0wx92, 0wx27, 0wxD9, 0wxD4, 0wxF9, 0wxE, 0wxF0, 0wxB0, 0wxC8, 0wxFD, 0wxCD, 0wx96, 0wxE6, 0wxE1, 0wxFA, 0wx3B, 0wxE1, 0wx54, 0wxC1, 0wxB6, 0wx82, 0wx6D, 0wx5, 0wxDB, 0wxA, 0wxB6, 0wx15, 0wx6C, 0wx2B, 0wxD8, 0wx56, 0wxB0, 0wxAD, 0wx60, 0wx5B, 0wxC1, 0wxB6, 0wx82, 0wx6D, 0wx5, 0wxDB, 0wxA, 0wxB6, 0wx15, 0wx6C, 0wx2B, 0wxD8, 0wx56, 0wxB0, 0wxAD, 0wx60, 0wx5B, 0wxC1, 0wxB6, 0wx61, 0wxCF, 0wxC1, 0wx1, 0wxC0, 0wxB, 0wx97, 0wxD4, 0wx3, 0wx4B])
