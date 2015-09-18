signature READER_SIG = sig
    type instream
    val input1: instream -> word8 option
    val inputN: instream * int -> word8 vector
end


fun printW8Vec vec = let
    fun loop i = let
        val w8 = Vector.sub(vec, i)
        val w8 = Word8.toString w8
        val w8 = "0wx" ^ w8
    in
        print(w8 ^ ", ")
      ; loop (i + 1)
    end
in
    print "["
  ; loop 0
    handle Subscript => ()
  ; print "]" 
end


functor PNGFun(Reader: READER_SIG) = struct
    val op << = Word.<<
    val op >> = Word.>>

    fun nameToWord name = CharVector.foldl (fn(c, acc) => <<(acc, 0w8) + (Word.fromInt (ord c))) 0w0 name
    (* Bsic Headers *)
    val IHDR = nameToWord "IHDR"
    val PLTE = nameToWord "PLTE"
    val IDAT = nameToWord "IDAT"
    val IEND = nameToWord "IEND"
    (* Aux Headers *)
    val tRNS = nameToWord "tRNS"
    val gAMA = nameToWord "aAMA"
    val cHRM = nameToWord "cHRM"
    val sRGB = nameToWord "sRGB"
    val iCCP = nameToWord "iCCP"
    val tEXt = nameToWord "tEXt"
    val zTXt = nameToWord "zTXt"
    val iTXt = nameToWord "iTXt"
    val bKGD = nameToWord "bKGD"
    val pHYs = nameToWord "pHYs"
    val sBIT = nameToWord "sBIT"
    val sPLT = nameToWord "sPLT"
    val hlST = nameToWord "hlST"
    val tIME = nameToWord "tIME"
    (* additional Chunks *)
    val fRAc = nameToWord "fRAc"
    val glFg = nameToWord "glFg"
    val glFt = nameToWord "glFt"
    val glFx = nameToWord "glFx"
    val oFFs = nameToWord "oFFs"
    val pCAL = nameToWord "pCAL"
    val sCAL = nameToWord "sCAL"
                          
    fun read32 i = let
        val t  = Word8.toLargeWord
        val SOME(b1) = Reader.input1 i
        val SOME(b2) = Reader.input1 i
        val SOME(b3) = Reader.input1 i
        val SOME(b4) = Reader.input1 i
    in
        <<(t b1, 0w24) + <<(t b2, 0w16) + <<(t b3, 0w8) + (t b4)
    end

    fun readChunk i = let
        val w = read32 i
        val length = Word.toInt(w)
        val name   = read32 i
        val data   = Reader.inputN(i, length)
        val crc    = read32 i
                         (* check crc *)
    in
        (name, data)
    end                             

    datatype colorType = GrayScale | RGB | Pallet | GrayAlpha | RGBA
    exception UnknownColorType
    fun word8ToColorType (w8: word8) = case w8 of
                                           0w0 => GrayScale
                                         | 0w2 => RGB
                                         | 0w3 => Pallet
                                         | 0w4 => GrayAlpha
                                         | 0w6 =>  RGBA
                                         | _  =>  raise UnknownColorType

    datatype compressMethod = ZLib
    exception UnknownCompressMethod
    fun word8ToCompressMethod (w8: word8) = case w8 of
                                                0w0 => ZLib
                                              | _ => raise UnknownCompressMethod

    datatype filterMethod = None | Sub | Up | Average | Paeth
    exception UnknownFilterMethod
    fun word8ToFilterMethod (w8:word8) = case w8 of
                                             0w0 => None
                                           | 0w1 => Sub
                                           | 0w2 => Up
                                           | 0w3 => Average
                                           | 0w4 => Paeth
                                           | _ =>  raise UnknownFilterMethod
    fun filt None img = img
      | filt Sub img = raise Fail "Sub filter is not implemented"
      | filt Up img = raise Fail "Up filter is not implemented"
      | filt Average img = raise Fail "Average filter is not implemented"
      | filt Paeth img = raise Fail "Paeth filter is not implemented"

    datatype interraceMethod = NonInterrace | Adam7
    exception UnknownInterraceMethod
    fun word8ToInterraceMethod (w8: word8) = case w8 of
                                                 0w0 => NonInterrace
                                               | 0w1 => Adam7
                                               | _ => raise UnknownInterraceMethod
    fun parseIHDR data = let
        val width           = BigEndianVec.read32(data, 0)
        val height          = BigEndianVec.read32(data, 4)
        val depth           = Vector.sub(data, 8)
        val colorType       = word8ToColorType (Vector.sub(data, 9))
        val compressMethod  = word8ToCompressMethod(Vector.sub(data, 10))
        val filterMethod    = word8ToFilterMethod(Vector.sub(data, 11))
        val interraceMethod = word8ToInterraceMethod(Vector.sub(data, 12))
    in
        {width = width, height = height, depth = depth,
         colorType = colorType, compressMethod = compressMethod,
         filterMethod = filterMethod, interraceMethod = interraceMethod}
    end

    fun parsePLTE data = let
        val r = Vector.sub(data, 0)
        val g = Vector.sub(data, 1)
        val b = Vector.sub(data, 2)
    in
        {r = r, g = g, b = b}
    end

    fun parseIDAT data = let
        val _ = ZLib.parseZLib data
    in
        data
    end

    (* fun alfaSeparate img = img *)
    (* fun indexize img = img *)
    (* fun RGBMerge img = img *)
    (* fun alphaCompress img = img *)
    (* fun sampleDepthScaling img = img *)


    fun vectorMatch a1 a2 = let
        val len1 = Vector.length(a1)
        val len2 = Vector.length(a2)
        fun loop i =  Vector.sub(a1, i) = Vector.sub(a2, i) andalso loop (i + 1)
    in
        loop 0
        handle Subscript => true
    end
                               
    exception NotPNG
    fun parsePNG i = let
        val buf = Reader.inputN(i, 8)

        val marker:word8 vector = Vector.fromList [0wx89, 0wx50, 0wx4E, 0wx47, 0wx0D, 0wx0A, 0wx1A, 0wx0A]
        val true = vectorMatch buf marker orelse raise NotPNG
        fun loop img= let
            val (name, data) = readChunk i
        in
            if      name = IHDR then loop img before (parseIHDR data; ())
            else if name = PLTE then loop img before (parsePLTE data; ())
            else if name = IDAT then loop (SOME(parseIDAT data))
            else if name = IEND then img
                                         (* Aux Headers *)
            else if name = tRNS then loop img before print ("ignoring tRNS\n")
            else if name = gAMA then loop img before print ("ignoring gAMA\n")
            else if name = cHRM then loop img before print ("ignoring cHRM\n")
            else if name = sRGB then loop img before print ("ignoring sRGB\n")
            else if name = iCCP then loop img before print ("ignoring iCCP\n")
            else if name = tEXt then loop img before print ("ignoring tEXt\n")
            else if name = zTXt then loop img before print ("ignoring zTXt\n")
            else if name = iTXt then loop img before print ("ignoring iTXt\n")
            else if name = bKGD then loop img before print ("ignoring bKGD\n")
            else if name = pHYs then loop img before print ("ignoring pHYs\n")
            else if name = sBIT then loop img before print ("ignoring sBIT\n")
            else if name = sPLT then loop img before print ("ignoring sPLT\n")
            else if name = hlST then loop img before print ("ignoring hlST\n")
            else if name = tIME then loop img before print ("ignoring tIME\n")
                                                           (* additional Chunks *)
            else if name = fRAc then loop img before print ("ignoring fRAc\n")
            else if name = glFg then loop img before print ("ignoring glFg\n")
            else if name = glFt then loop img before print ("ignoring glFt\n")
            else if name = glFx then loop img before print ("ignoring glFx\n")
            else if name = oFFs then loop img before print ("ignoring oFFs\n")
            else if name = pCAL then loop img before print ("ignoring pCAL\n")
            else if name = sCAL then loop img before print ("ignoring sCAL\n")
            else loop img before print((Word.toString name) ^ "\n")
        end
    in
        loop NONE
    end
end

structure PNG = PNGFun(BinIO)
(* fun printW8Vec vec = let *)
(*     fun loop i = let *)
(*         val w8 = Vector.sub(vec, i) *)
(*         val w8 = Word8.toString w8 *)
(*     in *)
(*         print(w8 ^ ", "); *)
(*         loop (i + 1) *)
(*     end *)
(* in *)
(*     loop 0 *)
(*     handle Subscript => () *)
(* end *)
