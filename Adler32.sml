structure Adler32 = struct
    val modulo = 0w65521
    exception Exit of word
    fun calc data = let
        fun loop i A B = let
            val byte = Vector.sub(data, i)
                       handle Subscript => raise Exit(Word.<<(B, 0w8) + Word.andb(A, 0wxff))
            val A = A + (Word8.toLargeWord byte) mod modulo
            val B = B + A mod modulo
        in
            loop (i + 1) A B
        end
    in
        loop 0 0w1 0w0
        handle Exit w => w
    end
end
