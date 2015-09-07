structure CRC = struct
    val >>   = Word.>>
    val andb = Word.andb
    val xorb = Word.xorb
    fun make_table () = let
        val table = Array.array(256, 0w0)
        fun loop i = let
            fun loop' c j =
              if j = 8 then c
              else if andb(c, 0w1) = 0w1
              then loop' (xorb(0wxedb88320, >>(c, 0w1))) (j + 1)
              else loop' (>>(c, 0w1)) (j + 1)
        in
            if i = 256 then Array.vector table
            else (Array.update(table, i, loop' 0w0 0);
                  loop (i + 1))
        end
    in
        loop 0
    end
    val table = make_table()
    fun calc vec = let
        val len = Vector.length vec
        fun loop crc i = let
            val index = Word.toInt (andb((xorb(crc, Vector.sub(vec, i))), 0wxff))
        in
            if i = len then crc
            else loop (xorb(Vector.sub(table, index), >>(crc, 0w8))) (i + 1)
        end
    in
        loop 0wxffffffff 0
    end
end

