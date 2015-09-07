structure Huffman = struct
    datatype 'a t = Leaf of 'a | Node of 'a t * 'a t
    fun getBit word i = let
        val mask = Word.<<(0wx1, Word.fromInt i)
    in
        Word.andb(word, mask) = mask
    end

    fun decode tree word len = let
        fun loop (Leaf v) 0 = SOME v
          | loop (Leaf v) _ = NONE
          | loop (Node(f, t)) len =
            case getBit word len of
                true  => loop t (len - 1)
              | false => loop f (len - 1)
    in
        loop tree len
    end
    fun import list = let
        fun loop [(_, _, v)] _ = Leaf v
          | loop [] _  = raise Fail "invalid import table"
          | loop l i = let 
              (* assert 1 <= len - i  *)
              val (t, f) = List.partition (fn (word, len, _) => getBit word (len - i - 1)) l
          in
              Node(loop f (i + 1), loop t (i + 1))
          end
    in
        loop list 0
    end

end
