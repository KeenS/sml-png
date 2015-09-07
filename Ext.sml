structure Ext =
struct
    structure Array = struct
        open Array
        fun subseq array {si = si, ei = ei} = 
          if ei <= si orelse si < 0 orelse Array.length array < ei
          then raise Subscript
          else let
              val elm = Array.sub(array, si)
              val newArray = Array.array(ei - si, elm)
              fun loop i = (
                  Array.update(newArray, i, Array.sub(array, si + i));
                  loop (i + 1)
              )
          in
              loop 0
              handle Subscript => 0;
              newArray
          end
    end
    structure Vector = struct
        open Vector
        fun subseqArray vector {si = si, ei = ei} = 
          if ei <= si orelse si < 0 orelse Vector.length vector < ei
          then raise Subscript
          else let
              val elm = Vector.sub(vector, si)
              val newArray = Array.array(ei - si, elm)
              fun loop i = (
                  Array.update(newArray, i, Vector.sub(vector, si + i));
                  loop (i + 1)
              )
          in
              loop 0
              handle Subscript => 0;
              newArray
          end
        fun subseq vector index = Array.vector (subseqArray vector index)
    end
end
