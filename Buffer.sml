structure Buffer =
struct
    type 'a t = {data: 'a array ref, pointer: int ref, init: 'a}
    fun buffer data (p: int) init =  {data = ref data, pointer = ref p, init = init }
    fun resize (dataRef as ref data) init = let
        val len = Array.length data
        val newData = Array.array(len * 2, init)
        val () = Array.copy {di = 0, src = data, dst = newData}
    in
        dataRef := newData
    end
    fun make (size, elm) = buffer (Array.array(size, elm)) 0 elm

    fun push (buf as {data=(dataRef as ref data),pointer=(pRef as ref p),init=init}) elm = 
      if Array.length data = p
      then let
          val () = resize dataRef init
          val () = Array.update(!dataRef, p, elm)
      in
          pRef := p + 1
      end
      else let
          val () = Array.update(data, p, elm)
      in
          pRef := p + 1
      end

    fun extend buf array = let
        fun loop i = (push buf (Array.sub(array, i)); loop (i + 1))
    in
        loop 0
        handle Subscript => 0;
        ()
    end

    fun extendVec buf vector =let
        fun loop i = (push buf (Vector.sub(vector, i)); loop (i + 1))
    in
        loop 0
        handle Subscript => 0;
        ()
    end

    fun array (buf as {data = ref data, pointer = ref p,...}: 'a t) =
      Ext.Array.subseq data {si = 0, ei = p}
end
