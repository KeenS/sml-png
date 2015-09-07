structure VectorStream =
struct
    type 'a t = {vec: 'a vector, i: int ref}
    fun stream v (i:int) = {vec = v, i = ref 0}
    fun next {vec = vec, i = (iRef as ref i)} = let
        val v = Vector.sub(vec, i)
        val () = iRef := i + 1
    in
        SOME(v)
    end
                                                handle Subscript => NONE
end
