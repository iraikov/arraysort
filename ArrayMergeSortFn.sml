
(*
 
 Merge sort implementation for Standard ML monomorphic arrays.

 Copyright 2013 Ivan Raikov and the Okinawa Institute of Science and Technology.

 This program is free software: you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation, either version 3 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 A full copy of the GPL license can be found at
 <http://www.gnu.org/licenses/>.

*)


signature MONO_ARRAY_SORT =
  sig

    structure A : MONO_ARRAY

    val sort : (A.elem * A.elem -> order) -> A.array -> unit

    val sortRange : (A.elem * A.elem -> order) -> (A.array * (int * int)) -> unit

    val sorted : (A.elem * A.elem -> order) -> A.array -> bool

  end; (* MONO_ARRAY_SORT *)


functor ArrayMergeSortFn (A : MONO_ARRAY) : MONO_ARRAY_SORT =
struct

    structure A = A

    (* Blit FROM[I,K) to TO[J,?]. *)

    fun blit (from,i,k,to,j) =
        (
        if not (i < k) then raise Subscript
        else (let
                  fun loop (i,j) =
                      if (i < k)
                      then (let
                                val vi = A.sub (from,i)
                            in
                                (A.update (to,j,vi);
                                 loop (i+1,j+1))
                            end)
                      else ()
              in
                  loop (i,j)
              end))



   (* Given array A and indices p, q, r such that p < q < r, 
      merge subarray A[p..q) and subarray A[q..r) into array B[n..] *)

   fun merge cmp (a,p,q,r,b,n) =
       if not ((p < q) andalso (q < r))
       then raise Subscript
       else (let
                 fun loop (i,j,k) =
                     if ((i < q) andalso (j < r))
                     then (let 
                               val ai = A.sub (a,i)
                               val aj = A.sub (a,j)
                           in
                               case cmp (ai,aj) of
                                   LESS => (A.update (b,k,ai);
                                            loop (i+1,j,k+1))
                                 | _    => (A.update (b,k,aj);
                                            loop (i,j+1,k+1))
                           end)
                     else 
                         (if (i < q)
                          then blit (a,i,q,b,k)
                          else (if (j < r)
                                then blit (a,j,r,b,k)
                                else ()))
             in
                 (loop (p,q,n); b)
             end)


   (* Vector merge sort *)
   fun sort cmp array =
       let 
           val n = A.length array
           val b = A.array (n, A.sub(array,0))
           fun loop m =
               if (m < n)
               then (let 
                         fun inner p =
                             if p < (n-m)
                             then (let
                                       val q = p+m
                                       val r = Int.min (n, p + (2*m))
                                   in
                                       (merge cmp (array,p,q,r,b,p);
                                        blit (b,p,r,array,p);
                                        inner (p + (2*m)))
                                   end)
                             else loop (2*m)
                     in
                         inner 0
                     end)
               else ()
       in
           loop 1
       end

       
    

   (* Vector merge sort in the range [from,to) *)
   fun sortRange cmp (array,(from,to)) =
       let 
           val n = A.length array
           val _ = if not ((from >= 0) andalso (to <= n)) then raise Subscript else ()
           val b = A.array (to-from, A.sub(array,from))

           fun loop m =
               if (m < to)
               then (let 
                         fun inner (p,bp) =
                             if p < (to-m)
                             then (let
                                       val q = p+m
                                       val r = Int.min (to, p + (2*m))
                                   in
                                       (merge cmp (array,p,q,r,b,bp);
                                        blit (b,bp,r-from,array,p);
                                        inner (p + (2*m),bp + (2*m)))
                                   end)
                             else loop (2*m)
                     in
                         inner (from,0)
                     end)
               else ()
       in
           loop 1
       end
    
    
    fun sorted cmp array = 
        let
            val len = A.length array
            fun s (v,i) = 
                let
                    val v' = A.sub(array,i)
                in
                    case cmp(v,v') of
                        GREATER => false
                      | _ => if i+1 = len then true else s(v',i+1)
                end
        in
            if len = 0 orelse len = 1 then true
            else s(A.sub(array,0),1)
        end

end            
