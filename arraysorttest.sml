

structure ArraySort = ArrayMergeSortFn (Real64Array)

fun timing (action) = 
    let
        val timer = Timer.startCPUTimer ()
        val result = action ()
        val times = Timer.checkCPUTimer timer
    in
        (result, Time.+ (#usr times, #sys times))
    end

fun putStrLn (file, str) = 
    (TextIO.output (file, str);
     TextIO.output (file, "\n"))

fun printArray a =
    let 
        val n      = Real64Array.length a
        fun loop 0 = ()
          | loop j = (putStrLn (TextIO.stdOut, Real.toString (Real64Array.sub(a, n-j)));
                      loop (j-1))
    in 
        loop (n-1)
    end

fun realRandomArray (xseed,yseed) n =
    let 
        val seed   = Random.rand (xseed,yseed)
        val a      = Real64Array.array(n, Random.randReal seed)
        fun loop 0 = a
          | loop j = (Real64Array.update(a, n-j, Random.randReal seed);
                      loop (j-1))
    in 
        loop (n - 1)
    end

val a5 = Real64Array.fromList [4.0, 3.0, 5.0]

val _ = ArraySort.sort Real.compare a5
val _ = if not (ArraySort.sorted Real.compare a5) 
        then raise Fail "a5 merge sort failed" else ()

val a4 = Real64Array.fromList [1.0, 2.0, 4.0, 3.0, 5.0]

val _ = ArraySort.sortRange Real.compare (a4,(2,5))
val _ = if not (ArraySort.sorted Real.compare a4) 
        then raise Fail "a4 merge sort failed" else ()

val a3 = Real64Array.fromList [1.0, 4.0, 2.0, 3.0, 0.0]

val _ = ArraySort.sortRange Real.compare (a3,(0,5))
val _ = if not (ArraySort.sorted Real.compare a3) 
        then raise Fail "a3 merge sort failed" else ()

val a2 = realRandomArray (13,17) 1000000

val (_,t) = timing (fn () => ArraySort.sort Real.compare a2)

val _ = putStrLn (TextIO.stdOut, ("a2 merge sort took " ^ (Time.toString t) ^ " s"))

val _ = if not (ArraySort.sorted Real.compare a2) 
        then raise Fail "a2 merge sort failed" else ()

val a1 = Real64Array.fromList [1.0, 4.0, 2.0, 3.0, 0.0]

val _ = ArraySort.sort Real.compare a1

val _ = if not (ArraySort.sorted Real.compare a1) 
        then raise Fail "a1 merge sort failed" else ()


