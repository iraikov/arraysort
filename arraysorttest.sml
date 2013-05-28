

structure ArraySort = ArrayMergeSortFn (Real64Array)

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

val a1 = realRandomArray (13,17) 100000

val _ = ArraySort.sort Real.compare a1

val _ = if not (ArraySort.sorted Real.compare a1) 
        then raise Fail "merge sort failed" else ()


