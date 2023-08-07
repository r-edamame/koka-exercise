

effect search
  fun push(x : int) : ()
  ctl quit(x : int) : ()



fun lsearch(l : list<int>, p : int -> bool) : quit ()
  l.foreach fn(v)
    if p(v) then quit(v)

fun quit-maybe(a : () -> quit ()) : maybe<int>
  with ctl quit(x)
    Just(x)
  a()
  Nothing

fun quit-log(a : () -> quit ()) : console ()
  with ctl quit(x)
    println("quitted " ++ x.show)
  a()

fun main()
  quit-log
    lsearch([1,2,3,4,5], fn (i) i==10)
  // maybe(r, "Nothing", show).println

