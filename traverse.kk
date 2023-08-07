
effect yield
  ctl yield(i : int) : bool

fun traverse(xs : list<int>) : yield ()
  match xs
    Cons(x, xx) -> if yield(x) then traverse(xx) else ()
    Nil -> ()

fun with-ctl(action : () -> yield ()) : console ()
  with ctl yield(i)
    println("yielded " ++ i.show)
    if i<=2 then
      resume(True)
    else
      println("")
  action()

fun with-fun(action : () -> yield()) : console ()
  with fun yield(i)
    println("yielded " ++ i.show)
    if (i<=2).not then println("")
    i<=2
  action()

fun main()
  println("with-ctl")
  with-ctl
    traverse([1,2,3,4])

  println("with-fun")
  with-fun
    traverse([1,2,3,4])
