
struct queue<a>
  front : list<a>
  back : list<a>

fun queue-empty() : queue<a>
  Queue([], [])
fun push-back(q : queue<a>, a : a) : queue<a>
  q(back = Cons(a,q.back))
fun pop-front(q : queue<a>) : (maybe<a>, queue<a>)
  match q.front
    Cons (x,xx) -> (Just(x), q(front = xx))
    Nil -> match q.back.reverse
      Nil -> (Nothing, q)
      Cons(x,xx) -> (Just(x), Queue(xx, []))

fun show(m : maybe<a>, s : a -> string) : string
  m.maybe("Nothing") fn(a)
    "Just(" ++ a.s ++ ")"

effect ll<a>
  fun push(a : a) : ()
  fun pop() : maybe<a>

fun with-queue(action : () -> <ll<a>, div|e> b) : <div|e> b
  var q := queue-empty()
  with handler
    fun push(a) q := q.push-back(a)
    fun pop()
      val (ma, q') = q.pop-front
      q := q'
      ma
  action()

fun with-stack(action : () -> <ll<a>, div|e> b) : <div|e> b
  var s := []
  with handler
    fun push(a) s := Cons(a,s)
    fun pop()
      val ma = s.head
      s := s.tail
      ma
  action()

fun main()
  val is : int -> string = show
  // with with-queue
  with with-stack
  push(1)
  pop().show(is).println
  pop().show(is).println
  push(2)
  push(3)
  pop().show(is).println
  pop().show(is).println
  pop().show(is).println
  ()
