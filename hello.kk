import std/os/readline;
import std/os/file;
import std/os/path;
import std/num/decimal;

effect yield
  ctl yield( i : int ) : bool

effect ring
  ctl sm( i : int, j : int ) : int
  ctl ml( i : int, j : int ) : int



fun traverse( xs : list<int> ) : yield ()
  match xs
    Cons(x, xx) -> if yield(x) then traverse(xx) else ()
    Nil -> ()

fun print-elems() : console ()
  with ctl yield(i)
    println("yielded" ++ i.show)
    resume(i<=2)
  traverse([1,2,3,4])

fun calc() : ring int
  val a = sm(1, 3)
  ml(a, a)

fun run-ring( c : () -> ring int ) : int
  with handler
    ctl sm(i, j) { resume(i + j) }
    ctl ml(i, j) { resume(i * j) }
  c()

effect input
  ctl read-word(): string
  ctl read-int(): int
  ctl read-int1(): int

effect state<a>
  fun get() : a
  fun set(x : a) : ()

fun state(init : a, action : () -> <state<a>, div|e> b) : <div|e> (b, a)
  var st := init
  with handler
    return (x) (x, st)
    fun get() st
    fun set(i) st := i
  action()


struct queue<a, h>
  vec : ref<h, vector<a>>
  head : int
  last: int

fun empty-queue(def: a, len : int) : <local<h>, alloc<h>|c> local-var<h, queue<a, h>>
  local-new(Queue(vector(len, def).ref, 0, 0))

fun is-empty(q : queue<a, h>) : bool
  q.head == q.last
fun enqueue(q : local-var<h, queue<a, h>>, a : a) : <local<h>, st<h>, div, exn|e> ()
  write(q.vec, q.last, a)
  q := q( last = q.last + 1 )
fun dequeue(q : local-var<h, queue<a, h>>) : <local<h>, st<h>, div|e> maybe<a>
  if q.is-empty then Nothing else
    q := q( head = q.head + 1)
    (!(q.vec)).at(q.head - 1)

fun write(vec : ref<h, vector<a>>, ix : int, a : a) : <st<h>, div, exn|e> ()
  vec.modify fn (v)
    v[ix] := a

fun show(m : maybe<a>, s : (a : a) -> string) : string
  match m
    Just(i) -> "Just " ++ s(i)
    Nothing -> "Nothing"

effect search
  push(x : int) : ()
  finish(x : int) : int

fun run-bfs(srch : (i : int) -> <search|e> a) : e a
  val queue : forall<h> local-var<h, queue<int, h>> = empty-queue(0, 10)
  with handler search
    ctl push(x)
      q.enqueue(x)
  





fun main()
  with ctl throw-exn(e)
    e.show.println

  val q : forall<h> local-var<h, queue<int, h>> = empty-queue(0, 10)
  q.enqueue(1)
  q.enqueue(2)
  q.enqueue(3)
  q.dequeue.show(show).println
  q.dequeue.show(show).println
  q.dequeue.show(show).println
  q.dequeue.show(show).println

