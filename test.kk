import std/num/float64

type set<a>
  Bin{value : a; size : int; left : set<a>; right : set<a>}
  Tip

fun size(s : set<a>) : int
  match s
    Tip -> 0
    Bin(_, sz, _, _) -> sz

fun intset(l : list<int>) : div set<int>
  val c : (int, int) -> order = compare
  l.foldl(Tip, fn(s, a) s.insert(a, c))

fun show(set : set<a>, s : a -> string) : div string
  match set
    Tip -> "Tip"
    Bin(a, sz, left, right) -> "Bin(" ++ [a.s, sz.show, left.show(s), right.show(s)].join(", ") ++ ")"
    // Tip -> ""
    // Bin(a, left, right) -> [left.show(s) , a.s ++ "\n", right.show(s)].join()

fun empty-set() : set<a>
  Tip
fun insert(s : set<a>, a : a, cmp : (a, a) -> order) : div set<a>
  match s
    Tip -> Bin(a, 1, Tip, Tip)
    Bin(v, sz, l, r) ->
      match cmp(a, v)
        Eq -> s
        Lt -> Bin(v, sz+1, l.insert(a, cmp), r)
        Gt -> Bin(v, sz+1, l, r.insert(a, cmp))



struct queue<h, a>
  vec : ref<h, vector<a>>
  head : int
  last : int
fun empty-queue(def : a) : <alloc<h>> queue<h, a>
  Queue(vector(8, def).ref, 0, 0)
fun enqueue(q : ref<h, queue<h, a>>, a : a) : <st<h>, div, exn|e> ()
  val Queue(vref, h, l) = !(q)
  vref.modify fn (v)
    v[l] := a
  set(q, Queue(vref, h, l + 1))
fun dequeue(q : ref<h, queue<h, a>>) : <st<h>, div|e> maybe<a>
  val Queue(vref, h, l) = !(q)
  if h == l then
    return Nothing
  val ret = (!(vref)).at(h)
  set(q, Queue(vref, h + 1, l))
  ret


type growable-vector<h, a>
  GV{capacity : int; size : int; vec : ref<h, vector<a>>}
fun gv(v : vector<a>) : <alloc<h>|e> growable-vector<h, a>
  GV(v.length, v.length, v.ref)
fun empty-gv(def : a) : <alloc<h>|e> growable-vector<h, a>
  GV(8, 0, vector(8, def).ref)
fun size(gv : ref<h, growable-vector<h, a>>) : <read<h>, div|e> int
  (!(gv)).size
fun foreach(gv : ref<h, growable-vector<h, a>>, f : a -> <read<h>, div, exn|e> ()) : <read<h>, div, exn|e> ()
  match !(gv)
    GV(_, siz, vec) ->
      val v = !(vec)
      list(0, siz - 1).foreach fn (i)
        f(v[i])
fun push(gv : ref<h, growable-vector<h, a>>, a : a) : <st<h>, div, exn|e> ()
  match !(gv)
    GV(cap, siz, vec) ->
      val cap' = if siz < cap then cap else
        val v = !(vec)
        val def = v[0]
        val v' = vector-init(cap * 2) fn (i)
          maybe(v.at(i), def, fn (x) x)
        vec.set(v')
        gv.set(GV(cap * 2, siz, vec))
        cap * 2

      vec.modify fn (v)
        v[siz] := a
      gv.set(GV(cap', siz + 1, vec))
fun at(gv : ref<h, growable-vector<h, a>>, i : int) : <read<h>, div> maybe<a>
  match !(gv)
    GV(_, siz, vec) ->
      if siz <= i then Nothing else
        (!(vec)).at(i)

fun insert(gv : ref<h, growable-vector<h, a>>, i : int, a : a) : <st<h>, div, exn|e> bool
  match !(gv)
    GV(_, siz, vec) ->
      if i < 0 || siz < i then
        return False
      if i == siz then
        gv.push(a)
      else
        val last = (!(vec))[siz - 1]
        gv.push(last)
        vec.modify fn (v)
          list(i, siz - 2).reverse.foreach fn (j)
            v[j + 1] := v[j]
          v[i] := a
      True

fun lower-bound(lower : int, upper : int, sat : int -> <div|e> bool) : <div|e> maybe<int>
  val ok = lower-bound-inner(lower - 1, upper + 1, sat)
  if ok == lower - 1 then Nothing else Just(ok)
fun lower-bound-inner(ok : int, ng : int, sat : int -> <div|e> bool) : <div|e> int
  if (ok - ng).abs <= 1 then ok else
    val mid = (ok + ng) / 2
    if sat(mid) then lower-bound-inner(mid, ng, sat) else lower-bound-inner(ok, mid, sat)
fun upper-bound(lower : int, upper : int, sat : int -> <div|e> bool) : <div|e> maybe<int>
  val ok = upper-bound-inner(lower - 1, upper + 1, sat)
  if ok == upper + 1 then Nothing else Just(ok)
fun upper-bound-inner(ng : int, ok : int, sat : int -> <div|e> bool) : <div|e> int
  if (ng - ok).abs <= 1 then ok else
    val mid = (ng + ok) / 2
    if sat(mid) then upper-bound-inner(ng, mid, sat) else upper-bound-inner(mid, ok, sat)

fun lower-bound(gv : ref<h, growable-vector<h, a>>, sat : a -> bool) : <read<h>, div, exn|e> maybe<int>
  match !(gv)
    GV(_, siz, vec) ->
      val v = !(vec)
      lower-bound(0, siz - 1) fn (i) sat(v[i])
fun upper-bound(gv : ref<h, growable-vector<h, a>>, sat : a -> bool) : <read<h>, div, exn|e> maybe<int>
  match !(gv)
    GV(_, siz, vec) ->
      val v = !(vec)
      upper-bound(0, siz - 1) fn (i) sat(v[i])



fun bisect-left(gv : ref<h, growable-vector<h, a>>, a : a, lte : (a, a) -> bool) : <read<h>, div, exn|e> int
  maybe(gv.upper-bound(fn (b) a.lte(b)), gv.size, fn (i) i)

effect cont<r>
  ctl quit(a : r) : ()
fun cont(action : () -> <cont<r>|e> r) : e r
  with ctl quit(r)
    r
  action()


fun show(gv : ref<h, growable-vector<h, a>>, s : list<a> -> string) : <read<h>, div|e> string
  match !(gv)
    GV(_, siz, vec) ->
      val v = !(vec)
      vlist(v).take(siz).s

struct rset<h, a>
  tab : vector<ref<h, growable-vector<h, a>>>
  size : int
fun rset-empty(bsize : int, def : a) : <alloc<h>|e> rset<h, a>
  val v = vector(bsize, ()).map fn (_) empty-gv(def).ref
  Rset(v, 0)
fun rset-singleton(x : a) : <alloc<h>, st<h>, div, exn|e> rset<h, a>
  val v = empty-gv(x).ref
  v.push(x)
  Rset(vector([v]), 1)
fun rset-empty() : <alloc<h>|e> rset<h, a>
  Rset(vector([]), 0)
fun size(rs : ref<h, rset<h, a>>) : <read<h>, div> int
  (!(rs)).size

fun ceildiv(a : int, b : int) : exn int
  (a + b - 1) / b

fun show(s : ref<h, rset<h, a>>, sh : list<a> -> string) : <read<h>, div|e> string
  val ss = (!(s)).tab.map fn (v) v.show(sh)
  ss.vlist.join("\n\n")

val rset-bucket-ratio = 50
fun rset-build(v : vector<a>, def : a) : <st<h>, div, exn|e> rset<h, a>
  // val bucket-size = int((v.length / rset-bucket-ratio).float64.sqrt) + 1
  val bucket-size = 3
  val per-bucket = v.length.ceildiv(bucket-size)
  val rs = rset-empty(bucket-size, def)
  val tab = rs.tab
  v.foreach-indexed fn (i, a)
    tab[i / per-bucket].push(a)
    ()
  rs(size = v.length)
fun rset-build-gen(g : () -> <generator<a>, st<h>, div, exn|e> (), siz : int, def : a) : <alloc<h>, st<h>, div, exn|e> rset<h, a>
  // val bucket-size = int((siz / rset-bucket-ratio).float64.sqrt) + 1
  val bucket-size = 3
  val per-bucket = siz.ceildiv(bucket-size)
  val rs = rset-empty(bucket-size, def)
  val tab = rs.tab
  gen-indexed(g) fn (i, a)
    tab[i / per-bucket].push(a)
  rs(size = siz)

fun rset-find-bucket(s : rset<h, a>, x : a, lte : (a, a) -> bool) : <read<h>, div, exn|e> ref<h, growable-vector<h, a>>
  cont
    s.tab.foreach-indexed fn (i, v)
      match v.at(v.size - 1)
        Nothing -> ()
        Just(lst) ->
          if x.lte(lst) then quit(v)
    s.tab[s.tab.length - 1]
fun add(st : ref<h, rset<h, a>>, x : a, lte : (a, a) -> bool) : <alloc<h>, st<h>, div, exn|e> bool
  if st.size == 0 then
    st.set(rset-singleton(x))
    return True
  val v = rset-find-bucket(!(st), x, lte)
  val i = v.bisect-left(x, lte)
  if maybe(v.at(i), False, fn (x') x'.lte(x) && x.lte(x'))
    return False
  v.insert(i, x)
  st.modify fn (st')
    st' := st'(size = st'.size + 1)
  st.set(rset-build-gen(fn () rset-all(st), st.size, 0))
  return True
fun rset-all(st : ref<h, rset<h, a>>) : <read<h>, generator<a>, div, exn|e> ()
  (!(st)).tab.foreach fn (v)
    v.foreach fn (a)
      yield(a)



// fun cmp-pair-i(p1 : (int, int), p2 : (int, int)) : order
//   match p1.fst.compare(p2.fst)
//     Eq -> p1.snd.compare(p2.snd)
//     other -> other


fun debug(label : string, v : string)
  trace([label, ": ", v].join())

fun find(lower : int, upper : int, sat : int -> bool) : int
  cont
    list(lower, upper).foreach fn (i)
      if sat(i) then quit(i)
    -1

effect generator<a>
  ctl yield(a : a) : ()

fun generator(gen : () -> <generator<a>|e> (), cb : a -> e ()) : e ()
  with ctl yield(a)
    cb(a)
    resume(())
  gen()

fun gen-indexed(gen : () -> <generator<a>|e> (), cb : (int, a) -> e ()) : e ()
  var i := 0
  with handler
    ctl yield(a)
      val j = i
      cb(i, a)
      i := i + 1
      resume(())
  gen()

fun gen(l : list<a>) : generator<a> ()
  l.foreach fn (a)
    yield(a)

fun main()
  val rs : ref<h, rset<h, int>> = rset-build(vector(list(1, 10)).map(fn (i) i * 2 - 1), 0).ref
  rs.show(show).println
  println("")

  generator(fn () gen(list(0, 10).map(fn (i) i * 2))) fn (n)
    rs.add(n, (<=))
    ()

  // list(0, 10).map(fn (i) i * 2).foreach fn (n)
  //   rs.add(n, (<=))
  //   ()

  rs.show(show).println
