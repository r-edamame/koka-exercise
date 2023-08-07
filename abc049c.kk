import std/os/readline;

/* ======================================== */
/* ================= Main ================= */
/* ======================================== */

fun main()
  val tokens = read-tokens()
  run-input(tokens.local-new)
    val s = word()
    (if solv(s) then "YES" else "NO").println

fun solv(s' : string) : <div, exn> bool
  val l = s'.count
  val ws = ["dream", "dreamer", "erase", "eraser"].map(list)
  var dp := vector(l + 1, False)
  var s := list(s')
  dp[0] := True

  list(0, l - 1).foreach fn (i)
    match dp.at(i)
      Just(True) ->
        ws.foreach fn (w)
          if s.starts-with(w, (!=)) && i + w.length <= l then
            dp[i + w.length] := True
      _ -> ()
    s := s.tail
  dp[l]

fun starts-with(l : list<a>, pre : list<a>, ne : (a, a) -> bool) : div bool
  match (l, pre)
    (_, Nil) -> True
    (Nil, _) -> False
    (Cons(x,xx), Cons(y,yy)) ->
      if x.ne(y) then False else starts-with(xx, yy, ne)


/* ============================================ */
/* ================= Library ================== */
/* ============================================ */


/* ============== Type Definitions ============ */

/* matrix */
struct mat<a>
  width : int
  height : int
  vecs : vector<vector<a>>
fun show(m : mat<int>) : string
  m.vecs.map(fn (v) v.map(show).vlist.join(" ")).vlist.join("\n")

fun read-tokens() : <console,div> list<string>
  var ls := []
  with ctl throw-exn(_)
    ls
  while(fn () True)
    ls := ls ++ readline().split(" ")
  []

/* mint */
struct mint
  value : int

val mint-mod = 100000

fun (+)(m1 : mint, m2 : mint) : mint
  Mint((m1.value + m2.value) % mint-mod)

/* ============== Input Effect ============ */

effect input
  ctl int() : int
  ctl word() : string

/* input util */
fun int1() : input int
  int() - 1
fun ints-base(n : int, i : () -> input int) : input vector<int>
  vector(1, n).map fn (_) i()
fun ints(n : int) : input vector<int>
  ints-base(n, int)
fun ints1(n : int) : input vector<int>
  ints-base(n, int1)
fun intmat-base(w : int, h : int, i : () -> input int) : input mat<int>
  val vecs = vector(1, h).map fn (_)
    vector(1, w).map fn (_)
      i()
  Mat(w, h, vecs)
fun intmat(w : int, h : int) : input mat<int>
  intmat-base(w, h, int)
fun intmat1(w : int, h : int) : input mat<int>
  intmat-base(w, h, int1)
fun charmat(w : int, h : int) : input mat<char>
  val vecs = vector(1, h).map fn (_) vector(word())
  Mat(w, h, vecs)

/* input handler */
fun run-input(tokens : local-var<h, list<string>>, action : () -> <input,exn|e> a) : <local<h>,exn|e> a
  with handler
    ctl int()
      match tokens
        Nil -> throw("error")
        Cons(x,xx) ->
          match x.parse-int
            Nothing -> throw("error")
            Just(i) ->
              tokens := xx
              resume(i)
    ctl word()
      match tokens
        Nil -> throw("error")
        Cons(x,xx) ->
          tokens := xx
          resume(x)
  action()


/* ============== Algorithms ============ */

