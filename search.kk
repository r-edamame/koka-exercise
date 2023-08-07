import std/os/readline;

effect search
  fun push(i : int) : ()
  ctl quit(i : int) : ()


struct tree
  value : int
  left-child : maybe<tree>
  right-child : maybe<tree>

fun leaf(i : int) : tree
  Tree(i, Nothing, Nothing)
fun f-branch(i : int, lc : tree, rc : tree) : tree
  Tree(i, Just(lc), Just(rc))
fun l-branch(i : int, lc : tree) : tree
  Tree(i, Just(lc), Nothing)
fun r-branch(i : int, rc : tree) : tree
  Tree(i, Nothing, Just(rc))


fun show(t : tree) : div string
  val l = maybe(t.left-child, "", show)
  val r = maybe(t.right-child, "", show)
  l ++ t.value.show ++ "\n" ++ r

fun lsearch(p : int -> bool, i : int) : <search|e> ()
  if p(i) then quit(i) else
    if i<10 then push(i+1)

fun run-search(initial : int, a : int -> search ()) : div maybe<int>
  var next := initial
  var remain := True
  with handler
    fun push(i)
      next := i
      remain := True
    ctl quit(i)
      Just(i)

  while(fn () { remain == True })
    remain := False
    a(next)
  Nothing

effect search2
  fun push2(i : int) : ()
  fun next2() : maybe<int>
  ctl quit2(i : int) : ()


fun search-while-nexts(f : int -> <search2|e> ()) : <search2|e> ()
  match next2()
    Nothing -> ()
    Just(i) ->
      f(i)
      search-while-nexts(f)

struct mat<a>
  width : int
  height : int
  vecs : vector<vector<a>>

fun show(m : mat<int>) : exn string
  m.vecs.map(fn (v) v.map(show).vjoin(" ")).vjoin("\n")

fun at(m : mat<a>, x : int, y : int) : maybe<a>
  maybe(m.vecs.at(y), Nothing) fn (vec)
    vec.at(x)

// fun read-mat-int(w : int, h : int) : <console,exn|e> mat<a>

fun vjoin(v : vector<string>, sep : string) : string
  vlist(v).join(sep)

fun read-tokens() : <console,div> list<string>
  var ls := []
  with ctl throw-exn(_)
    ls
  while(fn () True)
    ls := ls ++ readline().split(" ")
  []

effect input
  ctl int() : int
  ctl word() : string

fun run-input(action : () -> <input|e> a) : <console, div|e> maybe<a>
  var tokens : local-var<h, list<string>> := read-tokens()
  with handler
    ctl int() : local
      match tokens
        Nil -> Nothing
        Cons(x,xx) ->
          match x.parse-int
            Nothing -> Nothing
            Just(i) ->
              tokens := xx
              resume(i)
    ctl word()
      match tokens
        Nil -> Nothing
        Cons(x,xx) ->
          tokens := xx
          resume(x)
  Just(action())


fun main() : <console,div> ()
  ()
