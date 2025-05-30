let sequentialAwait = async () => {
  let result1 = await paused("first")
  nodeJsAssert.equal(result1, "first")
  
  let result2 = await paused("second")
  nodeJsAssert.equal(result2, "second")
}

let f = async () => ()
let f = async (.) => ()
let f = async f => f()
let f = async (a, b) => a + b
let f = async (. a, b) => a + b


let maybeSomeValue = switch await fetchData(url) {    
| data => Some(data)
| exception JsExn(_) => None
}

(await f)(a, b) 
-(await f)
await 1 + await 2

assert(await f())

(await f).json()

user.data = await fetch()

<Navbar promise={await gc()}>{await weirdReactSuspenseApi}</Navbar>

let inBinaryExpression = await x->Js.Promise.resolve + 1
let inBinaryExpression = await x->Js.Promise.resolve + await y->Js.Promise.resolve

let withCallback = async (. ()) => {
    async (. x) => await (x->Js.promise.resolve) + 1
}

let () = await (await fetch(url))->(await resolve)

let _ = await (1 + 1)
let _ = await 1 + 1
let _ = await (-1)
let _ = await - 1
let _ = await !ref 
let _ = await f
let _ = await %extension 
let _ = await "test"
let _ = await ((a, b) => a + b)
let _ = await (async (a, b) => a + b)
let _ = await (switch x { | A => () | B => ()})
let _ = await [1, 2, 3]
let _ = await (1, 2, 3)
let _ = await {name: "Steve", age: 32}
let _ = await (user.name = "Steve")
let _ = await (if 20 { true } else {false})
let _ = await (condition() ? true : false)
let _ = await f(x)
let _ = await f(. x)
let _ = await (f(x) : Js.Promise.t<unit>)
let _ = await (while true { infiniteLoop() })
let _ = await (try ok() catch { | _  =>  logError() })
let _ = await (for i in 0 to 10 { sideEffect()})
let _ = await (assert(x))
let _ = await promises[0]
let _ = await promises["resolved"]
let _ = await (promises["resolved"] = sideEffect())
let _ = @outer await @inner expr
let _ = await module(Foo)
let _ = await module(Foo: Bar)
let _ = await Promise
let _ = await Promise(status)

// braces are being dropped, because the ast only has space to record braces surrounding the await
let _ = await {x}
// here braces stay, because of precedence
let _ = await {
    let x = 1
    Js.Promise.resolve(x)
}

let f1 = async (~x, ~y) => x + y
let f2 = async (@foo ~x, @bar ~y) => x + y
let f3 = @foo async (@bar ~x as @zz z, ~y) => x + y
let f4 = async x => x
let f5 = async x => async y => 3
let f6 = async (~x1, ~x2) => async y => 3
let f7 = async x => async (~y) => 3
let f8 = async (~x1, ~x2) => async (~y) => 3
let f9 = x => async (~y) => 3
let f10 = x => async y => 3
let f11 = (. ~x) => (. ~y) => 3

let f12 = @a (@b x) => 3
let f13 = @a @b (~x) => 3

let aw = (await (server->start))->foo
let aw = (@foo (server->start))->foo
let aw = (await (3**4))->foo

let foo = async(~a=34)
let bar = async(~a)=>a+1

let a1 = await 3 + await 4
let a2 = await 3 ** await 4
let a3 = await foo->bar(~arg)
let a4 = await foo.bar.baz

let b1  = await (3+4)
let b2 = await (3**4)
let b3 = await (foo->bar(~arg))
let b4 = await (foo.bar.baz)

let c1 = @foo x => @bar y => x + y
let c2 = (. x) => y => x+y
let c3 = (. x) => @foo y => x+y

type t1 = (. int) => string => bool
type t2 = (. int, string) => bool

let f = async (type a, ()) => {
  await Js.Promise.resolve(())
}

let attr1 = @a async x => x+1
let attr2 =  @a async (type a) => (type b c, x) => 3
let attr3 =  @a  (type a) => async (type b c, x) => 3
let attr4 =  @a  (type a) => @b async (type b c, x) => 3
let attr5 : int => promise<int> =  @a @b async (type a, type b c) => (x:a) => x
