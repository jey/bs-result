@@ocaml.doc(
  "
 * # Result
 *
 * Monadic interface functions for the `Belt.Result.t` object.
 *
 * Here a basic example:
 * ```reason
 * let r =
 *   Result.return(10)
 *   -> Result.map(x => x + 10)
 *   -> Result.flatMap(x => Result.return(x + 2))
 *   -> Result.getOrElse(-1);
 * Js.log2(\"Result: \", r);
 * ```
 * ## Functor
 *
 * @doc map, map2, map3
 *
 * ## Monad
 *
 * @doc flatMap, flatMap2, flatMap3
 *
 * ## Applicative
 *
 * @doc return, error, ap
 *
 * ## Bifunctor
 *
 * @doc bimap
 *
 * ## Belt.Option.t('a)
 *
 * @doc toOption, fromOption
 *
 * ## Logical Operations
 *
 * @doc isOk, isError, forAll, forEach
 *
 * ## Extraction
 * This is also known as the Comonad specification, where the method names
 * are `extend` and `extract` for `unsafeGet` and `getOrElseThunk`
 * respectively.
 *
 * @doc fold, getOrElse, getOrElseThunk, unsafeGet
 *
 * ## Other
 *
 * @doc swap
 *
 * ## Promise
 *
 * @doc
 "
)

type t<'a, 'b> = Belt.Result.t<'a, 'b>

module Promise = Result_promise

@ocaml.doc(
  "
 * This operation is the `pure` Applicative operation for lifting a value
 * into the `Result` context.
 *
 * ```re
 * let r = Result.return(42);
 * ```
 "
)
let return: 'a => Belt.Result.t<'a, 'b>

@ocaml.doc(
  "
 * This operation is the constructor to lift a value into the `Result`
 * context as an `Error`.
 *
 * ```re
 * let e = Result.error(\"bad things\");
 * ```
 "
)
let error: 'b => Belt.Result.t<'a, 'b>

let isOk: Belt.Result.t<'a, 'b> => bool

let isError: Belt.Result.t<'a, 'b> => bool

@ocaml.doc(
  "
 * Applicative apply operation. This implements the Applicative specification
 *
 * 1. Identity
 * ```re
 * let f = Result.return(x => x);
 * let a = Result.return(2);
 * let b = Result.ap(a, f);
 * Js.log2(\"Should be true\", a == b);
 * ```
 * 2. Homomorphism
 * ```re
 * let f = x => x + 1;
 * let x = 2;
 * let a = Result.return(f) -> Result.ap(Result.return(x));
 * let b = Result.return(f(x));
 * Js.log2(\"Should be true\", a == b);
 * ```
 * 3. Interchange
 * ```reason
 * let u = x => x + 1;
 * let y = 1;
 * let a = Result.ap(Result.return(u), Result.return(y));
 * let b = Result.ap(Result.return(f => f(y)), Result.return(u));
 * Js.log2(\"Should be true\", a == b);
 * ```
 "
)
let ap: (Belt.Result.t<'a => 'c, 'b>, Belt.Result.t<'a, 'b>) => Belt.Result.t<'c, 'b>

@ocaml.doc(
  "
 * `map` provides a Functor compatible interface for `Belt.Result.t`.
 * There are 2 laws which a Functor must meet.
 *
 * 1. Identity
 * ```reason
 * Js.log2(\"Should be 42\", Result.map(x => x, Result.return(42)));
 * ```
 * 2. Composition
 * ```reason
 * let f = x => x + 1;
 * let g = x => x + 2;
 * let a = Result.return(0) -> Result.map(x => f(g(x)));
 * let b = Result.return(0) -> Result.map(g) -> Result.map(f);
 * Js.log2(\"Should be true\", a == b);
 * ```
 "
)
let map: (Belt.Result.t<'a, 'c>, 'a => 'b) => Belt.Result.t<'b, 'c>

let mapError: (Belt.Result.t<'a, 'b>, 'b => 'c) => Belt.Result.t<'a, 'c>

@ocaml.doc(
  "
 * `map2` provides the Functor compatible map over 2 results and passes the
 * contents of those results to a function with an arity of 2.
 "
)
let map2: (Belt.Result.t<'a, 'd>, Belt.Result.t<'b, 'd>, ('a, 'b) => 'c) => Belt.Result.t<'c, 'd>

@ocaml.doc(
  "
 * `map3` provides the Functor compatible map over 3 results and passes the
 * contents of those results to a function with an arity of 3.
 "
)
let map3: (
  Belt.Result.t<'a, 'e>,
  Belt.Result.t<'b, 'e>,
  Belt.Result.t<'c, 'e>,
  ('a, 'b, 'c) => 'd,
) => Belt.Result.t<'d, 'e>

let fold: (Belt.Result.t<'a, 'b>, 'a => 'c, 'b => 'c) => 'c

@ocaml.doc(
  "
 * `bimap` provides a Bifunctor compatible interface for `Belt.Result.t`.
 * There are 2 laws which a Bifunctor must meet.
 *
 * 1. Identity
 * ```reason
 * let a = Result.return(42);
 * let b = Result.error(\"boom!\");
 * let a1 = a -> Result.bimap(a => a, b => b);
 * let b1 = b -> Result.bimap(a => a, b => b);
 * Js.log3(\"Both should be true\", a == a1, b == b1);
 * ```
 * 2. Composition
 * ```reason
 * let f = x => x + 1;
 * let g = x => x + 2;
 * let h = x => x + 3;
 * let i = x => x + 4;
 * let a = 42;
 * let b = 24;
 * let ok1 = Result.return(a) -> Result.bimap(a => f(g(a)), b => h(i(b)));
 * let ok2 = Result.return(a) -> Result.bimap(f, h) -> Result.bimap(g, i);
 * let err1 = Result.error(b) -> Result.bimap(a => f(g(a)), b => h(i(b)));
 * let err2 = Result.error(b) -> Result.bimap(f, h) -> Result.bimap(g, i);
 * Js.log3(\"Both should be true\", ok1 == ok2, err1 == err2);
 * ```
 "
)
let bimap: (Belt.Result.t<'a, 'b>, 'a => 'c, 'b => 'd) => Belt.Result.t<'c, 'd>

@ocaml.doc(
  "
 * Returns a `Belt.Option.Some('a)` if the source is an `Ok('a)`, or
 * a `Belt.Option.None` if the source is an `Error('b)`.
 "
)
let toOption: Belt.Result.t<'a, 'b> => option<'a>

@ocaml.doc(
  "
 * Given a `Belt.Option.t('a)` and a function `unit => 'b` return an `Ok('a)`
 * if the given `Belt.Option.t('a`) is a `Belt.Option.Some('a)`, or
 * an `Error('b)` if the input is a `Belt.Option.None`.
 "
)
let fromOption: (option<'a>, unit => 'b) => Belt.Result.t<'a, 'b>

let swap: Belt.Result.t<'a, 'b> => Belt.Result.t<'b, 'a>

@ocaml.doc(
  "
 * `flatMap` (chain) provides a Monad compatible interface for `Belt.Result.t`.
 * There are 2 laws which a Monad must meet.
 *
 * 1. Left identity
 * ```reason
 * let f = x => Result.return(x + 1);
 * let a = 42;
 * let x = Result.return(a) -> Result.flatMap(f);
 * Js.log2(\"Should be true\", x == f(a));
 * ```
 * 2. Right identity
 * ```reason
 * let a = Result.return(42);
 * let x = a -> Result.flatMap(Result.return);
 * Js.log2(\"Should be true\", a == x);
 * ```
 "
)
let flatMap: (Belt.Result.t<'a, 'c>, 'a => Belt.Result.t<'b, 'c>) => Belt.Result.t<'b, 'c>

@ocaml.doc(
  "
 * `flatMap2` provides the Monad compatible flatMap over 2 results and passes
 * the contents of those results to a function with an arity of 2.
 "
)
let flatMap2: (
  Belt.Result.t<'a, 'd>,
  Belt.Result.t<'b, 'd>,
  ('a, 'b) => Belt.Result.t<'c, 'd>,
) => Belt.Result.t<'c, 'd>

@ocaml.doc(
  "
 * `flatMap3` provides the Monad compatible flatMap over 3 results and passes
 * the contents of those results to a function with an arity of 3.
 "
)
let flatMap3: (
  Belt.Result.t<'a, 'e>,
  Belt.Result.t<'b, 'e>,
  Belt.Result.t<'c, 'e>,
  ('a, 'b, 'c) => Belt.Result.t<'d, 'e>,
) => Belt.Result.t<'d, 'e>

let forAll: (Belt.Result.t<'a, 'b>, 'a => bool) => bool

let forEach: (Belt.Result.t<'a, 'b>, 'a => unit) => unit

let getOrElse: (Belt.Result.t<'a, 'b>, 'a) => 'a

let getOrElseThunk: (Belt.Result.t<'a, 'b>, 'b => 'a) => 'a

let unsafeGet: Belt.Result.t<'a, exn> => 'a
