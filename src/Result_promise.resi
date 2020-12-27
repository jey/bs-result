@@ocaml.doc(
  "
   * # Result.Promise
   *
   * Integration with the [Js.Promise][js-promise] module.
   *
   * [js-promise]: https://bucklescript.github.io/bucklescript/api/Js.Promise.html
   "
)
type t<'a, 'b> = Js.Promise.t<Belt.Result.t<'a, 'b>>
let return: 'a => Js.Promise.t<Belt.Result.t<'a, 'b>>
let error: 'a => Js.Promise.t<Belt.Result.t<'b, 'a>>
let isOk: Js.Promise.t<Belt.Result.t<'a, 'b>> => Js.Promise.t<bool>
let isError: Js.Promise.t<Belt.Result.t<'a, 'b>> => Js.Promise.t<bool>
let ap: (
  Js.Promise.t<Belt.Result.t<'a => 'c, 'b>>,
  Js.Promise.t<Belt.Result.t<'a, 'b>>,
) => Js.Promise.t<Belt.Result.t<'c, 'b>>
let map: (Js.Promise.t<Belt.Result.t<'a, 'c>>, 'a => 'b) => Js.Promise.t<Belt.Result.t<'b, 'c>>
let fold: (
  Js.Promise.t<Belt.Result.t<'a, 'b>>,
  'a => Js.Promise.t<'c>,
  'b => Js.Promise.t<'c>,
) => Js.Promise.t<'c>
let bimap: (
  Js.Promise.t<Belt.Result.t<'a, 'b>>,
  'a => 'c,
  'b => 'd,
) => Js.Promise.t<Belt.Result.t<'c, 'd>>
let andThen: (
  Js.Promise.t<Belt.Result.t<'a, 'c>>,
  'a => Js.Promise.t<Belt.Result.t<'b, 'c>>,
) => Js.Promise.t<Belt.Result.t<'b, 'c>>
let flatMap: (
  Js.Promise.t<Belt.Result.t<'a, 'c>>,
  'a => Belt.Result.t<'b, 'c>,
) => Js.Promise.t<Belt.Result.t<'b, 'c>>
let unsafeResolve: Js.Promise.t<Belt.Result.t<'a, exn>> => Js.Promise.t<'a>
let unsafeMapResolve: (Js.Promise.t<Belt.Result.t<'a, exn>>, 'a => 'b) => Js.Promise.t<'b>
let unsafeFlatMapResolve: (
  Js.Promise.t<Belt.Result.t<'a, exn>>,
  'a => Belt.Result.t<'b, exn>,
) => Js.Promise.t<'b>
let resolveAll: Js.Promise.t<array<Belt.Result.t<'ok, 'err>>> => t<array<'ok>, array<'err>>
