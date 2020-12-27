type t<'a, 'b> = Js.Promise.t<Belt.Result.t<'a, 'b>>

let return = x => Ok(x) |> Js.Promise.resolve

let error = x => Error(x) |> Js.Promise.resolve

let isOk = promise => promise |> Js.Promise.then_(x =>
    switch x {
    | Ok(_) => true
    | Error(_) => false
    } |> Js.Promise.resolve
  )

let isError = promise => promise |> Js.Promise.then_(x =>
    switch x {
    | Ok(_) => false
    | Error(_) => true
    } |> Js.Promise.resolve
  )

let map = (promise, fn) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => fn(v)->return
    | Error(e) => e->error
    }
  )

let mapError = (promise, fn) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => v->return
    | Error(e) => fn(e)->error
    }
  )

let fold = (promise, ok, error) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => ok(v)
    | Error(e) => error(e)
    }
  )

let bimap = (promise, fnOk, fnError) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => fnOk(v)->return
    | Error(e) => fnError(e)->error
    }
  )

let andThen = (promise, fn) => promise |> Js.Promise.then_(x =>
    switch x {
    | Error(e) => error(e)
    | Ok(v) => fn(v)
    }
  )

let flatMap = (promise, fn) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => fn(v)->Js.Promise.resolve
    | Error(e) => error(e)
    }
  )

let flatMapError = (promise, fn) => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => return(v)
    | Error(e) => fn(e)->Js.Promise.resolve
    }
  )

let ap = (pfResult, pResult) => pfResult |> Js.Promise.then_(fResult =>
    switch fResult {
    | Ok(fn) => pResult->map(fn)
    | Error(e) => error(e)
    }
  )

let unsafeResolve = promise => promise |> Js.Promise.then_(result =>
    switch result {
    | Ok(v) => Js.Promise.resolve(v)
    | Error(e) => raise(e)
    }
  )

let unsafeMapResolve = (promise, fn) => promise->map(fn)->unsafeResolve

let unsafeFlatMapResolve = (promise, fn) => promise->flatMap(fn)->unsafeResolve

let resolveAll = (promise: Js.Promise.t<array<result<'ok, 'err>>>): t<array<'ok>, array<'err>> => {
  promise |> Js.Promise.then_(results => results->Js.Array2.reduce((a, v) =>
      switch (a, v) {
      | (Ok(arr), Ok(x)) => {
          let _ = arr->Js.Array2.push(x)
          Ok(arr)
        }
      | (Ok(_), Error(e)) => Error([e])
      | (Error(arr), Ok(_)) => Error(arr)
      | (Error(arr), Error(e)) => {
          let _ = arr->Js.Array2.push(e)
          Error(arr)
        }
      }
    , Ok([])) |> Js.Promise.resolve)
}
