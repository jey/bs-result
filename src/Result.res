open Belt.Result

module Promise = Result_promise

let return = x => Ok(x)

let error = x => Error(x)

let isOk = x =>
  switch x {
  | Error(_) => false
  | Ok(_) => true
  }

let isError = x =>
  switch x {
  | Error(_) => true
  | Ok(_) => false
  }

let map = (result, fn) =>
  switch result {
  | Error(bad) => Error(bad)
  | Ok(good) => good->fn->return
  }

let fold = (result, ok, error) =>
  switch result {
  | Error(bad) => error(bad)
  | Ok(good) => ok(good)
  }

let bimap = (result, fnOk, fnError) =>
  switch result {
  | Error(bad) => bad->fnError->error
  | Ok(good) => good->fnOk->return
  }

let toOption = x =>
  switch x {
  | Error(_) => None
  | Ok(good) => Some(good)
  }

let fromOption = (option, errFn) =>
  switch option {
  | None => errFn()->error
  | Some(v) => v->return
  }

let swap = x =>
  switch x {
  | Ok(good) => error(good)
  | Error(bad) => return(bad)
  }

let flatMap = (result, fn) =>
  switch result {
  | Ok(good) => fn(good)
  | Error(bad) => Error(bad)
  }

let flatMap2 = (fst, snd, fn) => fst->flatMap(x => snd->flatMap(y => fn(x, y)))

let flatMap3 = (a, b, c, fn) => a->flatMap(x => b->flatMap(y => c->flatMap(z => fn(x, y, z))))

let map2 = (fst, snd, fn) => fst->flatMap(x => snd->map(y => fn(x, y)))

let map3 = (a, b, c, fn) => a->flatMap(x => b->flatMap(y => c->map(z => fn(x, y, z))))

let ap = (fResult, result) =>
  switch result {
  | Error(bad) => Error(bad)
  | Ok(v) => fResult->map(f => f(v))
  }

let forAll = (result, fn) =>
  switch result {
  | Error(_) => true
  | Ok(good) => fn(good)
  }

let forEach = (result, fn) =>
  switch result {
  | Error(_) => ()
  | Ok(good) => fn(good)
  }

let getOrElse = (result, default) =>
  switch result {
  | Error(_) => default
  | Ok(good) => good
  }

let getOrElseThunk = (result, fn) =>
  switch result {
  | Error(bad) => fn(bad)
  | Ok(good) => good
  }

let unsafeGet = x =>
  switch x {
  | Error(bad) => raise(bad)
  | Ok(good) => good
  }
