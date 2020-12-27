open Jest

exception UnsafeGetFailure(string)

describe("Basic Result Utilities", () => {
  test("return", () => {
    let actual = Result.return(1)
    let expected = Belt.Result.Ok(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("return", () => {
    let actual = Result.return(1)
    let expected = Belt.Result.Ok(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("error", () => {
    let actual = Result.error("boom")
    let expected = Belt.Result.Error("boom")
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("isOk", () => {
    let actual = (Result.isOk(Result.return(1)), Result.isOk(Result.error("boom")))
    let expected = (true, false)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("isError", () => {
    let actual = (Result.isError(Result.return(1)), Result.isError(Result.error("boom")))
    let expected = (false, true)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("ap - Ok", () => {
    let value = Result.return(40)
    let actual = Result.return(x => x + 2)->Result.ap(value)
    let expected = Result.return(42)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("ap - Error", () => {
    let value = Result.return(40)
    let actual = Result.error("boom")->Result.ap(value)
    let expected = Result.error("boom")
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("ap - Value Error", () => {
    let value = Result.error("boom!")
    let actual = Result.return(x => x + 2)->Result.ap(value)
    let expected = Result.error("boom!")
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("ap - Identity", () => {
    let value = Result.return(40)
    let actual = Result.return(x => x)->Result.ap(value)
    let expected = Result.return(40)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("ap - Homomorphism", () => {
    let fn = Result.return(x => x + 1)
    let value = Result.return(40)
    let a = Result.ap(fn, value)
    let b = Result.return(41)
    Expect.expect(b) |> Expect.toEqual(a)
  })
  test("ap - Interchange", () => {
    let u = x => x + 1
    let y = 1
    let a = Result.ap(Result.return(u), Result.return(y))
    let b = Result.ap(Result.return(f => f(y)), Result.return(u))
    Expect.expect(b) |> Expect.toEqual(a)
  })
  test("map - Ok", () => {
    let actual = Result.return(1)->Result.map(x => x + 1)
    let expected = Result.return(2)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("map - Error", () => {
    let actual = Result.error(1)->Result.map(x => x + 1)
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("map2 - Ok", () => {
    let actual = Result.map2(Result.return(1), Result.return(2), (x, y) => x + y)
    let expected = Result.return(3)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("map2 - Error", () => {
    let actual = Result.map2(Result.error(1), Result.error(2), (x, y) => x + y)
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("map3 - Ok", () => {
    let actual = Result.map3(Result.return(1), Result.return(2), Result.return(3), (x, y, z) =>
      x + y + z
    )
    let expected = Result.return(6)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("map3 - Error", () => {
    let actual = Result.map3(Result.error(1), Result.error(2), Result.error(3), (x, y, z) =>
      x + y + z
    )
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("fold - Ok", () => {
    let actual = Result.return(1)->Result.fold(x => x + 2, x => x + 1)
    let expected = 3
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("fold - Error", () => {
    let actual = Result.error(1)->Result.fold(x => x + 2, x => x + 1)
    let expected = 2
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("bimap - Ok", () => {
    let actual = Result.return(1)->Result.bimap(x => x + 2, x => x + 1)
    let expected = Result.return(3)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("bimap - Error", () => {
    let actual = Result.error(1)->Result.bimap(x => x + 2, x => x + 1)
    let expected = Result.error(2)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("bimap - Identity - Ok", () => {
    let f = a => a
    let g = b => b
    let x = Result.return(42)
    let y = x->Result.bimap(f, g)
    Expect.expect(x) |> Expect.toEqual(y)
  })
  test("bimap - Identity - Error", () => {
    let f = a => a
    let g = b => b
    let x = Result.error("boom!")
    let y = x->Result.bimap(f, g)
    Expect.expect(x) |> Expect.toEqual(y)
  })
  test("bimap - Composition - Ok", () => {
    let f = x => x + 1
    let g = x => x + 2
    let h = x => x + 3
    let i = x => x + 4
    let a = 42
    let ok1 = Result.bimap(Result.return(a), a => f(g(a)), b => h(i(b)))
    let ok2 = Result.return(a)->Result.bimap(f, h)->Result.bimap(g, i)
    Expect.expect(ok1) |> Expect.toEqual(ok2)
  })
  test("bimap - Composition - Error", () => {
    let f = x => x + 1
    let g = x => x + 2
    let h = x => x + 3
    let i = x => x + 4
    let b = 24
    let err1 = Result.error(b)->Result.bimap(a => f(g(a)), b => h(i(b)))
    let err2 = Result.error(b)->Result.bimap(f, h)->Result.bimap(g, i)
    Expect.expect(err1) |> Expect.toEqual(err2)
  })
  test("toOption - Ok", () => {
    let actual = Result.toOption(Result.return(1))
    let expected = Some(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("toOption - Error", () => {
    let actual = Result.toOption(Result.error(1))
    let expected = None
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("fromOption - Ok", () => {
    let actual = Some(42)->Result.fromOption(() => ())
    let expected = Result.return(42)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("fromOption - Error", () => {
    let actual = None->Result.fromOption(() => ())
    let expected = Result.error()
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("swap - Ok", () => {
    let actual = Result.swap(Result.return(1))
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("swap - Error", () => {
    let actual = Result.swap(Result.error(1))
    let expected = Result.return(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap - Ok", () => {
    let actual = Result.return(1)->Result.flatMap(x => Result.return(x + 1))
    let expected = Result.return(2)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap - Error", () => {
    let actual = Result.error(1)->Result.flatMap(x => Result.return(x + 1))
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap - Left Identity - Ok", () => {
    let f = x => Result.return(x + 1)
    let a = 42
    let x = Result.return(a)->Result.flatMap(f)
    Expect.expect(f(a)) |> Expect.toEqual(x)
  })
  test("flatMap - Right Identity - Ok", () => {
    let a = Result.return(42)
    let x = a->Result.flatMap(Result.return)
    Expect.expect(a) |> Expect.toEqual(x)
  })
  test("flatMap2 - Ok", () => {
    let actual = Result.flatMap2(Result.return(2), Result.return(40), (x, y) =>
      Result.return(x + y)
    )
    let expected = Result.return(42)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap - Ok", () => {
    let actual = Result.return(1)->Result.flatMap(x => Result.return(x + 1))
    let expected = Result.return(2)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap - Error", () => {
    let actual = Result.error(1)->Result.flatMap(x => Result.return(x + 1))
    let expected = Result.error(1)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap3 - Ok", () => {
    let actual = Result.flatMap3(
      Result.return(2),
      Result.return(18),
      Result.return(22),
      (x, y, z) => Result.return(x + y + z),
    )
    let expected = Result.return(42)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("flatMap3 - Error", () => {
    let actual = Result.flatMap3(Result.return(2), Result.error(18), Result.return(22), (x, y, z) =>
      Result.return(x + y + z)
    )
    let expected = Result.error(18)
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("forAll - Ok (true)", () => {
    let actual = Result.return(1)->Result.forAll(x => x == 1)
    let expected = true
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("forAll - Ok (false)", () => {
    let actual = Result.return(1)->Result.forAll(x => x == 2)
    let expected = false
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("forAll - Error (pass)", () => {
    let actual = Result.error(2)->Result.forAll(x => x == 2)
    let expected = true
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("forAll - Error (fail)", () => {
    let actual = Result.error(1)->Result.forAll(x => x == 2)
    let expected = true
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("forEach - Ok", () => {
    let actual = ref(1)
    Result.return(1)->Result.forEach(x => actual := x + actual.contents)
    let expected = 2
    Expect.expect(actual.contents) |> Expect.toEqual(expected)
  })
  test("forEach - Error", () => {
    let actual = ref(1)
    Result.error(1)->Result.forEach(x => actual := x + actual.contents)
    let expected = 1
    Expect.expect(actual.contents) |> Expect.toEqual(expected)
  })
  test("getOrElse - Ok", () => {
    let actual = Result.return("bar")->Result.getOrElse("foo")
    let expected = "bar"
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("getOrElse - Error", () => {
    let actual = Result.error("baz")->Result.getOrElse("foo")
    let expected = "foo"
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("getOrElseThunk - Ok", () => {
    let actual = Result.return("bar")->Result.getOrElseThunk(_ => "foo")
    let expected = "bar"
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("getOrElseThunk - Error", () => {
    let actual = Result.error("baz")->Result.getOrElseThunk(_ => "foo")
    let expected = "foo"
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("unsafeGet - Ok", () => {
    let actual = try Result.return(42)->Result.unsafeGet catch {
    | UnsafeGetFailure(_) => 0
    }
    let expected = 42
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
  test("unsafeGet - Error", () => {
    let actual = try Result.error(UnsafeGetFailure("bad"))->Result.unsafeGet catch {
    | UnsafeGetFailure(_) => 17
    }
    let expected = 17
    Expect.expect(actual) |> Expect.toEqual(expected)
  })
})

describe("Result.Promise based utilities", () => {
  testPromise("return", () =>
    Result.Promise.return(42) |> Js.Promise.then_(actual =>
      Expect.expect(actual) |> Expect.toEqual(Result.return(42)) |> Js.Promise.resolve
    )
  )
  testPromise("return", () =>
    Result.Promise.return(42) |> Js.Promise.then_(actual =>
      Expect.expect(actual) |> Expect.toEqual(Result.return(42)) |> Js.Promise.resolve
    )
  )
  testPromise("error", () =>
    Result.Promise.error("boom") |> Js.Promise.then_(actual =>
      Expect.expect(actual) |> Expect.toEqual(Result.error("boom")) |> Js.Promise.resolve
    )
  )
  testPromise("isOk - true", () =>
    Result.Promise.return(1) |> Result.Promise.isOk |> Js.Promise.then_(actual => {
      let expected = true
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  )
  testPromise("isOk - false", () =>
    Result.Promise.error("error") |> Result.Promise.isOk |> Js.Promise.then_(actual => {
      let expected = false
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  )
  testPromise("isError - true", () =>
    Result.Promise.error("error") |> Result.Promise.isError |> Js.Promise.then_(actual => {
      let expected = true
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  )
  testPromise("isError - false", () =>
    Result.Promise.return(1) |> Result.Promise.isError |> Js.Promise.then_(actual => {
      let expected = false
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  )
  testPromise("ap - Ok", () => {
    let value = Js.Promise.resolve(Result.return(40))
    let fn = Js.Promise.resolve(Result.return(x => x + 2))

    Result.Promise.ap(fn, value) |> Js.Promise.then_(actual => {
      let expected = Result.return(42)
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  })
  testPromise("ap - Error", () => {
    let value = Js.Promise.resolve(Result.return(40))
    let error = Js.Promise.resolve(Result.error("error"))
    Result.Promise.ap(error, value) |> Js.Promise.then_(actual => {
      let expected = Result.error("error")
      Expect.expect(actual) |> Expect.toEqual(expected) |> Js.Promise.resolve
    })
  })
  testPromise("map - Ok", () =>
    Result.Promise.return(42)->Result.Promise.map(x => x + 1)
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.return(43)) |> Js.Promise.resolve
      )
  )
  testPromise("map - Error", () =>
    Result.Promise.error("boom")->Result.Promise.map(x => x + 1)
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.error("boom")) |> Js.Promise.resolve
      )
  )
  testPromise("fold - Ok", () =>
    Result.Promise.return(42)->Result.Promise.fold(
      x => string_of_int(x) |> Js.Promise.resolve,
      x => Js.String.toUpperCase(x) |> Js.Promise.resolve,
    )
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual("42") |> Js.Promise.resolve
      )
  )
  testPromise("fold - Error", () =>
    Result.Promise.error("boom")->Result.Promise.fold(
      x => string_of_int(x) |> Js.Promise.resolve,
      x => Js.String.toUpperCase(x) |> Js.Promise.resolve,
    )
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual("BOOM") |> Js.Promise.resolve
      )
  )
  testPromise("bimap - Ok", () =>
    Result.Promise.return(42)->Result.Promise.bimap(x => x + 1, x => Js.String.toUpperCase(x))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.return(43)) |> Js.Promise.resolve
      )
  )
  testPromise("bimap - Error", () =>
    Result.Promise.error("boom")->Result.Promise.bimap(x => x + 1, x => Js.String.toUpperCase(x))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.error("BOOM")) |> Js.Promise.resolve
      )
  )
  testPromise("andThen - Ok", () =>
    Result.Promise.return(42)->Result.Promise.andThen(x => Result.Promise.return(x + 1))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.return(43)) |> Js.Promise.resolve
      )
  )
  testPromise("andThen - Error", () =>
    Result.Promise.error(42)->Result.Promise.andThen(x => Result.Promise.error(x + 1))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.error(42)) |> Js.Promise.resolve
      )
  )
  testPromise("flatMap - Ok", () =>
    Result.Promise.return(42)->Result.Promise.flatMap(x => Result.return(x + 1))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.return(43)) |> Js.Promise.resolve
      )
  )
  testPromise("flatMap - Error", () =>
    Result.Promise.error("boom")->Result.Promise.flatMap(x => Result.return(x + 1))
      |> Js.Promise.then_(actual =>
        Expect.expect(actual) |> Expect.toEqual(Result.error("boom")) |> Js.Promise.resolve
      )
  )
  testPromise("unsafeResolve - Ok", () =>
    Result.Promise.return(42)
    |> Js.Promise.then_(result => Result.Promise.return(result) |> Result.Promise.unsafeResolve)
    |> Js.Promise.then_(actual =>
      Expect.expect(actual) |> Expect.toEqual(Result.return(42)) |> Js.Promise.resolve
    )
  )
  testPromise("unsafeResolve - Error", () =>
    Result.Promise.error(UnsafeGetFailure("boom"))->Result.Promise.unsafeResolve
    |> Js.Promise.then_(_ => fail("should not be possible") |> Js.Promise.resolve)
    |> Js.Promise.catch(error =>
      Js.Json.stringifyAny(error)
      |> Expect.expect
      |> Expect.toEqual(Some("{\"RE_EXN_ID\":\"ResultTest.UnsafeGetFailure/4\",\"_1\":\"boom\"}"))
      |> Js.Promise.resolve
    )
  )
  testPromise("unsafeMapResolve - Ok", () => {
    let actual = 42
    let expected = 43
    Result.Promise.return(actual)->Result.Promise.unsafeMapResolve(x => x + 1)
      |> Js.Promise.then_(result =>
        Expect.expect(result) |> Expect.toEqual(expected) |> Js.Promise.resolve
      )
  })
  testPromise("unsafeFlatMapResolve - Ok", () => {
    let actual = 42
    let expected = 43
    Result.Promise.return(actual)->Result.Promise.unsafeFlatMapResolve(x => Result.return(x + 1))
      |> Js.Promise.then_(result =>
        Expect.expect(result) |> Expect.toEqual(expected) |> Js.Promise.resolve
      )
  })
  testPromise("resolveAll - Ok", () => {
    let input = [1, 2, 3, 4]
    let expected = Ok([2, 4, 6, 8])
    input
    ->Js.Array2.map(x => Result.Promise.return(x * 2))
    ->Js.Promise.all
    ->Result.Promise.resolveAll
      |> Js.Promise.then_(result =>
        Expect.expect(result) |> Expect.toEqual(expected) |> Js.Promise.resolve
      )
  })
  testPromise("resolveAll - Error", () => {
    let input = [1, 2, 3, 4]
    let expected = Error([10, 30])
    input->Js.Array2.map(x =>
      if mod(x, 2) == 0 {
        Result.Promise.return(x * 2)
      } else {
        Result.Promise.error(x * 10)
      }
    )->Js.Promise.all->Result.Promise.resolveAll
      |> Js.Promise.then_(result =>
        Expect.expect(result) |> Expect.toEqual(expected) |> Js.Promise.resolve
      )
  })
})
