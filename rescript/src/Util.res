module Array = {
  let tap = (arr: array<'a>, fn: 'a => unit): array<'a> => {
    arr->Belt.Array.map(it => {
      fn(it)
      it
    })
  }
}

module Input = {
  let toArray = (input: string): array<string> => {
    input->Js.String2.trim->Js.String2.split("\n")
  }
}

module String = {
  let toArray = (str: string): array<string> => {
    open Js
    str->String2.castToArrayLike->Array2.from
  }

  let divide = (str, anchor): (string, string) => {
    let first = str->Js.String2.slice(~from=0, ~to_=anchor)
    let last = str->Js.String2.sliceToEnd(~from=anchor)
    (first, last)
  }
}

module Result = {
  let toOption = (result: result<'a, 'b>) => {
    switch result {
    | Ok(val) => Some(val)
    | Error(_) => None
    }
  }
  let traverse = (results: array<result<'a, 'b>>): result<array<'a>, 'b> => {
    results->Belt.Array.reduce(Ok([]), (r, it) =>
      r->Belt.Result.flatMap(prev => it->Belt.Result.map(val => prev->Belt.Array.concat([val])))
    )
  }
  let fold = (result: result<'a, 'b>, err: 'b => unit, ok: 'a => unit): unit => {
    switch result {
    | Ok(val) => ok(val)
    | Error(val) => err(val)
    }
  }
}
