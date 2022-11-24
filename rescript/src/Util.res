module Array = {
  let tap = (arr: array<'a>, fn: 'a => unit): array<'a> => {
    arr->Belt.Array.map(it => {
      fn(it)
      it
    })
  }
}

module Input = {
  @module("fs")
  external readFile: (string, @as(json`{ "encoding": "utf8" }`) _) => string = "readFileSync"
}

module Option = {
  let traverse = (items: array<option<'a>>): option<array<'a>> => {
    items->Belt.Array.reduce(Some([]), (acc, it) => {
      acc->Belt.Option.flatMap(prev => it->Belt.Option.map(val => prev->Belt.Array.concat([val])))
    })
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

  let getMatchs = (str, re) => {
    re
    ->Js.Re.exec_(str)
    ->Belt.Option.map(Js.Re.captures)
    ->Belt.Option.map(Belt.Array.map(_, Js.Nullable.toOption))
    ->Belt.Option.flatMap(Option.traverse)
  }
}

module Result = {
  let toOption = (result: result<'a, 'b>) => {
    switch result {
    | Ok(val) => Some(val)
    | Error(_) => None
    }
  }
  let fromOption = (option: option<'a>, fromNone: result<'a, 'b>): result<'a, 'b> => {
    switch option {
    | Some(val) => Ok(val)
    | None => fromNone
    }
  }
  let traverse = (results: array<result<'a, 'b>>): result<array<'a>, 'b> => {
    results->Belt.Array.reduce(Ok([]), (r, it) =>
      r->Belt.Result.flatMap(prev => it->Belt.Result.map(val => prev->Belt.Array.concat([val])))
    )
  }
  let swap = (result: result<'a, 'b>): result<'b, 'a> => {
    switch result {
    | Ok(val) => Error(val)
    | Error(val) => Ok(val)
    }
  }

  let print = result => {
    switch result {
    | Ok(val) => Js.log2("Ok:", val)
    | Error(val) => Js.log2("Err:", val)
    }
  }
}

module Range = {
  let inRange = (val, min, max) => min <= val && val <= max
}
