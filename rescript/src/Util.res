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
}
