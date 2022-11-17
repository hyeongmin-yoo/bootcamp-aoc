// Types
type grid = array<array<string>>
type pos = (int, int)

// Utils
let toArray = (str: string): array<string> => {
  open Js
  str->String2.castToArrayLike->Array2.from
}

let tap = (arr: array<'a>, fn: 'a => unit): array<'a> => {
  arr->Belt.Array.map(it => {
    fn(it)
    it
  })
}

let count = (arr: array<'a>): int => {
  arr->Belt.Array.reduce(0, (result, _) => result + 1)
}

// TODO: Bigint 어떻게 써야하지?
// 현재 int max 값을 벗어나서 오류남.
// int64 , float
let multipyAll = (arr: array<float>): float => {
  arr->Belt.Array.reduce(1.0, (result, val) => result *. val)
}

let toGrid = (str: string): grid => {
  open Js
  str->String2.trim->String2.split("\n")->Array2.map(toArray)
}

// nx + 1 = row
// [(3,1), (3,1), (3,1)] = length!. (3,1)
// Belt.Array.rangeBy(0, length -1, ~setp=2) -> [1,2,3,4,5,6////]
let makeCmds = (slope: pos, grid): array<pos> => {
  open Belt
  let (_, y) = slope
  let size = ((grid->Array.length - 1)->Int.toFloat /. y->Int.toFloat)->Js.Math.ceil_int
  Array.make(size, slope)
}

// [(3,1), (6,2), (9,3)]
// clojure <- lazy evaluation
// 무한의 값
// [(3,1), ... ] -> take
let mapToPos = (positions: array<pos>): array<pos> => {
  positions->Belt.Array.reduce([], (arr: array<pos>, cmdPos) => {
    let (cmdX, cmdY) = cmdPos
    let prev = arr->Belt.Array.get(arr->Belt.Array.length - 1)
    let pos = switch prev {
    | Some((x, y)) => (cmdX + x, cmdY + y)
    | None => cmdPos
    }
    arr->Belt.Array.concat([pos])
  })
}

let findChar = (pos, grid): option<string> => {
  open Belt

  grid
  ->Array.get(0)
  ->Option.map(Array.length)
  ->Option.flatMap(width => {
    let (_x, y) = pos
    let x = mod(_x, width)
    grid->Array.get(y)->Option.flatMap(Array.get(_, x))
  })
}

let isTree = (char: string) => {
  char == "#"
}

let countTrees = (slope: pos, grid) => {
  open Belt

  // data
  slope
  // [], ...
  ->makeCmds(grid)
  // ...
  ->mapToPos
  ->Array.keepMap(findChar(_, grid))
  //->Array.keepMap(a => a) // 없애기 let id = x => x, keepMap(id)
  // 옵션에서 꺼내려고 하지 말기
  // point-style
  ->Array.keep(c => c == "#")
  ->count
}

let main = () => {
  open Belt

  let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample2.txt")

  let grid = input->toGrid
  let slopes: array<pos> = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  slopes->Array.map(countTrees(_, grid))->Array.map(Int.toFloat)->multipyAll->Js.log
}

main()

// float
// let _ = 1. +. 2.

// map, keep -> 데이터 구조를 보존
// reduce -> 데이터 구조를 변형
// PPAP: Parse -> Process -> Aggregate(reduce) -> Print
