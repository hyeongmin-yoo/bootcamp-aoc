type seatCode = string // "FBFBFBFLRL"
type rowCode = string // "FBFBFBF"
type colCode = string // "LRL"
type splitedCode = (rowCode, colCode) // ("FBFBFBF", "LRL")
type seatId = int

type row = int
type col = int
type coord = (row, col)

let splitCode = (seatCode): splitedCode => {
  let row = seatCode->Js.String2.slice(~from=0, ~to_=7)
  let col = seatCode->Js.String2.sliceToEnd(~from=7)
  (row, col)
}

// type row = F | B
// type col = L | R

// row -> int
let calcRowPos = (code: rowCode): int => {
  let (row, _) =
    code
    ->Util.String.toArray
    ->Belt.Array.reduce((0, 127), (prev, code) => {
      let (start, end) = prev
      switch code {
      | "F" => (start, (end - start) / 2 + start)
      | "B" => ((end - start + 1) / 2 + start, end)
      | _ => prev
      }
    })
  row
}

// col -> int
let calcColPos = (code: colCode): int => {
  let (col, _) =
    code
    ->Util.String.toArray
    ->Belt.Array.reduce((0, 7), (prev, code) => {
      let (start, end) = prev
      switch code {
      | "L" => (start, (end - start) / 2 + start)
      | "R" => ((end - start + 1) / 2 + start, end)
      | _ => prev
      }
    })
  col
}

let toSeatCoord = ((rowCode, colCode): splitedCode): coord => {
  (calcRowPos(rowCode), calcColPos(colCode))
}

let toSeatId = (coord): seatId => {
  let (row, col) = coord
  row * 8 + col
}

let findHighest = (seatIds: array<seatId>): seatId => {
  // -1 보다는 option이 좋을까... // -> -Inifinity // -> Math.max many_int
  seatIds->Belt.Array.reduce(-1, (prev, seatId) => {
    Js.Math.max_int(prev, seatId)
  })
}

// sliding window
// input -> array<(pre, curr)> -> ((pre, curr) => curr-pre > 1)

type findingCase = Start | Process(int) | Found(int)
let findBlankId = (seatIds: array<seatId>): option<seatId> => {
  // 중도에 일찍 반환할 수 있다면 좋을 듯. // -> 재귀
  let findingCase = seatIds->Belt.Array.reduce(Start, (prev: findingCase, seatId) => {
    switch prev {
    | Found(id) => Found(id)
    | Process(id) if seatId - id > 1 => Found(seatId - 1)
    | _ => Process(seatId)
    }
  })

  switch findingCase {
  | Found(val) => Some(val)
  | _ => None
  }
}

/*
- 10개의 문자열을 파싱하여 좌석 위치를 알아낸다.
- 좌석 위치는 seat ID로 치환 됨.
- 입력값들을 모두 seat ID로 변환 후 
- part1: 가장 높은 seat ID를 반환함
- part2: 순차적인 목록에서 비어있는 seat ID 반환
*/
let main = () => {
  open Belt
  let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

  input
  // 인풋을 배열문자열로 변환함
  // string => array<string>
  ->Util.Input.toArray

  // 각 배열에서 문자열을 행,열로 분리
  // map(string => splitedCode) // splitedCode: (rowCode: string, colCode: string)
  ->Array.map(splitCode)

  // 분리된 행,열 문자열을 좌표로 변환
  // map(splitedCode => coord) // coord: (int, int)
  ->Array.map(toSeatCoord)

  // 좌표를 seatID로 변환
  // map(coord => seatId) // seatId: int
  ->Array.map(toSeatId)

  // 순차적으로 없는 항목을 찾기 위해 id 정렬
  // array<seatId> => array<seatId>
  ->SortArray.stableSortBy((a, b) => a - b)

  // 빈 값을 찾아서 반환
  // array<seatId> => option<seatId>
  ->findBlankId

  // 출력
  ->Js.log
}

let _ = main()
