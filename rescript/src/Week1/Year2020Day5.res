type seatCode = string // "FBFBFBFLRL"
type rowCode = string // "FBFBFBF"
type colCode = string // "LRL"
type splitedCode = (rowCode, colCode) // ("FBFBFBF", "LRL")
type seatId = int
type coord = (int, int)

let splitCode = (seatCode): splitedCode => {
  let row = seatCode->Js.String2.slice(~from=0, ~to_=7)
  let col = seatCode->Js.String2.sliceToEnd(~from=7)
  (row, col)
}

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
  // -1 보다는 option이 좋을까...?
  seatIds->Belt.Array.reduce(-1, (prev, seatId) => {
    Js.Math.max_int(prev, seatId)
  })
}

/*
- 10개의 문자열을 파싱하여 좌석 위치를 알아낸다.
- 좌석 위치는 seat ID로 치환 됨.
- 입력값들을 모두 seat ID로 변환 후 가장 높은 seat ID를 반환함
*/
let main = () => {
  open Belt
  let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

  input
  // 인풋을 배열문자열로 변환함
  // string => array<string>
  ->Util.Input.toArray
  // 각 배열에서 문자열을 행,열로 분리
  // map(string => splitedCode)
  ->Array.map(splitCode)
  // 분리된 행,열 문자열을 좌표로 변환
  // map(splitedCode => coord)
  ->Array.map(toSeatCoord)
  // 좌표를 seatID로 변환
  // map(coord => seatId)
  ->Array.map(toSeatId)
  // 가장 높은 값을 반환
  ->findHighest
  // 출력
  ->Js.log
}

let _ = main()
