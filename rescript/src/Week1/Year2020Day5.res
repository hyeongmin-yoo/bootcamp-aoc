type row = F | B
type col = L | R
type seatCode = (array<row>, array<col>)
type seatId = int

type coord = (int, int)
type parseResult<'a> = result<array<'a>, string>

let makeParser = (parseChar: string => result<'a, string>, input): parseResult<'a> => {
  // chars: ["F", "B", "F"...]
  let rec parse = (chars: array<string>, prev: parseResult<'a>) => {
    open Belt
    chars
    ->Array.get(0)
    ->Option.map(char =>
      char
      ->parseChar
      ->Result.flatMap(unit => prev->Result.map(Array.concat(_, [unit])))
      ->Result.flatMap(units => parse(chars->Array.sliceToEnd(1), Ok(units)))
    )
    ->Option.getWithDefault(prev)
  }

  parse(input->Util.String.toArray, Ok([]))
}

let parseRow = makeParser(char => {
  switch char {
  | "F" => Ok(F)
  | "B" => Ok(B)
  | _ => Error("unexpected character")
  }
})

let parseCol = makeParser(char => {
  switch char {
  | "L" => Ok(L)
  | "R" => Ok(R)
  | _ => Error("unexpected character")
  }
})

let parseSeatCode = (text): result<seatCode, string> => {
  if text->Js.String2.length != 10 {
    Error("invalid input size")
  } else {
    let (rowText, colText) = text->Util.String.divide(7)

    rowText
    ->parseRow
    ->Belt.Result.flatMap(row => {
      colText->parseCol->Belt.Result.flatMap(col => Ok(row, col))
    })
  }
}

let calcRowPos = (rows: array<row>): int => {
  let (rowNum, _) = rows->Belt.Array.reduce((0, 127), (prev, code) => {
    let (start, end) = prev
    switch code {
    | F => (start, (end - start) / 2 + start)
    | B => ((end - start + 1) / 2 + start, end)
    }
  })
  rowNum
}

// col -> int
let calcColPos = (cols: array<col>): int => {
  let (colNum, _) = cols->Belt.Array.reduce((0, 7), (prev, code) => {
    let (start, end) = prev
    switch code {
    | L => (start, (end - start) / 2 + start)
    | R => ((end - start + 1) / 2 + start, end)
    }
  })
  colNum
}

let toSeatCoord = ((rows, cols): seatCode): coord => {
  (calcRowPos(rows), calcColPos(cols))
}

let toSeatId = (coord): seatId => {
  let (row, col) = coord
  row * 8 + col
}

let parseSeatId = text => {
  open Belt
  text
  // string => seatCode // seatCode: (array<row>, array<col>)
  ->parseSeatCode

  // 분리된 행,열 문자열을 좌표로 변환
  // (seatCode => coord) // coord: (int, int)
  ->Result.map(toSeatCoord)

  // 좌표를 seatID로 변환
  // map(coord => seatId) // seatId: int
  ->Result.map(toSeatId)
}

let rec findMissingId = (seatIds: array<seatId>): option<seatId> => {
  open Belt
  seatIds
  ->Array.get(0)
  ->Option.flatMap(current => seatIds->Array.get(1)->Option.map(next => (current, next)))
  ->Option.flatMap(((current, next)) =>
    next - current > 1 ? Some(current + 1) : findMissingId(seatIds->Array.sliceToEnd(1))
  )
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

  // 문자열을 Id로 파싱
  // map(string => result<seatId, string>) // seatId: int
  ->Array.map(parseSeatId)

  // 실패한 값을 필터링하고 result 에서 값을 꺼냄.
  // TODO: array<result> 조합에서 Error가 하나라도 있는 경우 Error 처리를 하고 싶다면...?
  // result<seatId, string> => option<seatId> => seatId
  ->Array.keepMap(Util.Result.toOption)

  // 순차적으로 없는 항목을 찾기 위해 id 정렬
  // array<seatId> => array<seatId>
  ->SortArray.stableSortBy((a, b) => a - b)

  // 빈 값을 찾아서 반환
  // array<seatId> => option<seatId>
  ->findMissingId

  // 출력
  ->Js.log
}

let _ = main()
