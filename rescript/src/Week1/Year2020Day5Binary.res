type seatId = int

let toBinaryFloat = raw => {
  let num = Js.Float.fromString(`0b${raw}`)
  num == Js.Float._NaN ? None : Some(num)
}

let parseSeatId = (text): option<seatId> => {
  text
  ->Util.String.toArray
  ->Belt.Array.map(char =>
    switch char {
    | "F" | "L" => Ok("0")
    | "B" | "R" => Ok("1")
    | _ => Error("unexpected letter")
    }
  )
  // array<either> -> either<array<string>, string>
  ->Util.Result.traverse
  // either<array<string>, string> -> option<array<string>>
  ->Util.Result.toOption
  ->Belt.Option.map(arr => Js.Array2.joinWith(arr, ""))
  ->Belt.Option.flatMap(toBinaryFloat)
  ->Belt.Option.map(Belt.Float.toInt)
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
  // map(string => option<seatId>) // seatId: int
  ->Array.keepMap(parseSeatId)

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
