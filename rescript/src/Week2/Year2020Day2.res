open Belt

type pair = (int, int)
type password = {
  pair: pair,
  char: string,
  value: string,
}

let inRange = (target, (min, max): pair) => {
  min <= target && target <= max
}

// "1-3" -> (1, 3)
let parsePair = input => {
  // 문자열 배열로 분리 ("1-3") => ["1", "3"]
  let nums = input->Js.String2.trim->Js.String2.split("-")

  nums
  ->Array.get(0)
  ->Option.flatMap(Int.fromString)
  ->Option.flatMap(min =>
    nums->Array.get(1)->Option.flatMap(Int.fromString)->Option.map(max => (min, max))
  )
}

// "1-3 a: aaaa" -> result<password>
let parsePassword = (input): result<password, string> => {
  // 정규 표현식에 맞춰서 배열을 반환
  %re("/^(\d+-\d+) (\w): (\w+)/")
  ->Js.Re.exec_(input)
  ->Option.map(Js.Re.captures)
  ->Option.flatMap(matches => {
    /*
    TODO: 여러개의 options 병합하여 계산 시 .flatMap 으로 합쳐가면서 꺼내는 방법말곤 없는 지.
 */
    let rangeMatched = matches[1]->Option.flatMap(Js.Nullable.toOption)
    let charMatched = matches[2]->Option.flatMap(Js.Nullable.toOption)
    let valueMatched = matches[3]->Option.flatMap(Js.Nullable.toOption)

    rangeMatched
    ->Option.flatMap(parsePair)
    ->Option.flatMap(pair => charMatched->Option.map(char => (pair, char)))
    ->Option.flatMap(((pair, char)) => valueMatched->Option.map(value => {pair, char, value}))
  })
  ->Util.Result.fromOption(Error(`invalid input: ${input}`))
}

let isValid1 = password => {
  let {char, pair, value} = password
  let charLength = value->Js.String2.split("")->Array.keep(String.equal(char))->Array.length
  charLength->inRange(pair)
}

let isValid2 = password => {
  let {char, pair, value} = password
  let (firstIdx, lastIdx) = pair
  let chars = value->Js.String2.split("")

  chars
  // 첫번째 인덱스의 문자열을 가져와 대상 문자열과 맞는지 bool
  ->Array.get(firstIdx - 1)
  ->Option.map(String.equal(char))
  ->Option.flatMap(firstMatched =>
    chars
    // 마지막 인덱스의 문자열을 가져와 비교 bool
    ->Array.get(lastIdx - 1)
    ->Option.map(String.equal(char))
    // 앞의 매칭과 뒤의 매칭이 동일하지 않아야 함 (둘 중 하나만 true)
    ->Option.map(lastMatched => firstMatched != lastMatched)
  )
  // 둘 중 하나라도 값이 없다면 false.
  ->Option.getWithDefault(false)
}

let part1 = input => {
  input
  // 인풋을 라인별로 분리
  ->Js.String2.split("\n")

  // 라인을 각 정보로 파싱
  // map(string => result<password>)
  ->Array.map(parsePassword)

  // array<result<password>> => result<array<password>>
  ->Util.Result.traverse

  // 파싱된 패스워드가 valid 한 항목만 필터링
  ->Result.map(Array.keep(_, isValid1))

  // 카운트함.
  ->Result.map(Array.length)
}

let part2 = input => {
  input
  // 인풋을 라인별로 분리
  ->Js.String2.split("\n")

  // 라인을 각 정보로 파싱
  // map(string => result<password>)
  ->Array.map(parsePassword)

  // array<result<password>> => result<array<password>>
  ->Util.Result.traverse

  // 파싱된 패스워드가 valid 한 항목만 필터링
  ->Result.map(Array.keep(_, isValid2))

  // 카운트함.
  ->Result.map(Array.length)
}

let main = () => {
  // let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.sample.txt")
  let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.input.txt")

  Js.log("part 1.")
  input->part1->Util.Result.print

  Js.log("part 2.")
  input->part2->Util.Result.print
}

let _ = main()
