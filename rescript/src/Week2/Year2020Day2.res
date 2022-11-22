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
let parsePair = (input): result<pair, string> => {
  // 문자열 배열로 분리 ("1-3") => ["1", "3"]
  let nums = input->Js.String2.trim->Js.String2.split("-")->Array.map(Int.fromString)

  switch nums {
  | [Some(first), Some(last)] => Ok((first, last))
  | _ => Error(`failed to parse pair: ${input}`)
  }
}

// "1-3 a: aaaa" -> result<password>
let parsePassword = (input): result<password, string> => {
  let matches =
    %re("/^(\d+-\d+) (\w): (\w+)/")
    ->Js.Re.exec_(input)
    ->Option.map(Js.Re.captures)
    ->Option.map(Array.map(_, Js.Nullable.toOption))

  switch matches {
  | Some([_, Some(pairStr), Some(char), Some(value)]) =>
    pairStr->parsePair->Result.map(pair => {pair, char, value})
  | _ => Error(`failed to parse password: ${input}`)
  }
}

let isValid1 = password => {
  let {pair, char, value} = password
  let charLength = value->Js.String2.split("")->Array.keep(String.equal(char))->Array.length
  charLength->inRange(pair)
}

let isValid2 = password => {
  let {pair, char, value} = password
  let (firstIdx, lastIdx) = pair
  let chars = value->Js.String2.split("")

  let matches = (
    chars->Array.get(firstIdx - 1)->Option.map(String.equal(char)),
    chars->Array.get(lastIdx - 1)->Option.map(String.equal(char)),
  )

  switch matches {
  | (Some(first), Some(last)) => first != last
  | _ => false
  }
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
