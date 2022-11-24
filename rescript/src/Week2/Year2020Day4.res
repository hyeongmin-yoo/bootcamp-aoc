open Belt

type fieldKey = string
type fieldValue = string
type fieldPair = (fieldKey, fieldValue)

type passportData = HashMap.String.t<fieldValue>

let splitToPassportRaw = input => input->Js.String2.split("\n\n")

let normalizeSpaces = input => input->Js.String2.trim->Js.String2.replaceByRe(%re("/\s+/g"), " ")

let parseFieldPair = (input): result<fieldPair, string> => {
  let pair = input->Js.String2.split(":")
  switch pair {
  | [key, value] => Ok((key, value))
  | _ => Error(`failed to parse field: ${input}`)
  }
}

let parsePassportData = (input): result<passportData, string> => {
  input
  // 불규칙한 공백들을 정리
  ->normalizeSpaces
  // 공백을 기준으로 분할
  ->Js.String2.split(" ")
  // 분할된 문자열을 fieldPair로 파싱
  ->Array.map(parseFieldPair)
  ->Util.Result.traverse
  // 파싱된 필드들을 오브젝트로 변환
  ->Result.map(HashMap.String.fromArray)
}

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let part1 = input => {
  input
  // string -> array<string>
  ->splitToPassportRaw
  // map(string -> result<HashMapString<string>, string>)
  ->Array.map(parsePassportData)
  // map(result<HashMapString<string>, string> -> result<passport1, string>)
  ->Array.map(Result.flatMap(_, Passport1.make))
  ->Array.keepMap(Util.Result.toOption)
  // count
  ->Array.length
}

let part2 = input => {
  input
  // string -> array<string>
  ->splitToPassportRaw
  // map(string -> result<HashMapString<string>, string>)
  ->Array.map(parsePassportData)
  // map(result<HashMapString<string>, string> -> result<passport2, string>)
  ->Array.map(Result.flatMap(_, Passport1.make))
  ->Array.map(Result.flatMap(_, Passport2.make))
  ->Array.keepMap(Util.Result.toOption)
  // count
  ->Array.length
}

let main = () => {
  // let input = Util.Input.readFile("input/Week2/Year2020Day4.sample.txt")
  let input = Util.Input.readFile("input/Week2/Year2020Day4.input.txt")

  input->part1->Js.log2("part1:", _)
  input->part2->Js.log2("part2:", _)
}

let _ = main()

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/

// ===== 피드백들 =====

// type hcl = BRW | ...
// type height = Cm(int) | Inch(int)

// part1 -> type passport1
// part2 -> type passport2

// 대수적 데이터 타입
// 합타입(Sum type), 곱타입(Product type)
// 배리언트가 합타입
// 곱타입은 레코드, 오브젝트, 튜플

// Algebraic Data Type
// type color = RED | GREEN | BLUE
// type x = {a: int, b: int, c: int}
// type y = (int, int, int)
// https://green-labs.github.io/algebraic-data-type

// vFn: a -> b
// data: Array, Option
// map: (data, vFn) -> result<t, string> 이거시 모나드인가요??

// map: F(a) -> (a -> b) -> F(b)

// Applicative validation: Applicative Functor 를 이용한 validation
// https://book.purescript.org/chapter7.html

// type option<'a> = Some('a) | None
// type result<'a, 'e> = Ok('a) | Error('e)
