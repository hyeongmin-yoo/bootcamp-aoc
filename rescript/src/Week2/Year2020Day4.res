open Belt
// --- Day 4: Passport Processing ---
// part1
/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/
type fieldKey = string
type fieldValue = string
type fieldPair = (fieldKey, fieldValue)

type passportData = HashMap.String.t<fieldValue>

type passport1 = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/
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

let parsePassport1 = (map: passportData): result<passport1, string> => {
  let results = (
    map->HashMap.String.get("byr"),
    map->HashMap.String.get("iyr"),
    map->HashMap.String.get("eyr"),
    map->HashMap.String.get("hgt"),
    map->HashMap.String.get("hcl"),
    map->HashMap.String.get("ecl"),
    map->HashMap.String.get("pid"),
    map->HashMap.String.get("cid"),
  )
  switch results {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), cid) =>
    Ok({byr, iyr, eyr, hgt, hcl, ecl, pid, cid})
  | _ => Error("fields are not fullfiled")
  }
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
  ->Array.map(Result.flatMap(_, parsePassport1))
  ->Array.keepMap(Util.Result.toOption)
  ->Array.length
}

// ===== Part 2. =====
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/
type year = int
type hgt = Cm(int) | Inch(int)
type hcl = string
type ecl = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
type id = string

type passport2 = {
  byr: year,
  iyr: year,
  eyr: year,
  hgt: hgt,
  hcl: hcl,
  ecl: ecl,
  pid: id,
  cid: option<id>,
}

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

let getRegMatched = (input, re) => {
  re
  ->Js.Re.exec_(input)
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.flatMap(Belt.Array.get(_, 0))
  ->Belt.Option.flatMap(Js.Nullable.toOption)
}

let parseYear = (input): result<year, string> =>
  input
  ->getRegMatched(%re("/^\d{4}$/"))
  ->Option.flatMap(Int.fromString)
  ->Util.Result.fromOption(Error(`invalid format (year): ${input}`))

let parseYearInRange = (input, ~min: int, ~max: int): result<year, string> => {
  input
  ->parseYear
  ->Result.flatMap(year =>
    year->Util.Range.inRange(min, max)
      ? Ok(year)
      : Error(`out of range (${min->Int.toString}~${max->Int.toString}): ${input}`)
  )
}

let parseLen = (input): result<hgt, string> => {
  %re("/^(\d+)(cm|in)/")
  ->Js.Re.exec_(input)
  ->Option.map(Js.Re.captures)
  ->Option.flatMap(matches => {
    switch matches {
    | [_, num, unit] =>
      num
      ->Js.Nullable.toOption
      ->Option.flatMap(Int.fromString)
      ->Option.flatMap(n => {
        switch unit->Js.Nullable.toOption {
        | Some(val) if val == "cm" => Some(Cm(n))
        | Some(val) if val == "in" => Some(Inch(n))
        | _ => None
        }
      })
    | _ => None
    }
  })
  ->Util.Result.fromOption(Error(`invalid format (len): ${input}`))
}

let parseHeight = input => {
  input
  ->parseLen
  ->Result.flatMap(len => {
    switch len {
    | Cm(val) =>
      val->Util.Range.inRange(150, 193)
        ? Ok(Cm(val))
        : Error(`out of range height(cm): ${val->Int.toString}`)
    | Inch(val) =>
      val->Util.Range.inRange(59, 76)
        ? Ok(Inch(val))
        : Error(`out of range height(in): ${val->Int.toString}`)
    }
  })
}

let parseHairColor = (input): result<hcl, string> => {
  input
  ->getRegMatched(%re("/^#[0-9a-f]{6}$/"))
  ->Util.Result.fromOption(Error(`invalid hcl: ${input}`))
}

let parseEyeColor = (input): result<ecl, string> => {
  switch input {
  | "amb" => Amb->Ok
  | "blu" => Blu->Ok
  | "brn" => Brn->Ok
  | "gry" => Gry->Ok
  | "grn" => Grn->Ok
  | "hzl" => Hzl->Ok
  | "oth" => Oth->Ok
  | _ => Error(`invalid ecl: ${input}`)
  }
}

let parsePassportID = (input): result<id, string> => {
  input->getRegMatched(%re("/^\d{9}$/"))->Util.Result.fromOption(Error(`invalid pid: ${input}`))
}

let getValueWithParser = (
  map: passportData,
  key: string,
  parser: string => result<'a, string>,
): result<'a, string> => {
  map
  ->HashMap.String.get(key)
  ->Util.Result.fromOption(Error(`${key} required`))
  ->Result.flatMap(parser)
}

// vFn: a -> b
// data: Array, Option
// map: (data, vFn) -> result<t, string> 이거시 모나드인가요??

// map: F(a) -> (a -> b) -> F(b)

let parsePassport2 = (map: passportData): result<passport2, string> => {
  let results = (
    map->getValueWithParser("byr", parseYearInRange(~min=1920, ~max=2002)),
    map->getValueWithParser("iyr", parseYearInRange(~min=2010, ~max=2020)),
    map->getValueWithParser("eyr", parseYearInRange(~min=2020, ~max=2030)),
    map->getValueWithParser("hgt", parseHeight),
    map->getValueWithParser("hcl", parseHairColor),
    map->getValueWithParser("ecl", parseEyeColor),
    map->getValueWithParser("pid", parsePassportID),
    map->HashMap.String.get("cid"),
  )
  switch results {
  | (Ok(byr), Ok(iyr), Ok(eyr), Ok(hgt), Ok(hcl), Ok(ecl), Ok(pid), cid) =>
    Ok({byr, iyr, eyr, hgt, hcl, ecl, pid, cid})

  // 가능하면 파싱하면서 받은 에러메시지를 노출함.
  | (byr, iyr, eyr, hgt, hcl, ecl, pid, _) =>
    [
      // 각 result 의 'a 타입이 달라 이것만으로 배열을 만들 수 없음.
      // 배열 아이템의 타입을 맞추기 위해 option으로 변경함.
      // result<'a, string> -> result<string, 'a> -> option<string>
      byr->Util.Result.swap->Util.Result.toOption,
      iyr->Util.Result.swap->Util.Result.toOption,
      eyr->Util.Result.swap->Util.Result.toOption,
      hgt->Util.Result.swap->Util.Result.toOption,
      hcl->Util.Result.swap->Util.Result.toOption,
      ecl->Util.Result.swap->Util.Result.toOption,
      pid->Util.Result.swap->Util.Result.toOption,
    ]
    // array<option<string>> -> option<array<string>>
    ->Util.Option.traverse
    // option<array<string>> -> option<string>
    ->Option.map(Js.Array2.joinWith(_, ", "))
    ->Option.getWithDefault("fields are not fullfiled")
    ->Error
  }
}
// Applicative validation: Applicative Functor 를 이용한 validation
// https://book.purescript.org/chapter7.html

// type option<'a> = Some('a) | None
// type result<'a, 'e> = Ok('a) | Error('e)

let part2 = input => {
  input
  // string -> array<string>
  ->splitToPassportRaw
  // map(string -> result<HashMapString<string>, string>)
  ->Array.map(parsePassportData)
  // map(result<HashMapString<string>, string> -> result<passport2, string>)
  ->Array.map(Result.flatMap(_, parsePassport2))
  ->Array.keepMap(Util.Result.toOption)
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
