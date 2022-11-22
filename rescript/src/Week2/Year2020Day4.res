open Belt
// --- Day 4: Passport Processing ---
// part1
/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/
type fieldKey = string
type fieldValue = string
type fieldPair = (fieldKey, fieldValue)
type len = Cm(int) | Inch(int)

type passportData = HashMap.String.t<fieldValue>
type passportProgress2 = {
  byr: result<int, string>,
  iyr: result<int, string>,
  eyr: result<int, string>,
  hgt: result<len, string>,
  hcl: result<string, string>,
  ecl: result<string, string>,
  pid: result<string, string>,
  cid: option<string>,
}
type passport2 = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: len,
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
  // 공백을 기준으로 분할
  ->Js.String2.split(" ")
  // 분할된 문자열을 fieldPair로 파싱
  ->Array.map(parseFieldPair)
  ->Util.Result.traverse
  // 파싱된 필드들을 오브젝트로 변환
  ->Result.map(HashMap.String.fromArray)

  /*
   TODO: 아래와 같이 키를 동적으로 할당하려면 어떤 문법이 있는지.

   ```ts
   function foo ([key, value]: pair) {
      return { [key]: value }
   }
   ```
 */
}

let parseYear = input =>
  input
  ->Util.String.getExactlyMatched(%re("/^\d{4}$/"))
  ->Option.flatMap(Int.fromString)
  ->Util.Result.fromOption(Error(`invalid format (year): ${input}`))

let parseBirthYear = input => {
  input
  ->parseYear
  ->Result.flatMap(year =>
    year->Util.Range.inRange(1920, 2002) ? Ok(year) : Error(`invalid byr: ${input}`)
  )
}

let parseIssueYear = input => {
  input
  ->parseYear
  ->Result.flatMap(year =>
    year->Util.Range.inRange(2010, 2020) ? Ok(year) : Error(`invalid iyr: ${input}`)
  )
}

let parseExpYear = input => {
  input
  ->parseYear
  ->Result.flatMap(year =>
    year->Util.Range.inRange(2020, 2030) ? Ok(year) : Error(`invalid eyr: ${input}`)
  )
}

let parseLen = (input): result<len, string> => {
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

let parseHairColor = input => {
  input
  ->Util.String.getExactlyMatched(%re("/^#[0-9a-f]{6}$/"))
  ->Util.Result.fromOption(Error(`invalid hcl: ${input}`))
}

let parseEyeColor = input => {
  switch input {
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => Ok(input)
  | _ => Error(`invalid ecl: ${input}`)
  }
}

let parsePassportID = input => {
  input
  ->Util.String.getExactlyMatched(%re("/^\d{9}$/"))
  ->Util.Result.fromOption(Error(`invalid pid: ${input}`))
}

let getRequired = (map: passportData, key: string): result<string, string> => {
  map->HashMap.String.get(key)->Util.Result.fromOption(Error(`${key} required`))
}

let parsePassport2 = (map: passportData): result<passport2, string> => {
  let prog: passportProgress2 = {
    byr: map->getRequired("byr")->Result.flatMap(parseBirthYear),
    iyr: map->getRequired("iyr")->Result.flatMap(parseIssueYear),
    eyr: map->getRequired("eyr")->Result.flatMap(parseExpYear),
    hgt: map->getRequired("hgt")->Result.flatMap(parseHeight),
    hcl: map->getRequired("hcl")->Result.flatMap(parseHairColor),
    ecl: map->getRequired("ecl")->Result.flatMap(parseEyeColor),
    pid: map->getRequired("pid")->Result.flatMap(parsePassportID),
    cid: map->HashMap.String.get("cid"),
  }
  switch prog {
  | {
      byr: Ok(byr),
      iyr: Ok(iyr),
      eyr: Ok(eyr),
      hgt: Ok(hgt),
      hcl: Ok(hcl),
      ecl: Ok(ecl),
      pid: Ok(pid),
      cid,
    } =>
    Ok({byr, iyr, eyr, hgt, hcl, ecl, pid, cid})

  | _ =>
    [
      prog.byr->Util.Result.toErrSome,
      prog.iyr->Util.Result.toErrSome,
      prog.eyr->Util.Result.toErrSome,
      prog.hgt->Util.Result.toErrSome,
      prog.hcl->Util.Result.toErrSome,
      prog.ecl->Util.Result.toErrSome,
      prog.pid->Util.Result.toErrSome,
    ]
    ->Util.Option.concatSomes
    ->Js.Array2.joinWith(_, ", ")
    ->Util.String.withDefault("fields are not fullfiled")
    ->Error
  }
}

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let part1 = input => {
  input
  ->splitToPassportRaw
  ->Array.map(normalizeSpaces)
  ->Array.map(parsePassportData)
  ->Array.map(Result.flatMap(_, parsePassport2))
  ->Array.keepMap(Util.Result.toOption)
  ->Array.length
}

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/

let main = () => {
  //   let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.sample.txt")
  let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.input.txt")

  input->part1->Js.log2("part2:", _)
}

let _ = main()
