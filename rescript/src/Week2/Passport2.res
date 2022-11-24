open Belt

type year = int
type hgt = Cm(int) | Inch(int)
type hcl = string
type ecl = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
type id = string

type t = {
  byr: year,
  iyr: year,
  eyr: year,
  hgt: hgt,
  hcl: hcl,
  ecl: ecl,
  pid: id,
  cid: option<id>,
}

let parseYear = (input): result<year, string> =>
  input
  ->Util.String.getMatchs(%re("/^\d{4}$/"))
  ->Option.flatMap(Array.get(_, 0))
  ->Option.flatMap(Int.fromString)
  ->Util.Result.fromOption(Error(`invalid year: ${input}`))

let parseYearInRange = (input, ~min: int, ~max: int): result<year, string> => {
  input
  ->parseYear
  ->Result.flatMap(year =>
    year->Util.Range.inRange(min, max)
      ? Ok(year)
      : Error(`out of range year (${min->Int.toString}~${max->Int.toString}): ${input}`)
  )
}

let parseHeight = (input): result<hgt, string> => {
  let toHgt = (num, unit) =>
    switch unit {
    | "cm" => Cm(num)->Some
    | "in" => Inch(num)->Some
    | _ => None
    }

  input
  ->Util.String.getMatchs(%re("/^(\d+)(cm|in)/"))
  ->Option.flatMap(matches =>
    switch matches {
    | [_, num, unit] => num->Int.fromString->Option.flatMap(toHgt(_, unit))
    | _ => None
    }
  )
  ->Util.Result.fromOption(Error(`invalid height: ${input}`))
}

let parseHeightInRange = (input, ~cm: (int, int), ~inch: (int, int)): result<hgt, string> => {
  input
  ->parseHeight
  ->Result.flatMap(height => {
    switch height {
    | Cm(val) => {
        let (min, max) = cm
        val->Util.Range.inRange(min, max)
          ? Ok(Cm(val))
          : Error(`out of range height(cm: ${min->Int.toString}~${max->Int.toString}): ${input}`)
      }

    | Inch(val) => {
        let (min, max) = inch
        val->Util.Range.inRange(min, max)
          ? Ok(Cm(val))
          : Error(`out of range height(inch: ${min->Int.toString}~${max->Int.toString}): ${input}`)
      }
    }
  })
}

let parseHairColor = (input): result<hcl, string> => {
  input
  ->Util.String.getMatchs(%re("/^#[0-9a-f]{6}$/"))
  ->Option.flatMap(Array.get(_, 0))
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
  input
  ->Util.String.getMatchs(%re("/^\d{9}$/"))
  ->Option.flatMap(Array.get(_, 0))
  ->Util.Result.fromOption(Error(`invalid pid: ${input}`))
}

let make = (data: Passport1.t): result<t, string> => {
  let results = (
    data.byr->parseYearInRange(~min=1920, ~max=2002),
    data.iyr->parseYearInRange(~min=2010, ~max=2020),
    data.eyr->parseYearInRange(~min=2020, ~max=2030),
    data.hgt->parseHeightInRange(~cm=(150, 193), ~inch=(59, 76)),
    data.hcl->parseHairColor,
    data.ecl->parseEyeColor,
    data.pid->parsePassportID,
    data.cid,
  )
  switch results {
  // Ok
  | (Ok(byr), Ok(iyr), Ok(eyr), Ok(hgt), Ok(hcl), Ok(ecl), Ok(pid), cid) =>
    Ok({byr, iyr, eyr, hgt, hcl, ecl, pid, cid})

  // Err: 가능하면 파싱하면서 받은 에러메시지를 노출함.
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
