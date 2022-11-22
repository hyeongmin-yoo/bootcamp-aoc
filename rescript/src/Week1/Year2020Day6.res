open Belt

let sum = counts => {
  counts->Belt.Array.reduce(0, (acc, value) => acc + value)
}

let splitToGroup = input => {
  input->Js.String2.split("\n\n")
}

let splitToPerson = input => {
  input->Js.String2.split("\n")
}

let splitToAnswers = input => {
  input->Js.String2.split("")
}

let toAllAnswers = input => {
  input->Js.String2.replaceByRe(%re("/\n/g"), "")->splitToAnswers
}

let toAllIntersected = (sets: array<Set.String.t>): Set.String.t => {
  let allQuestions = "abcdefghijklmnopqrstuvwxyz"->Js.String2.split("")->Set.String.fromArray

  sets->Array.reduce(allQuestions, (prev, cur) => Set.String.intersect(prev, cur))
}

let toAllYesSet = (input): Set.String.t => {
  input
  // string -> array<string>
  ->splitToPerson
  // map(string -> array<string>)
  ->Array.map(splitToAnswers)
  // map(array<string> -> SetString)
  ->Array.map(Set.String.fromArray)
  // array<SetString> -> SetString
  ->toAllIntersected
}

let part1 = input => {
  input
  // 인풋을 그룹으로 분리 (string -> array<string>)
  ->splitToGroup

  // 각 그룹의 문자열을 해체(줄바꿈 지워버리고 모두 포함)
  // map(string -> array<string>) // ["a","b",...]
  ->Array.map(toAllAnswers)

  // 문자열을 set으로 변환
  // map(array<char> -> set<char>)
  ->Array.map(Belt.Set.String.fromArray)

  // set 사이즈를 매핑
  ->Array.map(Belt.Set.String.size)

  // 카운트를 모두 더함
  ->sum
}

let part2 = input => {
  input
  // 인풋을 그룹으로 분리 (string -> array<string>)
  ->splitToGroup

  // 그룹 내 사람들이 모두 yes 한 답안 set 반환
  // map(array<string> -> set<string>)
  ->Array.map(toAllYesSet)

  // set 사이즈를 매핑
  ->Array.map(Belt.Set.String.size)

  // 카운트를 모두 더함
  ->sum
}

let main = () => {
  // let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.sample.txt")
  let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.input.txt")

  Js.log("part1: ")
  input->part1->Js.log

  Js.log("part2: ")
  input->part2->Js.log
}

let _ = main()
