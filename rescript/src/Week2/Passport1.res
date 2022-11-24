open Belt

type passportData = HashMap.String.t<string>

type t = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let make = (data: passportData): result<t, string> => {
  let results = (
    data->HashMap.String.get("byr"),
    data->HashMap.String.get("iyr"),
    data->HashMap.String.get("eyr"),
    data->HashMap.String.get("hgt"),
    data->HashMap.String.get("hcl"),
    data->HashMap.String.get("ecl"),
    data->HashMap.String.get("pid"),
    data->HashMap.String.get("cid"),
  )
  switch results {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), cid) =>
    Ok({byr, iyr, eyr, hgt, hcl, ecl, pid, cid})
  | _ => Error("fields are not fullfiled")
  }
}
