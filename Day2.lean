import Batteries

def reportSafe : List Nat → Bool
| [] => true
| (x :: xs) => Id.run do
  let mut prev := x;
  for x in xs do
    if x < prev + 1 || prev + 3 < x then
      return false
    prev := x
  true

def reportSafeAnyDir (report : List Nat) : Bool :=
  reportSafe report || reportSafe report.reverse

def dampSafe (report : List Nat) : Bool := Id.run do
  for i in [:report.length] do
    if report |>.dropSlice i 1 |> reportSafeAnyDir then
      return true
  return false

def main : IO Unit := do
  let stream ← IO.getStdin

  let mut reports := []
  while true do
    let line ← stream.getLine
    if line.isEmpty then break

    let report := line.trim.splitOn.map (fun x => x.toNat!)
    reports := report :: reports

  let total1 := reports.filter reportSafeAnyDir |>.length
  let total2 := reports.filter dampSafe |>.length

  IO.println s!"{total1}, {total2}"
