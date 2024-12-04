import Batteries
import Aoc24lean

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
  let input <- read_input

  let reports := input.splitOn "\n" |>.map (fun line => line.splitOn.map String.toNat!)

  let total1 := reports.filter reportSafeAnyDir |>.length
  let total2 := reports.filter dampSafe |>.length

  IO.println s!"{total1}, {total2}"
