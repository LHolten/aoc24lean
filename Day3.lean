import Aoc24lean

def readArgs (args : String) : Option Nat := do
  let arg1 := args.takeWhile (fun x => x.isDigit)
  let args := args.drop arg1.length
  if !args.startsWith "," then none
  let args := args.drop 1
  let arg2 := args.takeWhile (fun x => x.isDigit)
  let args := args.drop arg2.length
  if !args.startsWith ")" then none
  return (<- arg1.toNat?) * (<- arg2.toNat?)

def String.execute (input : String) : Nat := input.splitOn "mul(" |>.drop 1 |>.filterMap readArgs |>.sum
def String.execute_smart (input : String) : Nat := input.splitOn "do()" |>.map (fun line => line.splitOn "don't()" |>.get! 0 |>.execute) |>.sum

def main : IO Unit := do
  let all_lines <- read_input

  let part1 := all_lines.execute
  let part2 := all_lines.execute_smart

  IO.println s!"{part1}, {part2}"