import Aoc24lean
import Batteries

def charsToString (input: List Char) : String := input.map String.singleton |> String.join

def diagonalCoords : List (List (Nat × Nat)) :=
  let triangle1 := List.range 140 |>.map (fun x => List.range (140 - x) |>.map (fun y => (y, x + y)))
  let triangle2 := List.range 140 |>.drop 1 |>.map (fun x => List.range (140 - x) |>.map (fun y => (x + y,y)))
  triangle1 ++ triangle2

def crossDiagonalCoords : List (List (Nat × Nat)) :=
  diagonalCoords.map (fun list => list.map (fun (y, x) => (139 - y, x)))

def main : IO Unit := do
  let all_lines <- read_input

  let horizontal := all_lines.splitOn "\n" |>.dropLast
  let data := horizontal |>.map (fun line => line.toList)

  let vertical := List.range 140 |>.map (fun x => List.range 140 |>.map (fun y => data[y]![x]!) |> charsToString)
  let diagonal := diagonalCoords.map (fun coords => coords.map (fun (y, x) => data[y]![x]!) |> charsToString)
  let crossDiagonal := crossDiagonalCoords.map (fun coords => coords.map (fun (y, x) => data[y]![x]!) |> charsToString)

  let all := horizontal ++ vertical ++ diagonal ++ crossDiagonal
  let part1 := all.map (fun line =>
    let normal := line.findAllSubstr "XMAS" |>.size
    let reverse := line.findAllSubstr "SAMX" |>.size
    normal + reverse
  ) |>.sum

  let masCoords := diagonal.zip diagonalCoords |>.flatMap (fun (line, coords) =>
    let substrs := line.findAllSubstr "MAS" ++ line.findAllSubstr "SAM"
    substrs.toList |>.map (fun s =>
      let idx := Substring.startPos s |>.byteIdx
      coords[idx + 1]!
    )
  )

  let masCoordsCross := crossDiagonal.zip crossDiagonalCoords |>.flatMap (fun (line, coords) =>
    let substrs := line.findAllSubstr "MAS" ++ line.findAllSubstr "SAM"
    substrs.toList |>.map (fun s =>
      let idx := Substring.startPos s |>.byteIdx
      coords[idx + 1]!
    )
  )

  let part2 := masCoords.bagInter masCoordsCross |>.length

  IO.println s!"{part1}, {part2}"

  return
