def read_input : IO String := do
  let stream ← IO.getStdin
  let mut lines := []
  while true do
    let line ← stream.getLine
    if line.isEmpty then break
    lines := line :: lines
  return String.join lines.reverse
