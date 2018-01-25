let rec power a n =
  if n < 1 then 1 else a * power a (n - 1) in
let rec loop ten d two =
  if ten <= 0 then two
  else
    let djo = power 2 d in
    let amari = ten mod (djo * 2) in
    let dif10 = if amari <> 0 then power 2 d else 0 in
    let dif2 = if amari <> 0 then power 10 d else 0 in
    let nokori = ten - dif10 in
    let nexttwo = two + dif2 in
    loop nokori (d + 1) nexttwo in
let rec tentwo n = loop n 0 0 in
tentwo 21
