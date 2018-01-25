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



let rec power a n =
  let if_30=n in
  let if_31=1 in
  if if_30<if_31
  then 1
  else let op1_32=a in
       let op2_33=let app_34=a in
                  let app_35=let op1_36=n in
                             let op2_37=1 in
                             (op1_36-op2_37) in
                  (power app_35 app_34) in
       (op1_32*op2_33)
in

let rec loop ten d two =
  let if_5=0 in
  let if_6=ten in
  if if_5<if_6
  then let djo=let app_28=2 in
               let app_29=d in
               (power app_29 app_28) in
       let amari=let op1_24=ten in
                 let op2_25=let op1_26=djo in
                            let op2_27=2 in
                            (op1_26*op2_27) in
                 (op1_24 mod op2_25) in
       let dif10=let if_20=amari in
                 let if_21=0 in
                 if if_20=if_21
                 then 0
                 else let app_22=2 in
                      let app_23=d in
                      (power app_23 app_22) in
       let dif2=let if_16=amari in
                let if_17=0 in
                if if_16=if_17
                then 0
                else let app_18=10 in
                     let app_19=d in
                     (power app_19 app_18) in
       let nokori=let op1_14=ten in
                  let op2_15=dif10 in
                  (op1_14-op2_15) in
       let nexttwo=let op1_12=two in
                   let op2_13=dif2 in
                   (op1_12+op2_13) in
       let app_7=nokori in
       let app_8=let op1_10=d in
                 let op2_11=1 in
                 (op1_10+op2_11) in
       let app_9=nexttwo in
       (loop app_9 app_8 app_7)
  else two
in

let rec tentwo n =
  let app_2=n in
  let app_3=0 in
  let app_4=0 in
  (loop app_4 app_3 app_2)
in

let app_1=21 in
(tentwo app_1)






let rec power _R_1 _R_2 =
  let a=_R_1 in
  let n=_R_2 in
  let if_30=n in
  let if_31=1 in
  if if_30<if_31
  then 1
  else let op1_32=a in
       let op2_33=let app_34=a in
                  let app_35=let op1_36=n in
                             let op2_37=1 in
                             (op1_36-op2_37) in
                  let _R_1=app_34 in
                  (app_35 _R_1) in
       (op1_32*op2_33)
in

let rec loop _R_1 _R_2 _R_3 =
  let ten=_R_1 in
  let d=_R_2 in
  let two=_R_3 in
  let if_5=0 in
  let if_6=ten in
  if if_5<if_6
  then let djo=let app_28=2 in
               let app_29=d in
               let _R_1=app_28 in
               (app_29 _R_1) in
       let amari=let op1_24=ten in
                 let op2_25=let op1_26=djo in
                            let op2_27=2 in
                            (op1_26*op2_27) in
                 (op1_24 mod op2_25) in
       let dif10=let if_20=amari in
                 let if_21=0 in
                 if if_20=if_21
                 then 0
                 else let app_22=2 in
                      let app_23=d in
                      let _R_1=app_22 in
                      (app_23 _R_1) in
       let dif2=let if_16=amari in
                let if_17=0 in
                if if_16=if_17
                then 0
                else let app_18=10 in
                     let app_19=d in
                     let _R_1=app_18 in
                     (app_19 _R_1) in
       let nokori=let op1_14=ten in
                  let op2_15=dif10 in
                  (op1_14-op2_15) in
       let nexttwo=let op1_12=two in
                   let op2_13=dif2 in
                   (op1_12+op2_13) in
       let app_7=nokori in
       let app_8=let op1_10=d in
                 let op2_11=1 in
                 (op1_10+op2_11) in
       let app_9=nexttwo in
       let _R_1=app_8 in
       let _R_2=app_7 in
       (app_9 _R_2 _R_1)
  else two
in

let rec tentwo _R_1 =
  let n=_R_1 in
  let app_2=n in
  let app_3=0 in
  let app_4=0 in
  let _R_1=app_3 in
  let _R_2=app_2 in
  (app_4 _R_2 _R_1)
in

let app_1=21 in
(app_1 )
