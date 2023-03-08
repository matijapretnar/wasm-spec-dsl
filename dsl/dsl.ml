type 'a meta = Meta of string | Concrete of 'a

let meta_latex f = function Meta x -> x | Concrete t -> f t

(* NUMTYPE *)

type numtype = numtype' meta
and numtype' = I32

let i32 = Concrete I32
let latex_numtype = meta_latex @@ function I32 -> "i32"

(* VALTYPE *)

type valtype = valtype' meta
and valtype' = NumType of numtype

let numtype t = Concrete (NumType t)
let latex_valtype = meta_latex @@ function NumType nt -> latex_numtype nt

(* NUMINSTR *)

type sx = sx' meta
and sx' = U

let latex_sx = meta_latex @@ function U -> "u"

type ibinop = ibinop' meta
and ibinop' = Add | Sub | Mul

let latex_ibinop =
  meta_latex @@ function Add -> "add" | Sub -> "sub" | Mul -> "mul"

type irelop = irelop' meta
and irelop' = Eq | Ne | Lt of sx

let latex_irelop =
  meta_latex @@ function
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt sx -> Printf.sprintf "%slt" (latex_sx sx)

type i32 = int meta

let latex_i32 = meta_latex @@ string_of_int

type num_instr = num_instr' meta
and num_instr' = IConst of i32 | IBinop of ibinop | IRelop of irelop

let latex_num_instr =
  meta_latex @@ function
  | IConst n -> Printf.sprintf "i32.const %s" (latex_i32 n)
  | IBinop op -> Printf.sprintf "i32.%s" (latex_ibinop op)
  | IRelop op -> Printf.sprintf "i32.%s" (latex_irelop op)

(* CONTROLINSTR & INSTR *)

type instr = instr' meta
and control_instr = NOp | If of expr * expr
and instr' = NumInstr of num_instr | ControlInstr of control_instr
and expr = instr list

let iconst n = Concrete (NumInstr (Concrete (IConst n)))
let ibinop op = Concrete (NumInstr (Concrete (IBinop op)))
let irelop op = Concrete (NumInstr (Concrete (IRelop op)))
let nop = Concrete (ControlInstr NOp)
let if_ e1 e2 = Concrete (ControlInstr (If (e1, e2)))

let rec latex_control_instr = function
  | NOp -> "nop"
  | If (es1, es2) ->
      Printf.sprintf "if %s else %s end" (latex_instrs es1) (latex_instrs es2)

and latex_instrs es = es |> List.map latex_instr |> String.concat " "
and latex_instr e = meta_latex latex_instr' e

and latex_instr' = function
  | NumInstr e -> latex_num_instr e
  | ControlInstr e -> latex_control_instr e

(* JUDGEMENTS *)

type sequent = { instr : instr; ty : valtype list * valtype list }

type judgement =
  | ForallIBinop of string * (ibinop -> judgement)
  | ForallIRelop of string * (irelop -> judgement)
  | ForallInt of string * (i32 -> judgement)
  | ForallInstr of string * (instr -> judgement)
  | ForallValty of string * (valtype -> judgement)
  | Judgement of { hyp : sequent list; conc : sequent }

let sequent instr ty = { instr; ty }
let judgement hyp conc = Judgement { hyp; conc }
let forall_ibinop op judg = ForallIBinop (op, judg)
let forall_irelop op judg = ForallIRelop (op, judg)
let forall_int n judg = ForallInt (n, judg)
let forall_instr e judg = ForallInstr (e, judg)
let forall_valty t judg = ForallValty (t, judg)

let latex_valtypes ts =
  ts |> List.map latex_valtype |> String.concat " " |> Format.sprintf "[%s]"

let latex_sequent { instr; ty = ty1, ty2 } =
  Format.sprintf "\\vdash %s : %s \\to %s" (latex_instr instr)
    (latex_valtypes ty1) (latex_valtypes ty2)

let text_sequent { instr; ty = ty1, ty2 } =
  Format.sprintf "$%s$ takes $%s$ and returns $%s$" (latex_instr instr)
    (latex_valtypes ty1) (latex_valtypes ty2)

let rec latex_judgement = function
  | ForallIBinop (n, j) -> latex_judgement (j (Meta n))
  | ForallIRelop (n, j) -> latex_judgement (j (Meta n))
  | ForallInt (n, j) -> latex_judgement (j (Meta n))
  | ForallInstr (n, j) -> latex_judgement (j (Meta n))
  | ForallValty (n, j) -> latex_judgement (j (Meta n))
  | Judgement { hyp; conc } ->
      let latex_hyp = List.map (fun hy -> "  " ^ latex_sequent hy ^ "\n") hyp
      and latex_conc = latex_sequent conc in
      Format.sprintf "\\frac{\n%s}{\n  %s\n}"
        (String.concat "\n\\qquad\n" latex_hyp)
        latex_conc

let rec text_judgement = function
  | ForallIBinop (n, j) ->
      Format.sprintf "For all binary operations %s:\n%s" n
        (text_judgement (j (Meta n)))
  | ForallIRelop (n, j) ->
      Format.sprintf "For all binary relations %s:\n%s" n
        (text_judgement (j (Meta n)))
  | ForallInt (n, j) ->
      Format.sprintf "For all integers %s:\n%s" n (text_judgement (j (Meta n)))
  | ForallInstr (n, j) ->
      Format.sprintf "For all instructions %s:\n%s" n
        (text_judgement (j (Meta n)))
  | ForallValty (n, j) ->
      Format.sprintf "For all value types %s:\n%s" n
        (text_judgement (j (Meta n)))
  | Judgement { hyp; conc } -> (
      let text_hyps = List.map text_sequent hyp
      and text_conc = text_sequent conc in
      let hyp_list =
        text_hyps
        |> List.mapi (fun i hyp -> Format.sprintf "  %d. %s" (i + 1) hyp)
      in
      match text_hyps with
      | [] -> Format.sprintf "We have:\n  %s." text_conc
      | _ :: _ ->
          Format.sprintf "If:\n%s\nthen:\n  %s."
            (String.concat ", \n" hyp_list)
            text_conc)

(* RULES *)

type rules = Rules of judgement list

let latex_rules (Rules rules) =
  Format.sprintf "$$\n%s\n$$"
    (List.map latex_judgement rules |> String.concat "\n\\qquad\n")

let text_rules (Rules rules) =
  Format.sprintf "%s" (List.map text_judgement rules |> String.concat "\n\n")

(* EXAMPLE *)

let rules =
  Rules
    [
      ( forall_int "m" @@ fun n ->
        judgement [] (sequent (iconst n) ([], [ numtype i32 ])) );
      ( forall_ibinop "op" @@ fun op ->
        judgement []
          (sequent (ibinop op) ([ numtype i32; numtype i32 ], [ numtype i32 ]))
      );
      ( forall_irelop "op" @@ fun op ->
        judgement []
          (sequent (irelop op) ([ numtype i32; numtype i32 ], [ numtype i32 ]))
      );
      judgement [] (sequent nop ([], []));
      ( forall_instr "e1" @@ fun e1 ->
        forall_instr "e2" @@ fun e2 ->
        forall_valty "t1" @@ fun t1 ->
        forall_valty "t2" @@ fun t2 ->
        judgement
          [ sequent e1 ([ t1 ], [ t2 ]); sequent e2 ([ t1 ], [ t2 ]) ]
          (sequent (if_ [ e1 ] [ e2 ]) ([ t1 ], [ t2 ])) );
    ]
