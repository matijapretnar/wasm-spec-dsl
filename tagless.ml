(* NUMTYPE *)

module type NUMTYPE = sig
  type numtype

  val i32 : numtype
end

module LaTeXNumType : NUMTYPE with type numtype = string = struct
  type numtype = string

  let i32 = "i32"
end

(* VALTYPE *)

module type VALTYPE = sig
  include NUMTYPE

  type valtype

  val numtype : numtype -> valtype
end

module LaTeXValType : VALTYPE with type valtype = string = struct
  include LaTeXNumType

  type valtype = numtype

  let numtype nt = nt
end

(* NUMINSTR *)

module type NUMINSTR = sig
  type sx

  val u : sx

  type ibinop

  val add : ibinop
  val sub : ibinop
  val mul : ibinop

  type irelop

  val eq : irelop
  val ne : irelop
  val lt : sx -> irelop

  type i32
  type instr

  val iconst : i32 -> instr
  val ibinop : ibinop -> instr
  val irelop : irelop -> instr
end

module LaTeXNumInstr :
  NUMINSTR
    with type instr = string
     and type i32 = string
     and type ibinop = string
     and type irelop = string = struct
  type sx = string

  let u = "u"

  type ibinop = string

  let add = "add"
  let sub = "sub"
  let mul = "mul"

  type irelop = string

  let eq = "eq"
  let ne = "ne"
  let lt sx = Printf.sprintf "%slt" sx

  type i32 = string
  type instr = string

  let iconst n = Printf.sprintf "i32.const %s" n
  let ibinop op = Printf.sprintf "i32.%s" op
  let irelop op = Printf.sprintf "i32.%s" op
end

(* CONTROLINSTR *)

module type CONTROL_INSTR = sig
  type instr

  val nop : instr
  val if_ : instr list -> instr list -> instr
end

module LaTeXControlInstr : CONTROL_INSTR with type instr = string = struct
  type instr = string

  let latex_instrs = String.concat " "
  let nop = "nop"

  let if_ es1 es2 =
    Printf.sprintf "if %s else %s end" (latex_instrs es1) (latex_instrs es2)
end

(* INSTR *)

module type INSTR = sig
  type instr

  include NUMINSTR with type instr := instr
  include CONTROL_INSTR with type instr := instr
end

module LaTeXInstr :
  INSTR
    with type instr = string
     and type i32 = string
     and type ibinop = string
     and type irelop = string = struct
  include LaTeXNumInstr
  include LaTeXControlInstr
end

(* JUDGEMENTS *)

module type JUDGEMENTS = sig
  include VALTYPE
  include INSTR

  type sequent
  type judgement

  val sequent : instr -> valtype list * valtype list -> sequent
  val judgement : sequent list -> sequent -> judgement
  val forall_int : string -> (i32 -> judgement) -> judgement
  val forall_ibinop : string -> (ibinop -> judgement) -> judgement
  val forall_irelop : string -> (irelop -> judgement) -> judgement
  val forall_instr : string -> (instr -> judgement) -> judgement
  val forall_valty : string -> (valtype -> judgement) -> judgement
end

module LaTeXJudgements : JUDGEMENTS with type judgement = string = struct
  include LaTeXValType
  include LaTeXInstr

  type sequent = string
  type judgement = string

  let sequent instr (ty1, ty2) =
    Format.sprintf "\\vdash %s : %s \\to %s" instr (String.concat " " ty1)
      (String.concat " " ty2)

  let judgement hyp conc =
    Format.sprintf "\\frac{%s}{%s}" (String.concat " \\qquad " hyp) conc

  let forall_int n judg = judg n
  let forall_valty n judg = judg n
  let forall_ibinop n judg = judg n
  let forall_irelop n judg = judg n
  let forall_instr n judg = judg n
end

module TextJudgements : JUDGEMENTS with type judgement = string = struct
  include LaTeXValType
  include LaTeXInstr

  type sequent = string
  type judgement = string

  let sequent instr (ty1, ty2) =
    Format.sprintf "%s takes %s and returns %s" instr (String.concat " " ty1)
      (String.concat " " ty2)

  let judgement hyps conc =
    let hyp_list =
      hyps |> List.mapi (fun i hyp -> Format.sprintf "  %d. $%s$" (i + 1) hyp)
    in
    Format.sprintf "If:\n%s\nthen:\n  $%s$."
      (String.concat ", \n" hyp_list)
      conc

  let forall_int n judg = Format.sprintf "For all integers %s:\n%s" n (judg n)

  let forall_ibinop op judg =
    Format.sprintf "For all binary operations %s:\n%s" op (judg op)

  let forall_irelop op judg =
    Format.sprintf "For all binary relations %s:\n%s" op (judg op)

  let forall_instr op judg =
    Format.sprintf "For all instructions %s:\n%s" op (judg op)

  let forall_valty ty judg = Format.sprintf "For all types %s:\n%s" ty (judg ty)
end

(* RULES *)

module type RULES = sig
  include JUDGEMENTS

  type rules

  val rules : judgement list -> rules
end

module LaTeXRules : RULES with type rules = string = struct
  include LaTeXJudgements

  type rules = string

  let rules rs = Format.sprintf "$$\n%s\n$$" (String.concat "\n\\qquad\n" rs)
end

module TextRules : RULES with type rules = string = struct
  include TextJudgements

  type rules = string

  let rules rs = Format.sprintf "%s" (rs |> String.concat "\n\n")
end

(* EXAMPLE *)

module Rules (J : RULES) = struct
  let rules =
    J.(
      rules
        [
          ( forall_int "m" @@ fun n ->
            judgement [] (sequent (iconst n) ([], [ numtype i32 ])) );
          ( forall_ibinop "op" @@ fun op ->
            judgement []
              (sequent (ibinop op)
                 ([ numtype i32; numtype i32 ], [ numtype i32 ])) );
          ( forall_irelop "op" @@ fun op ->
            judgement []
              (sequent (irelop op)
                 ([ numtype i32; numtype i32 ], [ numtype i32 ])) );
          judgement [] (sequent nop ([], []));
          ( forall_instr "e1" @@ fun e1 ->
            forall_instr "e2" @@ fun e2 ->
            forall_valty "t1" @@ fun t1 ->
            forall_valty "t2" @@ fun t2 ->
            judgement
              [ sequent e1 ([ t1 ], [ t2 ]); sequent e2 ([ t1 ], [ t2 ]) ]
              (sequent (if_ [ e1 ] [ e2 ]) ([ t1 ], [ t2 ])) );
        ])
end

let a =
  let module LR = Rules (LaTeXRules) in
  let module TR = Rules (TextRules) in
  print_endline LR.rules;
  print_endline TR.rules
