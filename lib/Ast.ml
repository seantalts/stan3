(* Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp, compare *)

(* Some unsized/return types -- we define these separately first, as we want to
   be able to use sets of them in the following, so we need to derive a
   comparison operator; we manually define comparison and sexp operations
   on these sets as Jane Street has not automated this  *)
type argblock = Data | Any

and returntype = Void | ReturnType of unsizedtype

and unsizedtype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of unsizedtype
  | Fun of (argblock * unsizedtype) list * returntype
[@@deriving sexp, compare]

module Set_Of_UnsizedType = Stdlib.Set.Make (struct
  let compare = compare_unsizedtype

  type t = unsizedtype
end)

type set_of_unsizedtype = Set_Of_UnsizedType.t

type list_of_unsizedtype = unsizedtype list [@@deriving sexp, compare]

let sexp_of_set_of_unsizedtype s =
  sexp_of_list_of_unsizedtype (Set_Of_UnsizedType.elements s)

let set_of_unsizedtype_of_sexp s =
  Set_Of_UnsizedType.of_list (list_of_unsizedtype_of_sexp s)

let compare_set_of_unsizedtype s1 s2 =
  compare_list_of_unsizedtype
    (Set_Of_UnsizedType.elements s1)
    (Set_Of_UnsizedType.elements s2)

module Set_Of_ReturnType = Stdlib.Set.Make (struct
  let compare = compare_returntype

  type t = returntype
end)

type set_of_returntype = Set_Of_ReturnType.t

type list_of_returntype = returntype list [@@deriving sexp, compare]

let sexp_of_set_of_returntype s =
  sexp_of_list_of_returntype (Set_Of_ReturnType.elements s)

let set_of_returntype_of_sexp s =
  Set_Of_ReturnType.of_list (list_of_returntype_of_sexp s)

let compare_set_of_returntype s1 s2 =
  compare_list_of_returntype
    (Set_Of_ReturnType.elements s1)
    (Set_Of_ReturnType.elements s2)

(* Programs. *)
type program =
  { functionblock: functionblock
  ; datablock: datablock
  ; transformeddatablock: transformeddatablock
  ; parametersblock: parametersblock
  ; transformedparametersblock: transformedparametersblock
  ; modelblock: modelblock
  ; generatedquantitiesblock: generatedquantitiesblock }

(* Blocks. *)
and functionblock = fundef list option

and datablock = topvardecl list option

and transformeddatablock = topvardecl_or_statement list option

and parametersblock = topvardecl list option

and transformedparametersblock = topvardecl_or_statement list option

and modelblock = vardecl_or_statement list option

and generatedquantitiesblock = topvardecl_or_statement list option

(* Declarations and definitions *)
(* TODO: Decorate fundef with optional marker like RNG, LP, PLAIN *)
and fundef =
  { returntype: returntype
  ; name: identifier
  ; arguments: argdecl list
  ; body: statement }

and identifier = string

and argdecl = argblock * unsizedtype * identifier

and topvardecl = sizedtype * transformation * identifier

and compound_topvardecl_assign =
  { sizedtype: sizedtype
  ; transformation: transformation
  ; identifier: identifier
  ; value: expression }

and vardecl = sizedtype * identifier

and compound_vardecl_assign =
  {sizedtype: sizedtype; identifier: identifier; value: expression}

and topvardecl_or_statement =
  | TVDecl of topvardecl
  | TStmt of statement
  | TVDeclAss of compound_topvardecl_assign

and vardecl_or_statement =
  | VDecl of vardecl
  | Stmt of statement
  | VDeclAss of compound_vardecl_assign

and sizedtype =
  | SInt
  | SReal
  | SVector of expression
  | SRowVector of expression
  | SMatrix of expression * expression
  | SArray of sizedtype * expression

and transformation =
  | Identity
  | Lower of expression
  | Upper of expression
  | LowerUpper of expression * expression
  | LocationScale of expression * expression
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

(* Expressions. *)
and expression = untypedexpression * set_of_unsizedtype

and untypedexpression =
  | Conditional of expression * expression * expression
  | InfixOp of expression * infixop * expression
  | PrefixOp of prefixop * expression
  | PostfixOp of expression * postfixop
  | Variable of identifier
  | IntNumeral of string
  | RealNumeral of string
  | FunApp of identifier * expression list
  | CondFunApp of identifier * expression list
  | GetLP
  (* deprecated *)
  | GetTarget
  | ArrayExpr of expression list
  | RowVectorExpr of expression list
  | Paren of expression
  | Indexed of expression * index list

and infixop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Exp
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq

and prefixop = Not | UMinus | UPlus

and postfixop = Transpose

and printable = PString of string | PExpr of expression

(* Statements. *)
and statement = untypedstatement * set_of_returntype

and untypedstatement =
  | Assignment of lhs * assignmentoperator * expression
  | NRFunApp of identifier * expression list
  | TargetPE of expression
  | IncrementLogProb of expression
  (* deprecated *)
  | Tilde of
      { arg: expression
      ; distribution: identifier
      ; args: expression list
      ; truncation: truncation }
  | Break
  | Continue
  | Return of expression
  | Print of printable list
  | Reject of printable list
  | Skip
  | IfElse of expression * statement * statement
  | While of expression * statement
  | For of
      { loop_variable: identifier
      ; lower_bound: expression
      ; upper_bound: expression
      ; loop_body: statement }
  | ForEach of identifier * expression * statement
  | Block of vardecl_or_statement list

and truncation =
  | NoTruncate
  | TruncateUpFrom of expression
  | TruncateDownFrom of expression
  | TruncateBetween of expression * expression

and lhs = identifier * index list

and index =
  | All
  | Single of expression
  | Upfrom of expression
  | Downfrom of expression
  | Between of expression * expression

and assignmentoperator =
  | Assign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | EltTimesAssign
  | EltDivideAssign
  | ArrowAssign
  (* deprecated *)
[@@deriving sexp, compare]
