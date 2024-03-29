﻿// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.
module lispgen

open FSharp.Compiler.SourceCodeServices

let tab = "   "
let paren s = "(" + s + ")"
let parens s = "(" + String.concat " " s + ")"

let rec strType (t:FSharpType) = 
  if t.IsAbbreviation then
    strType t.AbbreviatedType
  else if t.IsFunctionType then
     strList "->" strType t.GenericArguments
  else if t.IsTupleType then
    "(Tuple " + (strList " " strParendType t.GenericArguments) + ")"
  else if t.HasTypeDefinition then
    let tdef = t.TypeDefinition
    let name = t.TypeDefinition.DisplayName
    if tdef.IsArrayType then
      "Vec " + strList "!!" strParendType t.GenericArguments
    else
      // Nasty string match -- is there a better way?
      let core = "Microsoft.FSharp.Core"
      match (tdef.AccessPath, name) with
      | core,"float" -> "Float"
      | core,"int" -> "Integer"
      | "System","Double" -> "Float"
      | "System","Int32" -> "Integer"
      | "DV", "Vector" -> "Vec " + strList ", " strParendType t.GenericArguments
      | a,n -> "UnkType{" + a + "." + n + "}"
  else
    match t.ToString() with
    | _ ->
        if t.HasTypeDefinition then "**UNKNOWN TYPE " + t.TypeDefinition.ToString()
        else "**UNKNOWN TYPE UNKNOWN"

and strRangeType (t:FSharpType) = 
  if t.IsFunctionType then
     strRangeType <| Seq.last t.GenericArguments
  else
     strType t

and strList sep mapper ts = String.concat sep <| Seq.map mapper ts

and strParendType = strType >> paren

let rec mangleType (t:FSharpType) = 
  if t.IsAbbreviation then
    mangleType t.AbbreviatedType
  else if t.IsFunctionType then
    "L[" + mangleList "_" mangleType t.GenericArguments + "]"
  else if t.IsTupleType then
    "T[" + (mangleList "_" mangleParendType t.GenericArguments) + "]"
  else if t.HasTypeDefinition then
    let tdef = t.TypeDefinition
    let name = t.TypeDefinition.DisplayName
    if tdef.IsArrayType then
      "V[" + mangleList "_" mangleParendType t.GenericArguments + "]"
    else
      // Nasty string match -- is there a better way?
      let core = "Microsoft.FSharp.Core"
      match (tdef.AccessPath, name) with
      | core,"float" -> "Float"
      | core,"int" -> "Integer"
      | "System","Double" -> "Float"
      | "System","Int32" -> "Integer"
      | "DV", "Vector" -> "V[" + mangleList "_" mangleParendType t.GenericArguments + "]"
      | a,n -> "**UK[" + a + "." + n + "]"
  else
    match t.ToString() with
    | _ ->
        if t.HasTypeDefinition then "**UNKNOWN TYPE " + t.TypeDefinition.ToString()
        else "**UNKNOWN TYPE UNKNOWN"

and mangleRangeType (t:FSharpType) = 
  if t.IsFunctionType then
     mangleRangeType <| Seq.last t.GenericArguments
  else
     mangleType t

and mangleList sep mapper ts = String.concat sep <| Seq.map mapper ts

and mangleParendType x = "[" + mangleType x + "]"



let strVal (v:FSharpMemberOrFunctionOrValue) =
  match v.CompiledName with
  | "Cos"                   -> "cos"
  | "Sin"                   -> "sin"
  | "Sqrt"                  -> "sqrt"
  | "Log"                   -> "log"
  | "Exp"                   -> "exp"
  | "ToDouble"              -> "to_float"
  | "ToInt"                 -> "to_int"
  | "op_DotMinus"           -> ".sub"
  | "op_DotPlus"            -> ".add"
  | "op_Addition"           -> "add"
  | "op_Multiply"           -> "mul"
  | "op_Subtraction"        -> "sub"
  | "op_Division"           -> "div"
  | "op_Modulus"            -> "mod"
  | "op_UnaryNegation"      -> "neg"
  | "op_Inequality"         -> "ne"
  | "op_Equality"           -> "eq"
  | "op_LessThan"           -> "lt"
  // These monomorphised versions are not correct in general.
  // F2K needs to take into account that there may be Integers.
  | "op_GreaterThan"        -> "gt"
  | "op_LessThanOrEqual"    -> "lt"
  | "op_GreaterThanOrEqual" -> "gt"
  | "op_Exponentiation"     -> "pow"
  | "get_Length"            -> "size" 
  | _ -> v.CompiledName

let ndb x = ""
let db x = "#|" + x.ToString() + "|#"
let sprintfloat v = db "F" + let s = v.ToString() in if s.Contains(".") then s else s + ".0"
let sprint v = v.ToString()

let rec toLispR indent (e:FSharpExpr) :string = 
    let iindent = indent + tab
    let nl = "\n" + iindent
    match e with 
    | BasicPatterns.AddressOf(lvalueExpr) -> 
        db "AO" + toLispR indent lvalueExpr
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        toLispR indent lvalueExpr + db "AddrSet" + toLispR indent rvalueExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        paren <| (toLispR indent funcExpr + db "App" + toLispRs indent argExprs)
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        let objName = match objExprOpt with
                      | Some e ->  paren (toLispR indent e)
                      | None -> ""
        match (memberOrFunc.CompiledName) with
        | "get_Item" -> paren ("index " + toLispR indent argExprs.[0] + " " + objName)
        | "GetArray" -> paren ("index#|GA|# " + toLispR indent argExprs.[1] + " " + toLispR indent argExprs.[0])
        // TODO? | "build2" -> paren ("build " + toLispR indent argExprs.[0] + " " + objName)
        | _ ->  let typeannot' tag (targs : 'a list) = 
                    let n = targs.Length
                    if n < 2 then 
                      ""
                    else 
                      "@" + (String.concat "," (targs.[.. n-2] |> List.map mangleType))
                let typeannot = typeannot' "" typeArgs1 + typeannot' "" typeArgs2
                paren (strVal memberOrFunc + typeannot + db memberOrFunc + " " + objName + " " + toLispRs indent argExprs)

(*    | BasicPatterns.Coerce(targetType, inpExpr) -> 
        toLispR indent inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> 
        toLispR indent startExpr; toLispR indent limitExpr; toLispR indent consumeExpr
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        toLispRs indent argExprs
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg indent objExprOpt
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg indent objExprOpt
        *)
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        paren ("if " + toLispR indent guardExpr + nl +
                       toLispR indent thenExpr + nl +
                       toLispR indent elseExpr)
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> 
        "(lam (" + lambdaVar.DisplayName + " : " + strType lambdaVar.FullType + ")\n" + iindent + (toLispR iindent bodyExpr) + ")"
    | BasicPatterns.Let((x, e1), e2) -> 
        sprintf "(let (%s %s)\n%s%s)" (x.DisplayName) (toLispR iindent e1) indent (toLispR iindent e2) 
    // | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
    //     List.iter (snd >> toLispR indent) recursiveBindings; toLispR indent bodyExpr
    // | BasicPatterns.NewArray(arrayType, argExprs) -> 
    //     toLispRs indent argExprs
    // | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
    //     toLispR indent delegateBodyExpr
    // | BasicPatterns.NewObject(objType, typeArgs, argExprs) -> 
    //     toLispRs indent argExprs
    // | BasicPatterns.NewRecord(recordType, argExprs) ->  
    //     toLispRs indent argExprs
    // | BasicPatterns.NewTuple(tupleType, argExprs) -> 
    //     toLispRs indent argExprs
    // | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
    //     toLispRs indent argExprs
    // | BasicPatterns.Quote(quotedExpr) -> 
    //     toLispR indent quotedExpr
    // | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
    //     visitObjArg indent objExprOpt
    // | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
    //     visitObjArg indent objExprOpt; toLispR indent argExpr
    // | BasicPatterns.Sequential(firstExpr, secondExpr) -> 
    //     toLispR indent firstExpr; toLispR indent secondExpr
    // | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> 
    //     toLispR indent bodyExpr; toLispR indent finalizeExpr
    // | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> 
    //     toLispR indent bodyExpr; toLispR indent catchExpr
    // | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
    //     toLispR indent tupleExpr
    // | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
    //     toLispR indent decisionExpr; List.iter (snd >> toLispR indent) decisionTargets
    // | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
    //     toLispRs indent decisionTargetExprs
    // | BasicPatterns.TypeLambda(genericParam, bodyExpr) -> 
    //     toLispR indent bodyExpr
    // | BasicPatterns.TypeTest(ty, inpExpr) -> 
    //     toLispR indent inpExpr
    // | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
    //     toLispR indent unionExpr; toLispR indent valueExpr
    // | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
    //     toLispR indent unionExpr
    // | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
    //     toLispR indent unionExpr
    // | BasicPatterns.UnionCaseTag(unionExpr, unionType) -> 
    //     toLispR indent unionExpr
    // | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
    //     toLispR indent baseCallExpr
    //     List.iter (visitObjMember indent) overrides
    //     List.iter (snd >> List.iter (visitObjMember indent)) interfaceImplementations
    // | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) -> 
    //     toLispRs indent argExprs
    // | BasicPatterns.ValueSet(valToSet, valueExpr) -> 
    //     toLispR indent valueExpr
    // | BasicPatterns.WhileLoop(guardExpr, bodyExpr) -> 
    //     toLispR indent guardExpr; toLispR indent bodyExpr
    // | BasicPatterns.BaseValue baseType -> ()
    // | BasicPatterns.DefaultValue defaultType -> ()
    // | BasicPatterns.ThisValue thisType -> ()
    | BasicPatterns.Const(null, float) -> sprintf "%A" null //How should be handling nulls??
    | BasicPatterns.Const(v, t) -> if v :? float then sprintfloat v else sprint v
    | BasicPatterns.Const(v, _) -> db "C" + sprintf "%A" v
    | BasicPatterns.Value(v) -> ndb "V" + sprintf "%s" v.DisplayName
    | _ -> sprintf "(**UNRECOGNIZED** %+A)" e

and toLispRs indent exprs = 
    String.concat " " (Seq.map (toLispR indent) exprs)

(*
and visitObjMember indent memb = 
    toLispR indent memb.Body
*)

let toLisp (e:FSharpExpr) = 
    toLispR "" e

let toLispDecls (checkedFile : FSharpImplementationFileContents) = 
    let rec toLispDecls prefix d  = 
        match d with 
        | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 
            seq {
              for subDecl in subDecls do 
                yield! toLispDecls (prefix+" ") subDecl
            }
        | FSharpImplementationFileDeclaration.InitAction(e) -> 
            printfn "%sA top-level expression was declared" prefix 
            Seq.empty

        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
            seq {
                let par (v : FSharpMemberOrFunctionOrValue) =
                     paren <| v.CompiledName + " : " + strType v.FullType
                let pars = List.map (List.map par) vs
                let spars = List.map (String.concat " ") pars
                yield sprintf "%s(def %s %s (%s)\n" prefix  
                        v.CompiledName 
                        (paren <| strRangeType v.FullType)
                        (String.concat " " <| spars)                         
                yield prefix + tab + toLispR (prefix + tab) e
                yield ")\n\n"
            }

    seq {
        for d in checkedFile.Declarations do 
            yield! toLispDecls "" d
    }




(*
open Microsoft.FSharp.Quotations
  
let (|ArraySlice|_|) (e: Expr): (Expr * Expr * Expr) option = 
  match e with 
  | Patterns.Call (None, op, argList) -> 
    match (op.Name, op.DeclaringType.Name) with
    | ("GetArraySlice", "OperatorIntrinsics") -> 
      let args = 
        List.map (fun x -> 
          match x with 
          | Patterns.NewUnionCase(_, [v]) -> v
          | _ -> x) argList
      Some(args.[0], args.[1], args.[2])
    | _ -> None
  | _ -> None

// Generates the corresponding lisp expression 
let rec lispcodegenExpr (e:Expr) (tabsNumber: int): string =
  let rec rcr (e1: Expr): string = lispcodegenExpr e1 tabsNumber
  let rec rcrInd (e1: Expr): string = lispcodegenExpr e1 (tabsNumber + 1)
  let rec accumulateLambdaArgs (e: Expr) (s: string): string * Expr = 
      match e with
      | Patterns.Lambda(i, body) -> accumulateLambdaArgs body (s + " " + i.Name)
      | _ -> (s, e)
  
  let printTabs times = String.replicate times TABS
  let tabs: string = printTabs tabsNumber
  let tabsInd: string = printTabs (tabsNumber + 1)
  let tabsInd2: string = printTabs (tabsNumber + 2)
  match e with
  | Patterns.Lambda(_, _) -> let (args, body) = accumulateLambdaArgs e "" in sprintf "(lam %s \n%s%s)" args tabsInd (rcrInd body)
  | Patterns.Let(x, e1, e2) -> sprintf "(let (%s \n%s%s)\n%s%s)" (x.Name) tabsInd (rcr e1) tabs (rcrInd e2) 
  | ArraySlice(e1, e2, e3) ->  sprintf "(linalg_vectorSlice %s %s %s)" (rcr e1) (rcr e2) (rcr e3)
  | Patterns.Call (None, op, elist) -> 
    match op.Name with
      | FsOperatorName opname -> 
        if((List.length elist) = 2) then 
          sprintf "(%s %s %s)" opname (rcr elist.[0]) (rcr elist.[1]) 
        elif ((List.length elist) = 1) then
          sprintf "(%s %s)" opname (rcr elist.[0])
        else 
          failwithf "OperatorName: %s %s" opname (elist |> List.map (fun e -> sprintf "(%s)" (rcr e)) |> String.concat " ")
      | "GetArray" -> sprintf "(index %s %s)" (rcr elist.[0]) (rcr elist.[1])
      | _ -> 
          let (md, mt) = (op.DeclaringType.Name, op.Name)
          ((sprintf "(%s_%s" md mt, elist) ||> List.fold (fun acc cur -> sprintf "%s %s" acc (rcr cur))) + ")"
  | Patterns.Var(x) -> sprintf "%s" x.Name
  | Patterns.NewArray(tp, elems) -> 
    sprintf "(array \n%s%s)" tabsInd2 (String.concat (sprintf "\n%s" tabsInd2) (List.map rcrInd elems))
  | Patterns.Value(v, tp) when tp = typeof<Unit> -> "()"
  | Patterns.Value(v, tp) when tp = typeof<int> -> sprintf "%d" (unbox<int> v)
  | Patterns.Value(v, tp) when tp = typeof<double> -> let dv = unbox<double>(v) in let s = dv.ToString() in if s.Contains(".") then s else s + ".0"
  | Patterns.Value(v, tp) -> v.ToString()
  | Patterns.Sequential(e1, e2) -> sprintf "%s;\n%s%s" (rcr e1) tabs (rcr e2)
  | Patterns.NewUnionCase (uci, args) when uci.Name = "Card" -> 
     sprintf "(%s %s)" uci.Name (String.concat ", " (List.map rcr args))
  | Patterns.PropertyGet (Some(var), pi, args) -> 
     sprintf "%s.%s" (rcr var) pi.Name
  | Patterns.Application(e1, e2) ->
     sprintf "(%s %s)" (rcr e1) (rcr e2)
  | Patterns.IfThenElse(cond, e1, e2) -> sprintf "(if %s \n%s%s\n%s \n%s%s)" (rcr cond) tabsInd (rcrInd e1) tabs tabsInd (rcrInd e2)
  | Patterns.NewTuple(es) -> sprintf "(pair %s)" (String.concat " " (List.map rcr es)) 
  | ExprShape.ShapeCombination(op, args) -> 
    failwithf "COMB{%A}(%s)" (op) (String.concat ", " (List.map rcr args))
  //| LibraryCall(name, argList) -> sprintf "%s(%s)" name (String.concat ", " (List.map fscodegenExpr argList))
  | _ -> failwithf "ERROR[%A]" e

let generate (e:Expr): string =
  lispcodegenExpr e 0
*)
