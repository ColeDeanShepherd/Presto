module TypeChecker

open CompilerCore
open ASTBuilder
open Lexer

open type PrestoProgram

type TypeCheckerState = {
    ScopeIdStack: System.Guid list
    ScopesById: Map<System.Guid, Scope>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    Errors: List<compile_error>
}

let getTypeScopeId (state: TypeCheckerState) (prestoType: PrestoType): (Option<System.Guid> * TypeCheckerState) =
    match prestoType with
    | Text scopeId -> (Some scopeId, state)
    | RecordType rt -> (Some rt.ScopeId, state)
    | UnionType ut -> (Some ut.ScopeId, state)
    | _ ->
        let error = compile_error(
            description = $"Couldn't access members of type: {prestoType}",
            position = text_position(file_name = "", line_index = 0u, column_index = 0u)
        )

        (None, { state with Errors = state.Errors @ [error] })

let rec resolveSymbolInternal (state: TypeCheckerState) (scope: Scope) (name: string) (nameTokenPosition: text_position): (Option<Symbol> * TypeCheckerState) =
    if scope.SymbolsByName.ContainsKey name then
        (Some scope.SymbolsByName.[name], state)
    else if scope.ParentId.IsSome then
        let parentScope = state.ScopesById[scope.ParentId.Value]

        resolveSymbolInternal state parentScope name nameTokenPosition
    else
        let error = compile_error(
            description = $"Unknown name: {name}",
            position = nameTokenPosition
        )
        let errors = List.append state.Errors [error]
        (None, { state with Errors = errors })

let resolveSymbol (state: TypeCheckerState) (nameToken: token): (Option<Symbol> * TypeCheckerState) =
    let currentScope = state.ScopesById[List.last state.ScopeIdStack]

    resolveSymbolInternal state currentScope nameToken._text nameToken.position

let pushScope (state: TypeCheckerState) (scopeId: System.Guid): TypeCheckerState =
    let parentScopeId = List.last state.ScopeIdStack
    let parentScope = state.ScopesById[parentScopeId]

    //if not (List.contains scopeId parentScope.ChildIds) then
    //    failwith $"Incorrectly pushed scope {scopeId}"
    //else
    { state with ScopeIdStack = state.ScopeIdStack @ [scopeId] }

let popScope (state: TypeCheckerState): TypeCheckerState =
    let scope = state.ScopesById[List.last state.ScopeIdStack]

    { state with ScopeIdStack = (List.take (state.ScopeIdStack.Length - 1) state.ScopeIdStack) }
    
let rec checkMany
    (state: TypeCheckerState)
    (checkFn: (TypeCheckerState -> 'a -> TypeCheckerState * Option<'b>))
    (nodes: List<'a>)
    (accumulator: List<Option<'b>>):
    TypeCheckerState * List<Option<'b>> =
        if nodes.IsEmpty then
            (state, accumulator)
        else
            let (state, optionExpressionType) = checkFn state nodes.Head
            let accumulator = accumulator @ [optionExpressionType]
            checkMany state checkFn nodes.Tail accumulator

and checkUnion (state: TypeCheckerState) (union: Union): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state union.ScopeId

    let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter union.TypeParameters []
    let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)
    
    if successfullyCheckedTypeParameterTypes then
        let typeParameterIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        let (state, optionUnionTypeConstructors) = checkMany state checkUnionCase union.Cases []
        let successfullyCheckedUnionTypeConstructors = not (List.exists<Option<UnionTypeConstructor>> (fun x -> x.IsNone) optionUnionTypeConstructors)
    
        if successfullyCheckedUnionTypeConstructors then
            let constructors =
                List.map<Option<UnionTypeConstructor>, UnionTypeConstructor>
                    (fun x -> x.Value)
                    optionUnionTypeConstructors

            (popScope state, Some (UnionType { ScopeId = union.ScopeId; TypeParamNameAndIds = typeParameterIdAndNames; Constructors = constructors }))
        else
            (popScope state, None)
    else
        (popScope state, None)

and checkUnionCase (state: TypeCheckerState) (unionCase: UnionCase): TypeCheckerState * Option<UnionTypeConstructor> =
    let (state, optionParameterTypes) = checkMany state checkParameter unionCase.Parameters []
    let successfullyCheckedParameterTypes = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterNames = List.map<Parameter, string> (fun x -> x.NameToken._text) unionCase.Parameters
        let parameterTypes =
            List.map<Option<PrestoType>, PrestoType>
                (fun x -> x.Value)
                optionParameterTypes

        (state, Some { Name = unionCase.NameToken._text; Parameters = List.zip parameterNames parameterTypes})
    else
        (state, None)

and checkRecordField (state: TypeCheckerState) (recordField: RecordField): TypeCheckerState * Option<PrestoType> =
    checkExpression state recordField.TypeExpression

and checkRecord (state: TypeCheckerState) (record: Record): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state record.ScopeId

    let (state, optionFieldTypes) = checkMany state checkRecordField record.Fields []
    let fieldTypes =
        List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionFieldTypes
        |> List.map (fun x -> x.Value)

    let succeededCheckingFields = fieldTypes.Length = optionFieldTypes.Length

    if succeededCheckingFields then
        let prestoType = RecordType { ScopeId = record.ScopeId; TypeParamNameAndIds = []; FieldTypes = fieldTypes }

        (popScope state, Some prestoType)
    else
        (popScope state, None)

and checkTypeParameter (state: TypeCheckerState) (typeParameter: TypeParameter): TypeCheckerState * Option<PrestoType> =
    (state, Some (TypeParameterType (typeParameter.Id, typeParameter.NameToken._text)))
    
and checkParameter (state: TypeCheckerState) (parameter: Parameter): TypeCheckerState * Option<PrestoType> =
    checkExpression state parameter.TypeExpression

// TODO: make sure we don't check types of anything twice via symbol references

and checkFunction (state: TypeCheckerState) (expression: Expression) (fn: Function): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state fn.ScopeId

    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let successfullyCheckedParameterTypes = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters

        let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter fn.TypeParameters []
        let successfullyCheckedTypeParameterTypes = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionTypeParameterTypes)
        
        let typeParameterIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        if successfullyCheckedTypeParameterTypes then
            let (state, optionSpecifiedReturnType) =
                match fn.OptionReturnTypeExpression with
                | Some returnTypeExpression -> checkExpression state returnTypeExpression
                | None -> (state, None)

            let state =
                match optionSpecifiedReturnType with
                | Some specifiedReturnType ->
                    let prestoType = FunctionType {
                        ScopeId = fn.ScopeId
                        TypeParamNameAndIds = typeParameterIdAndNames
                        ParamTypes = parameterTypes
                        ReturnType = specifiedReturnType
                    }

                    { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) }
                | None -> state

            // TODO: fix recursion when return type isn't specified
            let (state, optionInferredReturnType) = checkExpression state fn.Value

            match optionInferredReturnType with
            | Some inferredReturnType ->

                if optionSpecifiedReturnType.IsSome then
                    let state = popScope state

                    if inferredReturnType = optionSpecifiedReturnType.Value then
                        (state, Some state.TypesByExpressionId[expression.Id])
                    else
                        let error = compile_error(
                            description = $"Function's specified return type ({optionSpecifiedReturnType.Value}) doesn't match its inferred return type ({inferredReturnType})",
                            position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                        )
                        let state = { state with Errors = state.Errors @ [error] }

                        (state, None)
                else
                    let inferredPrestoType = FunctionType {
                        ScopeId = fn.ScopeId
                        TypeParamNameAndIds = typeParameterIdAndNames
                        ParamTypes = parameterTypes
                        ReturnType = inferredReturnType
                    }
                    (popScope state, Some inferredPrestoType)
            | None -> (popScope state, None)
        else
            (popScope state, None)
    else
        (popScope state, None)

and checkFunctionType (state: TypeCheckerState) (expression: Expression) (fn: FunctionType): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state fn.ScopeId

    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let successfullyCheckedParameterTypes = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters

        let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter fn.TypeParameters []
        let successfullyCheckedTypeParameterTypes = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionTypeParameterTypes)
        
        let typeParameterIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        if successfullyCheckedTypeParameterTypes then
            let (state, optionReturnType) = checkExpression state fn.ReturnTypeExpression

            let state =
                match optionReturnType with
                | Some specifiedReturnType ->
                    let prestoType = FunctionType {
                        ScopeId = fn.ScopeId
                        TypeParamNameAndIds = typeParameterIdAndNames
                        ParamTypes = parameterTypes
                        ReturnType = specifiedReturnType
                    }

                    { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) }
                | None -> state

            if optionReturnType.IsSome then
                let state = popScope state

                (state, Some state.TypesByExpressionId[expression.Id])
            else
                (popScope state, None)
        else
            (popScope state, None)
    else
        (popScope state, None)

and areTypesEqual (a: PrestoType) (b: PrestoType): bool =
    a = b

and checkIfThenElse (state: TypeCheckerState) (ifThenElse: IfThenElse): TypeCheckerState * Option<PrestoType> =
    let (state, optionIfExpressionType) = checkExpression state ifThenElse.IfExpression

    match optionIfExpressionType with
    | Some ifExpressionType ->
        if ifExpressionType = PrestoType.Boolean then
            let (state, optionThenExpressionType) = checkExpression state ifThenElse.ThenExpression

            match optionThenExpressionType with
            | Some thenExpressionType ->
                let (state, optionElseExpressionType) = checkExpression state ifThenElse.ElseExpression

                match optionElseExpressionType with
                | Some elseExpressionType ->
                    if areTypesEqual thenExpressionType elseExpressionType then
                        (state, Some thenExpressionType)
                    else
                        let error = compile_error(
                            description = $"\"then\" type ({thenExpressionType}) doesn't match \"else\" type ({elseExpressionType})",
                            position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                        )
                        let state = { state with Errors = state.Errors @ [error] }

                        (state, None) // TODO: error
                | None -> (state, None)
            | None -> (state, None)
        else
            let error = compile_error(
                description = $"\"if\" type ({ifExpressionType}) isn't a bool",
                position = text_position(file_name = "", line_index = 0u, column_index = 0u)
            )
            let state = { state with Errors = state.Errors @ [error] }

            (state, None) // TODO: error
    | None -> (state, None)

and inferFunctionCallTypeArgs (paramTypes: List<PrestoType>) (argTypes: List<PrestoType>): Map<System.Guid, PrestoType> =
    let equalities = List.zip paramTypes argTypes
    let typesByTypeParamId: Map<System.Guid, PrestoType> = Map.empty
    typeInferenceHandleEqualities equalities typesByTypeParamId

and typeInferenceHandleEqualities (equalities: List<PrestoType * PrestoType>) (typesByTypeParamId: Map<System.Guid, PrestoType>): Map<System.Guid, PrestoType> =
    if equalities.IsEmpty then
        typesByTypeParamId
    else
        let (type1, type2) = equalities.Head
        let typeArgsByName = typeInferenceHandleEquality type1 type2 typesByTypeParamId
        typeInferenceHandleEqualities equalities.Tail typeArgsByName

and typeInferenceHandleEquality (type1: PrestoType) (type2: PrestoType) (typesByTypeParamId: Map<System.Guid, PrestoType>): Map<System.Guid, PrestoType> =
    if type1 = type2 then
        typesByTypeParamId
    else
        match (type1, type2) with
        | (TypeParameterType (id1, name1), TypeParameterType (id2, name2)) ->
            let typesByTypeParamId = typeInferenceHandleTypeParameterEquality id1 type2 typesByTypeParamId
            typeInferenceHandleTypeParameterEquality id2 type1 typesByTypeParamId
        | (TypeParameterType (id1, name1), _) ->
            typeInferenceHandleTypeParameterEquality id1 type2 typesByTypeParamId
        | (_, TypeParameterType (id2, name2)) ->
            typeInferenceHandleTypeParameterEquality id2 type1 typesByTypeParamId
        | (RecordInstanceType (tct1, typeArgs1), RecordInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (UnionInstanceType (tct1, typeArgs1), UnionInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (TypeClassInstanceType (tct1, typeArgs1), TypeClassInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (TupleType elTypes1, TupleType elTypes2) ->
            let equalities = List.zip elTypes1 elTypes2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | _ -> typesByTypeParamId

and typeInferenceHandleTypeParameterEquality (typeParameterId: System.Guid) (prestoType: PrestoType) (typesByTypeParamId: Map<System.Guid, PrestoType>): Map<System.Guid, PrestoType> =
    if typesByTypeParamId.ContainsKey typeParameterId then
        let prevInferredType = typesByTypeParamId[typeParameterId]

        if prevInferredType = prestoType then
            typesByTypeParamId
        else
            failwith ""
    else
        typesByTypeParamId.Add (typeParameterId, prestoType)

//and typeInferenceHandleEquality (type1: PrestoType) (type2: PrestoType) (typeArgsByName: Map<string, PrestoType>): Map<string, PrestoType> = 
//    let (genericType, concreteType) =
//        if isTypeParameterType type2 then
//            (type2, type1)
//        else
//            (type1, type2)

//    match genericType with
//    | Nothing -> typeArgsByName
//    | Nat -> typeArgsByName
//    | Real -> typeArgsByName
//    | Text _ -> typeArgsByName
//    | Boolean -> typeArgsByName
//    | Character -> typeArgsByName
//    | Type -> typeArgsByName
//    | RecordType (scopeId, typeParamNames, fieldTypes) -> failwith "TODO"
//    | UnionType (scopeId, typeParamNames, constructors) -> failwith "TODO"
//    | TypeParameterType typeParameter ->
//        if not (typeArgsByName.ContainsKey typeParameter) then
//            Map.add typeParameter concreteType typeArgsByName
//        else
//            if typeArgsByName[typeParameter] = concreteType then
//                typeArgsByName
//            else
//                failwith ""
//    | FunctionType (scopeId, typeParamNames, argTypes, returnType) ->
//        match concreteType with
//        | FunctionType (cScopeId, cTypeParamNames, cArgTypes, cReturnType) ->
//            let typeArgsByName = typeInferenceHandleEqualities typeArgsByName argTypes cArgTypes
//            typeInferenceHandleEquality returnType cReturnType typeArgsByName
//        | _ -> failwith ""
//    | UnionInstanceType (scopeId, typeParameters) ->
//        match concreteType with
//        | UnionInstanceType (cScopeId, cTypeParameters) ->
//            if scopeId = cScopeId then
//                typeInferenceHandleEqualities typeArgsByName typeParameters cTypeParameters
//            else
//                failwith ""
//        | _ -> failwith ""
//    | TypeClassInstanceType (scopeId, typeParameters) ->
//        match concreteType with
//        | TypeClassInstanceType (cScopeId, cTypeParameters) ->
//            if scopeId = cScopeId then
//                typeInferenceHandleEqualities typeArgsByName typeParameters cTypeParameters
//            else
//                failwith ""
//        | _ -> failwith ""
//    | RecordInstanceType (scopeId, typeParameters) -> failwith "TODO"
//    | TupleType elementTypes ->
//        match concreteType with
//        | TupleType (cElementTypes) ->
//            typeInferenceHandleEqualities typeArgsByName elementTypes cElementTypes
//        | _ -> failwith ""
//    | TypeClassType (scopeId, typeArguments) -> failwith "TODO"

and _reifyType (prestoType: PrestoType) (typeArgsByParamId: Map<System.Guid, PrestoType>): PrestoType =
    match prestoType with
    | TypeParameterType (typeParameterId, typeParameterName) ->
        if typeArgsByParamId.ContainsKey typeParameterId then
            typeArgsByParamId[typeParameterId]
        else
            prestoType
    | TypeClassInstanceType (scopeId, typeArgumentTypes) ->
        let typeArgumentTypes = List.map (fun t -> _reifyType t typeArgsByParamId) typeArgumentTypes
        TypeClassInstanceType (scopeId, typeArgumentTypes)
    | FunctionType ft ->
        let reifiedParamTypes = List.map (fun t -> _reifyType t typeArgsByParamId) ft.ParamTypes
        let reifiedReturnType = _reifyType ft.ReturnType typeArgsByParamId
        FunctionType {
            ScopeId = ft.ScopeId
            TypeParamNameAndIds = []
            ParamTypes = reifiedParamTypes
            ReturnType = reifiedReturnType
        }
    | _ -> prestoType

and checkFunctionCall (state: TypeCheckerState) (functionCall: FunctionCall): TypeCheckerState * Option<PrestoType> =
    let (state, optionTypeArgumentTypes) = checkMany state checkExpression functionCall.TypeArguments []
    let typeArgumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionTypeArgumentTypes |> List.map (fun x -> x.Value)
    let succeededCheckingTypeArguments = typeArgumentTypes.Length = optionTypeArgumentTypes.Length
    
    if succeededCheckingTypeArguments then
        let (state, optionArgumentTypes) = checkMany state checkExpression functionCall.Arguments []
        let argumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionArgumentTypes |> List.map (fun x -> x.Value)
        let succeededCheckingArguments = argumentTypes.Length = optionArgumentTypes.Length

        // TODO: ensure argument types match function parameter types

        if succeededCheckingArguments then
            let (state, optionFunctionType) = checkExpression state functionCall.FunctionExpression

            match optionFunctionType with
            | Some functionType ->
                match functionType with
                | FunctionType ft ->
                    // TODO: handle inference diff than specified

                    let typeArgsByName = inferFunctionCallTypeArgs ft.ParamTypes argumentTypes
                    let reifiedReturnType = _reifyType ft.ReturnType typeArgsByName

                    (state, Some reifiedReturnType)
                | _ ->
                    let error = compile_error(
                        description = $"Expected function type, got {functionType}",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, Some functionType)
            | None -> (state, None)
        else
            (state, None)
    else
        (state, None)

and checkGenericInstantiation (state: TypeCheckerState) (genericInstantiation: GenericInstantiation): TypeCheckerState * Option<PrestoType> =
    let (state, optionTypeArgumentTypes) = checkMany state checkExpression genericInstantiation.TypeArguments []
    let typeArgumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionTypeArgumentTypes |> List.map (fun x -> x.Value)
    let succeededCheckingTypeArguments = typeArgumentTypes.Length = optionTypeArgumentTypes.Length
    
    if succeededCheckingTypeArguments then
        let (state, optionExpressionType) = checkExpression state genericInstantiation.Expression

        match optionExpressionType with
        | Some expressionType ->
            match expressionType with
            | TypeClassType tct ->
                let typeParameterNameCount = tct.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let expressionType = TypeClassInstanceType (tct, typeArgumentTypes)
                    (state, Some expressionType)
                else
                    let error = compile_error(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, Some expressionType) // Why are we returning expression type?
            | UnionType ut ->
                let typeParameterNameCount = ut.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let expressionType = UnionInstanceType (ut, typeArgumentTypes)
                    (state, Some expressionType)
                else
                    let error = compile_error(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, Some expressionType) // Why are we returning expression type?
            | RecordType rt ->
                let typeParameterNameCount = rt.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let expressionType = RecordInstanceType (rt, typeArgumentTypes)
                    (state, Some expressionType)
                else
                    let error = compile_error(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, Some expressionType) // Why are we returning expression type?
            | FunctionType ft ->
                let typeParameterNameCount = ft.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let typeParameterNames = []
                    let typeParamIds = List.map (fun (id, _) -> id) ft.TypeParamNameAndIds
                    let typeParamIdTypeArgs = List.zip typeParamIds typeArgumentTypes
                    let typeArgsByParamId = Map.ofList typeParamIdTypeArgs
                    let parameterTypes = List.map (fun pt -> (_reifyType pt typeArgsByParamId)) ft.ParamTypes
                    let returnType = _reifyType ft.ReturnType typeArgsByParamId
                    let expressionType = FunctionType {
                        ScopeId = ft.ScopeId
                        TypeParamNameAndIds = typeParameterNames
                        ParamTypes = parameterTypes
                        ReturnType = returnType
                    }

                    (state, Some expressionType)
                else
                    let error = compile_error(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, None)
            | _ ->
                let error = compile_error(
                    description = $"Unexpected expression type {expressionType}",
                    position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                )
                let state = { state with Errors = state.Errors @ [error]}

                (state, Some expressionType) // Why are we returning expression type?
        | None -> (state, None)
    else
        (state, None)

and checkBlockChild (state: TypeCheckerState) (blockChild: BlockChild): TypeCheckerState * Option<PrestoType> =
    match blockChild with
    | BlockChildBinding binding ->
        checkBinding state binding
    | BlockChildExpression expression ->
        checkExpression state expression

and checkBlock (state: TypeCheckerState) (block: Block): TypeCheckerState * Option<PrestoType> =
    // check the types of all non-last bindings & exprs, then check the type of the last expression
    let (state, optionBlockChildTypes) = checkMany state checkBlockChild block.Children []
    
    let blockChildTypes =
        List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionBlockChildTypes
        |> List.map (fun x -> x.Value)

    let succeededCheckingChildren = blockChildTypes.Length = optionBlockChildTypes.Length

    if succeededCheckingChildren then
        if blockChildTypes.IsEmpty then
            (state, Some PrestoType.Nothing)
        else
            (state, Some (List.last blockChildTypes))
    else
        (state, None)
        
and checkMemberAccess (state: TypeCheckerState) (memberAccess: MemberAccess): TypeCheckerState * Option<PrestoType> =
    let (state, optionLeftType) = checkExpression state memberAccess.LeftExpression

    match optionLeftType with
    | Some leftType ->
        let (optionScopeId, state) = getTypeScopeId state leftType

        match optionScopeId with
        | Some scopeId ->
            let scope = state.ScopesById[scopeId]
            let memberName = memberAccess.RightIdentifier._text

            if scope.SymbolsByName.ContainsKey memberName then
                let rightSymbol = scope.SymbolsByName[memberName]

                let (state, optionRightType) = checkSymbol state rightSymbol

                match optionRightType with
                | Some rightType -> (state, Some rightType)
                | None -> (state, None)
            else
                let error = compile_error(
                    description = $"Invalid member: {memberName}",
                    position = memberAccess.RightIdentifier.position
                )

                (
                    { state with Errors = state.Errors @ [error]},
                    None
                )
        | None -> (state, None)
    | None -> (state, None)

and isTypeParameterType (x: PrestoType): bool =
    match x with
    | TypeParameterType _ -> true
    | _ -> false

and canTypesBeTheSame (a: PrestoType) (b: PrestoType): bool =
    (a = b) ||
    (isTypeParameterType a) ||
    (isTypeParameterType b) ||
    match a, b with
    | UnionInstanceType (_, aTypeArgs), UnionInstanceType (_, bTypeArgs) ->
        (aTypeArgs.Length = bTypeArgs.Length) &&
        List.forall (fun (a, b) -> canTypesBeTheSame a b) (List.zip aTypeArgs bTypeArgs)
    //| RecordType (_, _, )
    | _ -> false
        
and checkBinaryOperator (state: TypeCheckerState) (binaryOperator: BinaryOperator): TypeCheckerState * Option<PrestoType> =
    let (state, optionLeftType) = checkExpression state binaryOperator.LeftExpression

    match optionLeftType with
    | Some leftType ->
        let (state, optionRightType) = checkExpression state binaryOperator.RightExpression

        match optionRightType with
        | Some rightType ->
            match binaryOperator.Type with
            | BinaryOperatorType.ReverseFunctionComposition ->
                // TODO: make sure this works with differing type param names
                match leftType with
                | FunctionType lft ->
                    match rightType with
                    | FunctionType rft when rft.ParamTypes.Length = 1 ->
                        if canTypesBeTheSame lft.ReturnType rft.ParamTypes[0] then
                            let resultType = FunctionType {
                                ScopeId = System.Guid.NewGuid()
                                TypeParamNameAndIds = lft.TypeParamNameAndIds
                                ParamTypes = lft.ParamTypes
                                ReturnType = rft.ReturnType
                            }

                            // TODO: ensure right function only has 1 arg?

                            let typeArgsByName = inferFunctionCallTypeArgs rft.ParamTypes [lft.ReturnType]
                            let reifiedResultType = _reifyType resultType typeArgsByName

                            (state, Some reifiedResultType)
                        else
                            let error = compile_error(
                                description = $"Return type of left side ({lft.ReturnType}) of reverse function composition operator is not compatible with type of right side's parameter ({rft.ParamTypes[0]}).",
                                position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                            )

                            (
                                { state with Errors = state.Errors @ [error]},
                                None
                            )
                    | _ ->
                        let error = compile_error(
                            description = $"Right side ({rightType}) of reverse function composition operator should be a function with one parameter.",
                            position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                        )

                        (
                            { state with Errors = state.Errors @ [error]},
                            None
                        )
                | _ ->
                    let error = compile_error(
                        description = $"Left side ({leftType}) of reverse function composition operator should be a function.",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )

                    (
                        { state with Errors = state.Errors @ [error]},
                        None
                    )
                // Ensure left type is function type
                // Ensure right type is function type
                // Ensure that the return type of L works with the only argument of R
            | _ -> 
                if leftType = rightType then
                    let resultType =
                        match binaryOperator.Type with
                        | BinaryOperatorType.Addition -> leftType
                        | BinaryOperatorType.Subtraction -> leftType
                        | BinaryOperatorType.Multiplication -> leftType
                        | BinaryOperatorType.Division -> leftType
                        | BinaryOperatorType.Equality -> PrestoType.Boolean
                        | _ -> failwith "Unknown binary operator type"

                    (state, Some resultType)
                else
                    let error = compile_error(
                        description = $"Left side ({leftType}) and right side ({rightType}) of binary operator ({binaryOperator.Type}) don't match.",
                        position = text_position(file_name = "", line_index = 0u, column_index = 0u)
                    )

                    (
                        { state with Errors = state.Errors @ [error]},
                        None
                    )
        | None -> (state, None)
    | None -> (state, None)

and checkSymbol (state: TypeCheckerState) (symbol: Symbol): TypeCheckerState * Option<PrestoType> =
    match symbol with
    | BindingSymbol binding -> checkExpression state binding.Value
    | TypeParameterSymbol typeParameter -> checkTypeParameter state typeParameter
    | ParameterSymbol parameter -> checkExpression state parameter.TypeExpression
    | RecordFieldSymbol recordField -> checkExpression state recordField.TypeExpression
    | UnionCaseSymbol unionCase -> (state, None)
    | BuiltInSymbol (builtInSymbol, prestoType) -> (state, Some prestoType)

and checkSymbolReference (state: TypeCheckerState) (token: token) (expressionId: System.Guid): TypeCheckerState * Option<PrestoType> =
    let (optionSymbol, state) = resolveSymbol state token

    match optionSymbol with
    | Some symbol ->
        let state = { state with ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId.Add(expressionId, symbol) }
        checkSymbol state symbol
    | None -> (state, None)

and checkNumberLiteral (state: TypeCheckerState) (numberLiteral: NumberLiteral): TypeCheckerState * Option<PrestoType> =
    (
        state,
        Some (
            if numberLiteral.Token._text.Contains('.') then
                PrestoType.Real
            else
                PrestoType.Nat
        )
    )

and checkParenExpr (state: TypeCheckerState) (parenExpr: ParenthesizedExpression2): TypeCheckerState * Option<PrestoType> =
    checkExpression state parenExpr.InnerExpression

and checkTupleExpr (state: TypeCheckerState) (tupleExpression: TupleExpression): TypeCheckerState * Option<PrestoType> =
    let (state, optionValueTypes) = checkMany state checkExpression tupleExpression.Values []
    let valueTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionValueTypes |> List.map (fun x -> x.Value)
    let succeededCheckingValues = valueTypes.Length = optionValueTypes.Length

    if succeededCheckingValues then
        (state, Some (TupleType valueTypes))
    else
        (state, None)
    
and checkErrorPropagationExpression (state: TypeCheckerState) (errorPropagationExpression: ErrorPropagationOperatorExpression): TypeCheckerState * Option<PrestoType> =
    let (state, optionInnerExpressionType) = checkExpression state errorPropagationExpression.InnerExpression

    match optionInnerExpressionType with
    | Some innerExpressionType ->
        match innerExpressionType with
        | UnionInstanceType (uf, typeParameters) when uf.ScopeId = resultScopeId ->
            (state, Some typeParameters[0])
        | _ ->
            let error = compile_error(
                description = $"{innerExpressionType} is not a Result[T, E]",
                position = text_position(file_name = "", line_index = 0u, column_index = 0u)
            )

            (
                { state with Errors = state.Errors @ [error]},
                None
            )
    | None -> (state, None)

and checkCharacterLiteral (state: TypeCheckerState) (characterLiteral: CharacterLiteral): TypeCheckerState * Option<PrestoType> =
    (state, Some PrestoType.Character)

and checkStringLiteral (state: TypeCheckerState) (stringLiteral: StringLiteral): TypeCheckerState * Option<PrestoType> =
    (state, Some textType)

and checkExpression (state: TypeCheckerState) (expression: Expression): TypeCheckerState * Option<PrestoType> =
    if state.TypesByExpressionId.ContainsKey expression.Id then
        let prestoType = state.TypesByExpressionId[expression.Id]

        (state, Some prestoType)
    else
        let (state, optionPrestoType) =
            match expression.Value with
            | RecordExpression record -> checkRecord state record
            | UnionExpression union -> checkUnion state union
            | FunctionExpression fn -> checkFunction state expression fn
            | FunctionTypeExpression fnType -> checkFunctionType state expression fnType
            | IfThenElseExpression ifThenElse -> checkIfThenElse state ifThenElse
            | FunctionCallExpression call -> checkFunctionCall state call
            | BlockExpression block -> checkBlock state block
            | MemberAccessExpression memberAccess -> checkMemberAccess state memberAccess
            | BinaryOperatorExpression binaryOperator -> checkBinaryOperator state binaryOperator
            | GenericInstantiationExpression genericInstantiation -> checkGenericInstantiation state genericInstantiation
            | SymbolReference token -> checkSymbolReference state token expression.Id
            | NumberLiteralExpression number -> checkNumberLiteral state number
            | CharacterLiteralExpression characterLiteral -> checkCharacterLiteral state characterLiteral
            | StringLiteralExpression stringLiteral -> checkStringLiteral state stringLiteral
            | ParenExpr parenthesizedExpression -> checkParenExpr state parenthesizedExpression
            | TupleExpr tupleExpression -> checkTupleExpr state tupleExpression
            | ErrorPropagationExpression e -> checkErrorPropagationExpression state e
            | TypeClassExpression typeClass -> failwith "not implemented"
            | _ -> failwith "not implemented"

        match optionPrestoType with
        | Some prestoType ->
            (
                { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) },
                optionPrestoType
            )
        | None -> (state, None)

and trySetTypesCanonicalName (state: TypeCheckerState) (scopeId: System.Guid) (name: string): TypeCheckerState =
    if not (state.TypeCanonicalNamesByScopeId.ContainsKey scopeId) then
        { state with TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId.Add(scopeId, name) }
    else
        state

and checkBinding (state: TypeCheckerState) (binding: Binding): TypeCheckerState * Option<PrestoType> =
    let name = binding.NameToken._text

    let state =
        match binding.Value.Value with
        | RecordExpression record -> trySetTypesCanonicalName state record.ScopeId name
        | UnionExpression union -> trySetTypesCanonicalName state union.ScopeId name
        | FunctionExpression fn -> trySetTypesCanonicalName state fn.ScopeId name
        | _ -> state

    checkExpression state binding.Value

let checkTypes (program: Program): Program * List<compile_error> =
    let state = {
        ScopeIdStack = [program.ScopeId]
        ScopesById = program.ScopesById
        TypesByExpressionId = program.TypesByExpressionId
        ResolvedSymbolsByExpressionId = program.ResolvedSymbolsByExpressionId
        TypeCanonicalNamesByScopeId = program.TypeCanonicalNamesByScopeId
        Errors = []
    }

    let state = trySetTypesCanonicalName state resultScopeId "Result"
    let state = trySetTypesCanonicalName state seqScopeId "seq"
    let state = trySetTypesCanonicalName state groupingScopeId "Grouping"
    
    let (state, _) = checkMany state checkBinding program.Bindings []

    (
        {
            program with
                ScopesById = state.ScopesById
                TypesByExpressionId = state.TypesByExpressionId
                ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId
                TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId
        },
        state.Errors
    )
