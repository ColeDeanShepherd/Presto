module TypeChecker

open CompilerCore
open ASTBuilder
open Lexer

open type PrestoProgram

type TypeCheckerState = {
    ScopeIdStack: System.Guid list
    ScopesById: Map<System.Guid, Scope>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    TypeArgumentsByExpressionId: Map<System.Guid, Map<System.Guid, PrestoType>>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    TraitTypeFieldsByScopeId: Map<System.Guid, TraitTypeFields>
    Errors: List<CompileError>
}

let getTypeScopeId (state: TypeCheckerState) (prestoType: PrestoType): (Option<System.Guid> * TypeCheckerState) =
    match prestoType with
    | RecordType rt -> (Some rt.ScopeId, state)
    | EnumType ut -> (Some ut.ScopeId, state)
    | _ ->
        let error = CompileError(
            description = $"Couldn't access members of type: {prestoType}",
            position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
        )

        (None, { state with Errors = state.Errors @ [error] })

let rec resolveSymbolInternal (state: TypeCheckerState) (scope: Scope) (name: string) (nameTokenPosition: TextPosition): (Option<Symbol> * TypeCheckerState) =
    if scope.SymbolsByName.ContainsKey name then
        (Some scope.SymbolsByName.[name], state)
    else if scope.ParentId.IsSome then
        let parentScope = state.ScopesById[scope.ParentId.Value]

        resolveSymbolInternal state parentScope name nameTokenPosition
    else
        let error = CompileError(
            description = $"Unknown name: {name}",
            position = nameTokenPosition
        )
        let errors = List.append state.Errors [error]
        (None, { state with Errors = errors })

let getCurrentScopeId (state: TypeCheckerState): System.Guid =
    List.last state.ScopeIdStack

let getParentScopeId (state: TypeCheckerState): System.Guid =
    state.ScopeIdStack[state.ScopeIdStack.Length - 1]

let resolveSymbol (state: TypeCheckerState) (nameToken: Token): (Option<Symbol> * TypeCheckerState) =
    let currentScope = state.ScopesById[getCurrentScopeId state]

    resolveSymbolInternal state currentScope nameToken.text nameToken.position

let pushScope (state: TypeCheckerState) (scopeId: System.Guid): TypeCheckerState =
    let parentScopeId = getCurrentScopeId state
    let parentScope = state.ScopesById[parentScopeId]

    //if not (List.contains scopeId parentScope.ChildIds) then
    //    failwith $"Incorrectly pushed scope {scopeId}"
    //else
    { state with ScopeIdStack = state.ScopeIdStack @ [scopeId] }

let popScope (state: TypeCheckerState): TypeCheckerState =
    let scope = state.ScopesById[getCurrentScopeId state]

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

and checkEnum (state: TypeCheckerState) (enum: Enum): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state enum.ScopeId

    let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter enum.TypeParameters []
    let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)
    
    if successfullyCheckedTypeParameterTypes then
        let typeParameterIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        let (state, optionEnumTypeConstructors) = checkMany state checkEnumCase enum.Cases []
        let successfullyCheckedEnumTypeConstructors = not (List.exists<Option<EnumTypeConstructor>> (fun x -> x.IsNone) optionEnumTypeConstructors)
    
        if successfullyCheckedEnumTypeConstructors then
            let constructors =
                List.map<Option<EnumTypeConstructor>, EnumTypeConstructor>
                    (fun x -> x.Value)
                    optionEnumTypeConstructors

            (popScope state, Some (EnumType { ScopeId = enum.ScopeId; TypeParamNameAndIds = typeParameterIdAndNames; Constructors = constructors }))
        else
            (popScope state, None)
    else
        (popScope state, None)

and isFunctionType (prestoType: PrestoType): bool =
    match prestoType with
    | FunctionType _ -> true
    | _ -> false

and checkTrait (state: TypeCheckerState) (_trait: Trait): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state _trait.ScopeId

    let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter _trait.TypeParameters []
    let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)

    if successfullyCheckedTypeParameterTypes then
        let typeParamIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        let (state, optionBindingTypes) = checkMany state checkBinding _trait.Bindings []
        let bindingTypes =
            List.filter<Option<PrestoType>> (fun x -> x.IsSome && (isFunctionType x.Value)) optionBindingTypes
            |> List.map (fun x -> x.Value)

        let succeededCheckingBindings = bindingTypes.Length = optionBindingTypes.Length

        if succeededCheckingBindings then
            let functionTypesByName =
                List.zip _trait.Bindings bindingTypes
                |> List.map (fun (b, t) -> (b.NameToken.text, t))
                |> Map.ofList

            let ttf: TraitTypeFields = { ScopeId = _trait.ScopeId; TypeParamNameAndIds = typeParamIdAndNames; FunctionTypesByName = functionTypesByName }
            let prestoType = TraitType ttf
            let state = { state with TraitTypeFieldsByScopeId = state.TraitTypeFieldsByScopeId.Add(_trait.ScopeId, ttf) }

            (popScope state, Some prestoType)
        else
            (popScope state, None)
    else
        (popScope state, None)

and checkEnumCase (state: TypeCheckerState) (enumCase: EnumCase): TypeCheckerState * Option<EnumTypeConstructor> =
    let (state, optionParameterTypes) = checkMany state checkParameter enumCase.Parameters []
    let successfullyCheckedParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterNames = List.map<Parameter, string> (fun x -> x.NameToken.text) enumCase.Parameters
        let parameterTypes =
            List.map<Option<PrestoType>, PrestoType>
                (fun x -> x.Value)
                optionParameterTypes

        (state, Some { Name = enumCase.NameToken.text; Parameters = List.zip parameterNames parameterTypes})
    else
        (state, None)

and checkRecordField (state: TypeCheckerState) (recordField: RecordField): TypeCheckerState * Option<PrestoType> =
    checkExpression state recordField.TypeExpression

and checkRecord (state: TypeCheckerState) (record: Record): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state record.ScopeId

    let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter record.TypeParameters []
    let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)

    if successfullyCheckedTypeParameterTypes then
        let typeParamIdAndNames =
            List.map<Option<PrestoType>, System.Guid * string>
                (fun x ->
                    match x.Value with
                    | TypeParameterType (guid, name) -> (guid, name)
                    | _ -> failwith "Unexpected failure")
                optionTypeParameterTypes

        let (state, optionFieldTypes) = checkMany state checkRecordField record.Fields []
        let fieldTypes =
            List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionFieldTypes
            |> List.map (fun x -> x.Value)

        let succeededCheckingFields = fieldTypes.Length = optionFieldTypes.Length

        if succeededCheckingFields then
            let prestoType = RecordType { ScopeId = record.ScopeId; TypeParamNameAndIds = typeParamIdAndNames; FieldTypes = fieldTypes }

            (popScope state, Some prestoType)
        else
            (popScope state, None)
    else
        (popScope state, None)

and checkTypeParameter (state: TypeCheckerState) (typeParameter: TypeParameter): TypeCheckerState * Option<PrestoType> =
    (state, Some (TypeParameterType (typeParameter.Id, typeParameter.NameToken.text)))
    
and checkParameter (state: TypeCheckerState) (parameter: Parameter): TypeCheckerState * Option<PrestoType> =
    checkExpression state parameter.TypeExpression

// TODO: make sure we don't check types of anything twice via symbol references

and checkFunction (state: TypeCheckerState) (expression: Expression) (fn: Function): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state fn.ScopeId

    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let successfullyCheckedParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters

        let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter fn.TypeParameters []
        let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)
        
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
                        let error = CompileError(
                            description = $"Function's specified return type ({optionSpecifiedReturnType.Value}) doesn't match its inferred return type ({inferredReturnType})",
                            position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
    let successfullyCheckedParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionParameterTypes)

    if successfullyCheckedParameterTypes then
        let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters

        let (state, optionTypeParameterTypes) = checkMany state checkTypeParameter fn.TypeParameters []
        let successfullyCheckedTypeParameterTypes = not (List.exists<Option<PrestoType>> (fun x -> x.IsNone) optionTypeParameterTypes)
        
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
                        let error = CompileError(
                            description = $"\"then\" type ({thenExpressionType}) doesn't match \"else\" type ({elseExpressionType})",
                            position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
                        )
                        let state = { state with Errors = state.Errors @ [error] }

                        (state, None) // TODO: error
                | None -> (state, None)
            | None -> (state, None)
        else
            let error = CompileError(
                description = $"\"if\" type ({ifExpressionType}) isn't a bool",
                position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
            )
            let state = { state with Errors = state.Errors @ [error] }

            (state, None) // TODO: error
    | None -> (state, None)

and inferFunctionCallTypeArgs (paramTypes: List<PrestoType>) (argTypes: List<PrestoType>): Map<System.Guid, PrestoType> =
    let equalities = List.zip paramTypes argTypes
    let typesByTypeParamId: Map<System.Guid, PrestoType> = Map.empty
    typeInferenceHandleEqualities equalities typesByTypeParamId

// TODO: handle infinite recursion
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
            typeInferenceHandleTypeParameterEquality type1 id1 type2 typesByTypeParamId
            //let typesByTypeParamId = typeInferenceHandleTypeParameterEquality id1 type2 typesByTypeParamId
            //typeInferenceHandleTypeParameterEquality id2 type1 typesByTypeParamId
        | (TypeParameterType (id1, name1), _) ->
            typeInferenceHandleTypeParameterEquality type1 id1 type2 typesByTypeParamId
        | (_, TypeParameterType (id2, name2)) ->
            typeInferenceHandleTypeParameterEquality type2 id2 type1 typesByTypeParamId
        | (RecordInstanceType (tct1, typeArgs1), RecordInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (EnumInstanceType (tct1, typeArgs1), EnumInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (TraitInstanceType (tct1, typeArgs1), TraitInstanceType (tct2, typeArgs2)) ->
            let equalities = List.zip typeArgs1 typeArgs2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (TupleType elTypes1, TupleType elTypes2) ->
            let equalities = List.zip elTypes1 elTypes2
            typeInferenceHandleEqualities equalities typesByTypeParamId
        | (FunctionType ftf1, FunctionType ftf2) ->
            let equalities = List.zip ftf1.ParamTypes ftf2.ParamTypes
            let typesByTypeParamId = typeInferenceHandleEqualities equalities typesByTypeParamId
            typeInferenceHandleEquality ftf1.ReturnType ftf2.ReturnType typesByTypeParamId
        | _ -> typesByTypeParamId

and typeInferenceHandleTypeParameterEquality
    (typeParameterType: PrestoType)
    (typeParameterId: System.Guid)
    (otherType: PrestoType)
    (typesByTypeParamId: Map<System.Guid, PrestoType>)
    : Map<System.Guid, PrestoType> =
    // If the type parameter already equals something, substitute the parameter with what it's equal to.
    if typesByTypeParamId.ContainsKey typeParameterId then
        let prevInferredType = typesByTypeParamId[typeParameterId]

        typeInferenceHandleEquality prevInferredType otherType typesByTypeParamId
    else
        match otherType with 
        // If the type parameter have a substitution, but it's equal to another type parameter with a substitution,
        // then substitute the other type parameter.
        | TypeParameterType (otherId, _) ->
            if typesByTypeParamId.ContainsKey otherId then
                let prevInferredType = typesByTypeParamId[otherId]

                typeInferenceHandleEquality typeParameterType prevInferredType typesByTypeParamId
            else
                typesByTypeParamId.Add (typeParameterId, otherType)
        | _ -> typesByTypeParamId.Add (typeParameterId, otherType)

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
//    | EnumType (scopeId, typeParamNames, constructors) -> failwith "TODO"
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
//    | EnumInstanceType (scopeId, typeParameters) ->
//        match concreteType with
//        | EnumInstanceType (cScopeId, cTypeParameters) ->
//            if scopeId = cScopeId then
//                typeInferenceHandleEqualities typeArgsByName typeParameters cTypeParameters
//            else
//                failwith ""
//        | _ -> failwith ""
//    | TraitInstanceType (scopeId, typeParameters) ->
//        match concreteType with
//        | TraitInstanceType (cScopeId, cTypeParameters) ->
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
//    | TraitType (scopeId, typeArguments) -> failwith "TODO"

and _reifyType (prestoType: PrestoType) (typeArgsByParamId: Map<System.Guid, PrestoType>): PrestoType =
    match prestoType with
    | TypeParameterType (typeParameterId, typeParameterName) ->
        if typeArgsByParamId.ContainsKey typeParameterId then
            typeArgsByParamId[typeParameterId]
        else
            prestoType
    | TraitInstanceType (scopeId, typeArgumentTypes) ->
        let typeArgumentTypes = List.map (fun t -> _reifyType t typeArgsByParamId) typeArgumentTypes
        TraitInstanceType (scopeId, typeArgumentTypes)
    | FunctionType ft ->
        let reifiedParamTypes = List.map (fun t -> _reifyType t typeArgsByParamId) ft.ParamTypes
        let reifiedReturnType = _reifyType ft.ReturnType typeArgsByParamId
        FunctionType {
            ScopeId = ft.ScopeId
            TypeParamNameAndIds = []
            ParamTypes = reifiedParamTypes
            ReturnType = reifiedReturnType
        }
    | RecordInstanceType (rtf, typeArgs) ->
        let typeArgumentTypes = List.map (fun t -> _reifyType t typeArgsByParamId) typeArgs
        //let rtf = { rtf with TypeParamNameAndIds = [] }
        RecordInstanceType (rtf, typeArgumentTypes)
    | EnumInstanceType (utf, typeArgs) ->
        let typeArgumentTypes = List.map (fun t -> _reifyType t typeArgsByParamId) typeArgs
        //let utf = { utf with TypeParamNameAndIds = [] }
        EnumInstanceType (utf, typeArgumentTypes)
    | _ -> prestoType

and mergeMaps m1 m2 =
    Map.fold (fun s k v -> Map.add k v s) m1 m2

and checkFunctionCall (state: TypeCheckerState) (expressionId: System.Guid) (functionCall: FunctionCall): TypeCheckerState * Option<PrestoType> =
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

                    let state = { state with TypeArgumentsByExpressionId = state.TypeArgumentsByExpressionId.Add(expressionId, typeArgsByName) }

                    let reifiedReturnType = _reifyType ft.ReturnType typeArgsByName

                    //let state =
                    //    match reifiedReturnType with
                    //    | TypeParameterType (id, name) -> state
                    //    | _ -> state

                    (state, Some reifiedReturnType)
                | _ ->
                    let error = CompileError(
                        description = $"Expected function type, got {functionType}",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
            | TraitType tct ->
                let typeParameterNameCount = tct.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let expressionType = TraitInstanceType (tct, typeArgumentTypes)
                    (state, Some expressionType)
                else
                    let error = CompileError(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, Some expressionType) // Why are we returning expression type?
            | EnumType ut ->
                let typeParameterNameCount = ut.TypeParamNameAndIds.Length
                let typeArgumentCount = typeArgumentTypes.Length

                if typeParameterNameCount = typeArgumentCount then
                    let expressionType = EnumInstanceType (ut, typeArgumentTypes)
                    (state, Some expressionType)
                else
                    let error = CompileError(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
                    let error = CompileError(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
                    let error = CompileError(
                        description = $"Expected {typeParameterNameCount} type arguments, got {typeArgumentCount}",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
                    )
                    let state = { state with Errors = state.Errors @ [error]}

                    (state, None)
            | _ ->
                let error = CompileError(
                    description = $"Unexpected expression type {expressionType}",
                    position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
            let memberName = memberAccess.RightIdentifier.text

            if scope.SymbolsByName.ContainsKey memberName then
                let rightSymbol = scope.SymbolsByName[memberName]

                let (state, optionRightType) = checkSymbol state rightSymbol

                match optionRightType with
                | Some rightType -> (state, Some rightType)
                | None -> (state, None)
            else
                let error = CompileError(
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
    | EnumInstanceType (_, aTypeArgs), EnumInstanceType (_, bTypeArgs) ->
        (aTypeArgs.Length = bTypeArgs.Length) &&
        List.forall (fun (a, b) -> canTypesBeTheSame a b) (List.zip aTypeArgs bTypeArgs)
    //| RecordType (_, _, )
    | _ -> false
        
and checkBinaryOperator (state: TypeCheckerState) (expressionId: System.Guid) (binaryOperator: BinaryOperator): TypeCheckerState * Option<PrestoType> =
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
                            
                            let state = { state with TypeArgumentsByExpressionId = state.TypeArgumentsByExpressionId.Add(expressionId, typeArgsByName) }

                            let reifiedResultType = _reifyType resultType typeArgsByName

                            (state, Some reifiedResultType)
                        else
                            let error = CompileError(
                                description = $"Return type of left side ({lft.ReturnType}) of reverse function composition operator is not compatible with type of right side's parameter ({rft.ParamTypes[0]}).",
                                position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
                            )

                            (
                                { state with Errors = state.Errors @ [error]},
                                None
                            )
                    | _ ->
                        let error = CompileError(
                            description = $"Right side ({rightType}) of reverse function composition operator should be a function with one parameter.",
                            position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
                        )

                        (
                            { state with Errors = state.Errors @ [error]},
                            None
                        )
                | _ ->
                    let error = CompileError(
                        description = $"Left side ({leftType}) of reverse function composition operator should be a function.",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
                    let error = CompileError(
                        description = $"Left side ({leftType}) and right side ({rightType}) of binary operator ({binaryOperator.Type}) don't match.",
                        position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
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
    | EnumCaseSymbol enumCase -> (state, None)
    | BuiltInSymbol (builtInSymbol, prestoType) -> (state, Some prestoType)

and checkSymbolReference (state: TypeCheckerState) (token: Token) (expressionId: System.Guid): TypeCheckerState * Option<PrestoType> =
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
            if numberLiteral.Token.text.Contains('.') then
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
        | EnumInstanceType (uf, typeParameters) when state.TypeCanonicalNamesByScopeId[uf.ScopeId] = "Result" -> // TODO: fully-qualified name
            (state, Some typeParameters[0])
        | _ ->
            let error = CompileError(
                description = $"{innerExpressionType} is not a Result[T, E]",
                position = TextPosition(file_path = "", line_index = 0u, column_index = 0u)
            )

            (
                { state with Errors = state.Errors @ [error]},
                None
            )
    | None -> (state, None)

and checkSelfTypeExpr (state: TypeCheckerState) (expressionId: System.Guid) (traitId: System.Guid): TypeCheckerState * Option<PrestoType> =
    // cur scope's parent should be a trait
    // TODO: verify

    (state, Some (SelfType (expressionId, traitId)))

and checkCharacterLiteral (state: TypeCheckerState) (characterLiteral: CharacterLiteral): TypeCheckerState * Option<PrestoType> =
    (state, Some PrestoType.Character)

and checkStringLiteral (state: TypeCheckerState) (stringLiteral: StringLiteral): TypeCheckerState * Option<PrestoType> =
    (state, Some PrestoType.Text)

and checkExpression (state: TypeCheckerState) (expression: Expression): TypeCheckerState * Option<PrestoType> =
    if state.TypesByExpressionId.ContainsKey expression.Id then
        let prestoType = state.TypesByExpressionId[expression.Id]

        (state, Some prestoType)
    else
        let (state, optionPrestoType) =
            match expression.Value with
            | RecordExpression record -> checkRecord state record
            | EnumExpression enum -> checkEnum state enum
            | TraitExpression _trait -> checkTrait state _trait
            | FunctionExpression fn -> checkFunction state expression fn
            | FunctionTypeExpression fnType -> checkFunctionType state expression fnType
            | IfThenElseExpression ifThenElse -> checkIfThenElse state ifThenElse
            | FunctionCallExpression call -> checkFunctionCall state expression.Id call
            | BlockExpression block -> checkBlock state block
            | MemberAccessExpression memberAccess -> checkMemberAccess state memberAccess
            | BinaryOperatorExpression binaryOperator -> checkBinaryOperator state expression.Id binaryOperator
            | GenericInstantiationExpression genericInstantiation -> checkGenericInstantiation state genericInstantiation
            | SymbolReference token -> checkSymbolReference state token expression.Id
            | NumberLiteralExpression number -> checkNumberLiteral state number
            | CharacterLiteralExpression characterLiteral -> checkCharacterLiteral state characterLiteral
            | StringLiteralExpression stringLiteral -> checkStringLiteral state stringLiteral
            | ParenExpr parenthesizedExpression -> checkParenExpr state parenthesizedExpression
            | TupleExpr tupleExpression -> checkTupleExpr state tupleExpression
            | ErrorPropagationExpression e -> checkErrorPropagationExpression state e
            | SelfTypeExpr (traitId) -> checkSelfTypeExpr state expression.Id traitId
            | TraitExpression _trait -> failwith "not implemented"
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
    let name = binding.NameToken.text

    let state =
        match binding.Value.Value with
        | RecordExpression record -> trySetTypesCanonicalName state record.ScopeId name
        | EnumExpression enum -> trySetTypesCanonicalName state enum.ScopeId name
        | FunctionExpression fn -> trySetTypesCanonicalName state fn.ScopeId name
        | TraitExpression _trait -> trySetTypesCanonicalName state _trait.ScopeId name
        | _ -> state

    checkExpression state binding.Value

let checkTypes (program: Program): Program * List<CompileError> =
    let state = {
        ScopeIdStack = [program.ScopeId]
        ScopesById = program.ScopesById
        TypesByExpressionId = program.TypesByExpressionId
        TypeArgumentsByExpressionId = program.TypeArgumentsByExpressionId
        ResolvedSymbolsByExpressionId = program.ResolvedSymbolsByExpressionId
        TypeCanonicalNamesByScopeId = program.TypeCanonicalNamesByScopeId
        TraitTypeFieldsByScopeId = program.TraitTypeFieldsByScopeId
        Errors = []
    }

    let state = trySetTypesCanonicalName state resultScopeId "Result"
    
    let (state, _) = checkMany state checkBinding program.Bindings []

    (
        {
            Bindings = program.Bindings
            ScopeId = program.ScopeId
            ScopesById = program.ScopesById
            TraitTypeFieldsByScopeId = state.TraitTypeFieldsByScopeId
            TypesByExpressionId = state.TypesByExpressionId
            TypeArgumentsByExpressionId = state.TypeArgumentsByExpressionId
            ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId
            TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId
        },
        state.Errors
    )
