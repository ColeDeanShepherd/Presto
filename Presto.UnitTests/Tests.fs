module Tests

open Xunit
open ASTBuilder
open TypeChecker

[<Fact>]
let ``inferFunctionCallTypeArgs1`` () =
    let seqTypeFields: TraitTypeFields = {
        ScopeId = System.Guid.NewGuid()
        TypeParamNameAndIds = [(System.Guid.NewGuid(), "T")]
        FunctionTypesByName = Map.empty
    }

    let seqType = TraitType seqTypeFields

    let tKeyTypeParamId = System.Guid.NewGuid()
    let tKeyTypeParam = TypeParameterType (tKeyTypeParamId, "TKey")

    let tTypeParamId = System.Guid.NewGuid()
    let tTypeParam = TypeParameterType (tTypeParamId, "T")
    
    let t1TypeParamId = System.Guid.NewGuid()
    let t1TypeParam = TypeParameterType (t1TypeParamId, "T1")

    let t2TypeParamId = System.Guid.NewGuid()
    let t2TypeParam = TypeParameterType (t2TypeParamId, "T2")

    let paramTypes = [
        TraitInstanceType (seqTypeFields, [tTypeParam])
        FunctionType {
            ScopeId = System.Guid.NewGuid()
            TypeParamNameAndIds = []
            ParamTypes = [
                tTypeParam
            ]
            ReturnType = tKeyTypeParam
        }
    ]
    let argTypes = [
        TraitInstanceType (seqTypeFields, [TupleType [PrestoType.Text; PrestoType.Real]])
        FunctionType {
            ScopeId = System.Guid.NewGuid()
            TypeParamNameAndIds = []
            ParamTypes = [
                TupleType [t1TypeParam; t2TypeParam]
            ]
            ReturnType = t1TypeParam
        }
    ]

    let typesByTypeParamId = inferFunctionCallTypeArgs paramTypes argTypes

    Assert.Equal(TupleType [PrestoType.Text; PrestoType.Real], typesByTypeParamId[tTypeParamId])
    Assert.Equal(PrestoType.Text, typesByTypeParamId[tKeyTypeParamId])
    Assert.Equal(PrestoType.Text, typesByTypeParamId[t1TypeParamId])
    Assert.Equal(PrestoType.Real, typesByTypeParamId[t2TypeParamId])

[<Fact>]
let ``inferFunctionCallTypeArgs2`` () =
    let seqTypeFields: TraitTypeFields = {
        ScopeId = System.Guid.NewGuid()
        TypeParamNameAndIds = [(System.Guid.NewGuid(), "T")]
        FunctionTypesByName = Map.empty
    }

    let seqType = TraitType seqTypeFields

    let groupingTypeFields: RecordTypeFields = {
        ScopeId = System.Guid.NewGuid()
        TypeParamNameAndIds = [(System.Guid.NewGuid(), "T"); (System.Guid.NewGuid(), "TKey")]
        FieldTypes = []
    }
    let groupingType = RecordType groupingTypeFields

    let groupingInstanceType = RecordInstanceType (groupingTypeFields, [PrestoType.Text; PrestoType.Real])
    
    let mapTiTypeParamId = System.Guid.NewGuid()
    let mapTiTypeParam = TypeParameterType (mapTiTypeParamId, "TIn")

    let mapToTypeParamId = System.Guid.NewGuid()
    let mapToTypeParam = TypeParameterType (mapToTypeParamId, "TOut")

    let paramTypes = [
        TraitInstanceType (seqTypeFields, [mapTiTypeParam])
        FunctionType {
            ScopeId = System.Guid.NewGuid()
            TypeParamNameAndIds = []
            ParamTypes = [
                mapTiTypeParam
            ]
            ReturnType = mapToTypeParam
        }
    ]
    let argTypes = [
        TraitInstanceType (seqTypeFields, [groupingInstanceType])
        FunctionType {
            ScopeId = System.Guid.NewGuid()
            TypeParamNameAndIds = []
            ParamTypes = [
                groupingInstanceType
            ]
            ReturnType = TupleType [PrestoType.Text; PrestoType.Real; PrestoType.Real; PrestoType.Real]
        }
    ]

    let typesByTypeParamId = inferFunctionCallTypeArgs paramTypes argTypes

    Assert.Equal(groupingInstanceType, typesByTypeParamId[mapTiTypeParamId])
    Assert.Equal(TupleType [PrestoType.Text; PrestoType.Real; PrestoType.Real; PrestoType.Real], typesByTypeParamId[mapToTypeParamId])