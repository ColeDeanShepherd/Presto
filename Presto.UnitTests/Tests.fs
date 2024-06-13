module Tests

open System
open Xunit
open ASTBuilder
open TypeChecker

[<Fact>]
let ``inferFunctionCallTypeArgs`` () =
    let seqTypeFields: TypeClassTypeFields = {
        ScopeId = System.Guid.NewGuid()
        TypeParamNameAndIds = [(System.Guid.NewGuid(), "t")]
    }

    let seqType = TypeClassType seqTypeFields

    let textType = PrestoType.Text (System.Guid.NewGuid())

    let tTypeParamId = System.Guid.NewGuid()
    let tTypeParam = TypeParameterType (tTypeParamId, "t")

    let tKeyTypeParamId = System.Guid.NewGuid()
    let tKeyTypeParam = TypeParameterType (tKeyTypeParamId, "tkey")
    
    let t1TypeParamId = System.Guid.NewGuid()
    let t1TypeParam = TypeParameterType (t1TypeParamId, "t1")

    let t2TypeParamId = System.Guid.NewGuid()
    let t2TypeParam = TypeParameterType (t2TypeParamId, "t2")

    let paramTypes = [
        TypeClassInstanceType (seqTypeFields, [tTypeParam])
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
        TypeClassInstanceType (seqTypeFields, [TupleType [textType; PrestoType.Real]])
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

    Assert.Equal(TupleType [textType; PrestoType.Real], typesByTypeParamId[tTypeParamId])
    Assert.Equal(textType, typesByTypeParamId[tKeyTypeParamId])
    Assert.Equal(textType, typesByTypeParamId[t1TypeParamId])
    Assert.Equal(PrestoType.Real, typesByTypeParamId[t2TypeParamId])
