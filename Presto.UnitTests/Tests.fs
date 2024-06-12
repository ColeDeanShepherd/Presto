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

    let tTypeParam = TypeParameterType (System.Guid.NewGuid(), "t")
    let tKeyTypeParam = TypeParameterType (System.Guid.NewGuid(), "tkey")
    
    let t1TypeParam = TypeParameterType (System.Guid.NewGuid(), "t1")
    let t2TypeParam = TypeParameterType (System.Guid.NewGuid(), "t2")

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

    Assert.True(true)
