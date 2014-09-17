module AST

open System

type Name = string
type astType = string

type Type =
    | ReturnType of astType
    | ReturnArrayType of astType
    | VariableType of astType
    | VariableArrayType of astType


type Param = {Name : Name; ParamType:Type}

type Prefix =
    | Final
    | Abstract
    | Virtual
    | Overwriten
    | Unsafe


type Visibility =
    | Public
    | Internal
    | Private
    | Protected
    | ProtectedInternal

type Expression =
    | Comparison
    | BoolStatement

type Statement =
    | If of Expression * seq<Statement>
    | Else of seq<Statement>
    | Declare of Name
    | Invoke
    | For
    | While
    | DoWhile
    | Foreach
    | Switch
    | Lock
    | Goto
    | Statements of seq<Statement>

type TypeMember =
    | Method of Option<Visibility> * Option<Prefix> * Type * Name * seq<Param> * Statement
    | Field of Option<Visibility> * Option<Prefix> * Type * Name 
    | Event 
    | Property
    | TypeMembers of seq<TypeMember>


type Types =
    | Class  of Visibility * Option<Prefix> * Name * TypeMember
    | Struct of Visibility * Option<Prefix> * Name * TypeMember
    | Delegate of Visibility * Name
    | Enum of Visibility * Name

type Using = string

type Scope = 
    | Namespace of Name * Option<Using> * seq<Types>
    | NotNamed of seq<Statement>
    | File of Option<Using> * seq<Scope>

   
//Small test if AST is usable
let c = 
      File(None,
        [Namespace ("Test", None,
            [Class(Public, None, "TestClass",
                TypeMembers [
                    Method(Some Public, None, ReturnType(""), "Cat", [], Statements[If (Comparison, []); Else []])
                ])
            ])
         ])