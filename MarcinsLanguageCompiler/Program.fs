type Name(name:string) =

    override this.ToString() =
        sprintf "%A" name


type Type =
    | ReturnType of string
    | ReturnArrayType of string
    | VariableType of string
    | VariableArrayType of string


type Param(name: Name, paramType:Type) =
    let ParamName: Name = name
    let ParamType: Type = paramType

    override this.ToString() =
        sprintf "%A of %A" name paramType


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

type Using(using: string) = class end

type Scope = 
    | Namespace of Name * Option<Using> * seq<Types>
    | NotNamed of seq<Statement>
    | File of Option<Using> * seq<Scope>


let c = 
      File(None, 
        [Namespace (Name("Test"), None,
            [Class(Public, None, Name("TestClass"), 
                TypeMembers [
                    Method(Some Public, None, ReturnType(""), Name("Cat"), [], Statements[If (Comparison, []); Else []])
                ])
            ])
         ])

#if INTERACTIVE
    #r "/Users/kevin/Projects/MarcinsLanguageCompiler/packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
    #r "/Users/kevin/Projects/MarcinsLanguageCompiler/packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"
#endif

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let pvisib = 
        (pstring "public" 
        <|> pstring "private"
        <|> pstring "protected"
        <|> pstring "internal"
        <|> pstring "protected internal")
        |>> (fun p -> 
               match p with
                | "public" -> Public
                | "protected" -> Protected
                | "protected internal" -> ProtectedInternal
                | "internal" -> Internal
                | "private" -> Private
                | _ -> failwith (sprintf "unknown visibility mode: %A" p)
            )

let pident = many1Satisfy (fun a -> System.Char.IsLetterOrDigit a)

let pname = pident |>> fun s-> Name(s)

let preturnType =  
            attempt (spaces >>. pident .>> spaces .>> pstring "[" .>> spaces .>> pstring "]" |>> fun p -> ReturnArrayType (p))
            <|> (spaces >>. pident |>> fun p -> ReturnType(p))

let pParam = preturnType .>> spaces .>>. pident |>> fun (retTyp, name) -> Param(Name(name), retTyp)

let pStm = preturnType .>> spaces .>>. pident |>> fun (retTyp, name) -> For


let pStms = 
            attempt (spaces >>. pstring "{" .>> spaces >>. pstring "}" |>> fun _ -> Statements([]))
            <|> (spaces >>. pstring "{" >>. many pStm .>> pstring "}" |>> fun stms -> Statements(stms))


let pMethod = 
            tuple5 (opt pvisib) 
                preturnType
                (spaces1 >>. pname .>> spaces)
                (between (pstring "(") (pstring ")") (sepBy pParam (spaces >>. pstring "," .>> spaces))) 
                pStms
                |>> (fun (vis, retType, name, param, stms) -> Method(vis, None, retType, name, param |> List.toSeq, stms))

test pMethod 
        """private void Main(string[] arg, int number)
        {
            System.Console.WriteLine("Example Test - should be displayed on screen");
        }"""
            