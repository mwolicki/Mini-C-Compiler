#if INTERACTIVE
    #r "/Users/kevin/Projects/MarcinsLanguageCompiler/packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
    #r "/Users/kevin/Projects/MarcinsLanguageCompiler/packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"
#endif

open FParsec
open AST

let test p str =
    run p str |> function
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let pvisib = 
        (pstring "public" 
        <|> pstring "private"
        <|> pstring "protected"
        <|> pstring "internal"
        <|> pstring "protected internal")
        |>> (function
                | "public" -> Public
                | "protected" -> Protected
                | "protected internal" -> ProtectedInternal
                | "internal" -> Internal
                | "private" -> Private
                | _ as p -> failwith (sprintf "unknown visibility mode: %A" p)
            )
            
let pident = many1Satisfy (fun a -> System.Char.IsLetterOrDigit a)

let pname = pident |>> fun s-> s

let preturnType =  
            attempt (spaces >>. pident .>> spaces .>> pstring "[" .>> spaces .>> pstring "]" |>> fun p -> ReturnArrayType (p))
            <|> (spaces >>. pident |>> fun p -> ReturnType(p))

let pParam = preturnType .>> spaces .>>. pident |>> fun (retTyp, name) -> { Name = name; ParamType = retTyp }

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