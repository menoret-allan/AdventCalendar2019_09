namespace Core

module Computer =
    type Mode =
        | Position
        | Immediate
        | Relative
    
    type Instruction =
        | Add of (bigint * bigint * bigint)
        | Mult  of (bigint * bigint * bigint)
        | Input of (bigint * bigint)
        | Output of bigint
        | Jump of bigint
        | NoAction of bigint
        | LessThan of (bigint * bigint)
        | Equal of (bigint * bigint)
        | IncRelative of (bigint)
        | End

    let translate (instructions: string) =
        instructions.Split [|','|]
        |> Seq.map(int64)
        |> Seq.map(bigint)
        |> Seq.toArray
    
    let translateBack result =
        match result with
        | None -> "Unknown instruction found"
        | Some(instructions, outputs) -> "Instructions: " + (instructions |> Seq.map(string) |> String.concat ",") + "\nOutputs: " + (outputs |> Seq.map(string) |> String.concat ",")

    let getValue (instructions: bigint []) pos =
        if bigint(Seq.length instructions) <= pos then 0I
        else instructions.[int32(pos)]

    let fillWithNewEmpty instructions nb =
        let newValues= seq { for _ in 1I .. nb -> 0I } |> Seq.toArray
        Array.append instructions newValues

    let setValue (instructions: bigint []) value pos =
        let nbElemMissing = pos - (bigint(Seq.length instructions) - 1I)
        let instruc = if nbElemMissing >= 0I then fillWithNewEmpty instructions nbElemMissing else instructions
        instruc.[int32(pos)] <- value // might be broken
        instruc

    let getModeX list place =
        if list |> List.length < place then Position
        else if list.[place - 1] = 0 then Position
        else if list.[place - 1] = 2 then Relative
        else Immediate

    let getMode2 getVal (pos:bigint) (relativeVal:bigint) list (place :int) =
        match getModeX list place with
            | Position -> getVal (pos + (bigint place))
            | Immediate -> pos + (bigint place)
            | Relative -> (getVal (pos + (bigint place))) + relativeVal

    let jumpIf cond value position =
        match (value <> 0I) = cond with
        | true -> Jump(position)
        | false -> NoAction(3I)

    let lessThan left right position =
        if left < right then LessThan(1I, position)
        else LessThan(0I, position)

    let equalOp left right position =
        if left = right then Equal(1I, position)
        else Equal(0I, position)

    let findInstruction inputValue instructions (pos:bigint) (relativePos:bigint) =
        let getVal = getValue instructions
        let getPosition = getMode2 getVal pos relativePos
        let intList = instructions.[int32(pos)] |> string |> Seq.rev |> Seq.map string |> Seq.map int |> Seq.toList
        match intList with
        | 9::9::_ -> Some(End)
        | 1::0::rest | 1::rest -> Some(Add(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getPosition rest 3))
        | 2::0::rest | 2::rest -> Some(Mult(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getPosition rest 3))
        | 3::0::rest | 3::rest -> Some(Input(inputValue, getPosition rest 1))
        | 4::0::rest | 4::rest -> Some(Output(getPosition rest 1 |> getVal))
        | 5::0::rest | 5::rest -> Some(jumpIf true (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 6::0::rest | 6::rest -> Some(jumpIf false (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 7::0::rest | 7::rest -> Some(lessThan (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getPosition rest 3))
        | 8::0::rest | 8::rest -> Some(equalOp (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getPosition rest 3))
        | 9::0::rest | 9::rest -> Some(IncRelative(getPosition rest 1 |> getVal))
        | _ -> None

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let interprete inputValue (instructions: bigint []) =
        let rec loop (instructions: bigint []) (output: seq<bigint>) (pos:bigint) (relative:bigint) =
            let insertInInstruction = setValue instructions
            match findInstruction inputValue instructions pos relative with
            | Some End -> Some (instructions, output)
            | Some(Output(x)) -> loop instructions (Seq.append output (Seq.singleton x)) (pos+2I) relative
            | Some(Input(value, position)) -> loop (insertInInstruction value position) output (pos+2I) relative
            | Some(Add(left, right, position)) -> loop (insertInInstruction (left + right) position) output (pos+4I) relative
            | Some(Mult(left, right, position)) -> loop (insertInInstruction (left * right) position) output (pos+4I) relative
            | Some(Jump(position)) -> loop instructions output position relative
            | Some(NoAction(increment)) -> loop instructions output (pos+increment) relative
            | Some(LessThan(value, position)) -> loop (insertInInstruction value position) output (pos+4I) relative
            | Some(Equal(value, position)) -> loop (insertInInstruction value position) output (pos+4I) relative
            | Some(IncRelative(value)) -> loop instructions output (pos+2I) (relative + value)
            | None -> None
        loop instructions Seq.empty 0I 0I
    
    let compute inputValue (instructions: string) =
        instructions |> translate |> interprete inputValue
