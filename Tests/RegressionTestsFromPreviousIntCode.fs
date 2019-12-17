namespace Tests

open Xunit
open FsUnit
open Core.Computer

module RegressionTestsFromPreviousIntCode =

    [<Theory>]
    [<InlineData("1,0,0,0,99", "2,0,0,0,99")>]
    [<InlineData("2,3,0,3,99", "2,3,0,6,99")>]
    [<InlineData("2,4,4,5,99,0", "2,4,4,5,99,9801")>]
    [<InlineData("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")>]
    [<InlineData("1002,4,3,4,33", "1002,4,3,4,99")>]
    [<InlineData("1101,100,-1,4,0", "1101,100,-1,4,99")>]
    [<InlineData("1004,2,99", "1004,2,99")>]
    let ``str calculator without jump/lessthan/equal instructions`` (instructions:string, expectation:string) =
        let result = instructions |> compute 1I
        let (instructions, _) = result.Value
        instructions |> should equivalent (expectation.Split ',' |> Seq.map int|> Seq.map bigint)
    
    [<Fact>]
    let ``read output simple instruction`` () = 
        findInstruction 1I [|4I;4I;2I;3I;69I|] 0I 0I |> should equal (Some (Output(69I)))
    
    [<Fact>]
    let ``read output with more digit simple instruction`` () = 
        findInstruction 1I [|1004I;4I;2I;3I;42I|] 0I 0I |> should equal (Some (Output(42I)))
    
    [<Fact>]
    let ``read output with more digit simple instruction with 1 mode`` () = 
        findInstruction 1I [|1104I;42I|] 0I 0I |> should equal (Some (Output(42I)))
    
    [<Fact>]
    let ``Check the first input instruction`` () =
        findInstruction 1I [|3I;225I|] 0I 0I |> should equal (Some (Input(1I,225I)))
    
    [<Fact>]
    let ``Check the second input instruction`` () =
        findInstruction 1I [|3I;7I;1I;7I;6I;6I;1100I;1I|] 2I 0I |> should equal (Some (Add(1I, 1100I, 6I)))
    
    [<Fact>]
    let ``Check the third input instruction`` () =
        findInstruction 1I [|3I;12I;1I;12I;6I;6I;1101I;1I;238I;12I;104I;0I;1I|] 6I 0I |> should equal (Some (Add(1I, 238I, 12I)))
    
    [<Fact>]
    let ``Check the fourth input instruction`` () =
        findInstruction 1I [|3I;11I;1I;11I;6I;6I;1101I;1I;238I;225I;104I;0I|] 10I 0I |> should equal (Some (Output(0I)))
    
    [<Theory>]
    [<InlineData("", 1)>]
    [<InlineData("", 2)>]
    [<InlineData("0", 1)>]
    [<InlineData("10", 2)>]
    [<InlineData("110", 3)>]
    let ``Get mode X return Position`` (list: string) (pos: int) = 
        let input = list |> Seq.map (string) |> Seq.map int |> Seq.toList 
        let result = getModeX input pos
        result |> should equal Position
    
    [<Theory>]
    [<InlineData("1", 1)>]
    [<InlineData("01", 2)>]
    [<InlineData("001", 3)>]
    let ``Get mode X return Immediate`` (list: string) (pos: int) = 
        let input = list |> Seq.map (string) |> Seq.map int |> Seq.toList 
        let result = getModeX input pos
        result |> should equal Immediate
    
    [<Theory>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 8, 1)>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 1, 0)>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 42, 0)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 8, 1)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 1, 0)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 9, 0)>]
    let ``str calculator with equal instruction`` (instructions:string, input:bigint, expectation:int) =
        let result = instructions |> compute input 
        let (_, outputs) = result.Value
        outputs |> should equivalent [expectation]
    
    
    [<Theory>]
    [<InlineData("3,9,7,9,10,9,4,9,99,-1,8", 1, 1)>]
    [<InlineData("3,9,7,9,10,9,4,9,99,-1,8", -2, 1)>]
    [<InlineData("3,9,7,9,10,9,4,9,99,-1,8", 8, 0)>]
    [<InlineData("3,9,7,9,10,9,4,9,99,-1,8", 69, 0)>]
    [<InlineData("3,3,1107,-1,8,3,4,3,99", -4, 1)>]
    [<InlineData("3,3,1107,-1,8,3,4,3,99", 7, 1)>]
    [<InlineData("3,3,1107,-1,8,3,4,3,99", 8, 0)>]
    [<InlineData("3,3,1107,-1,8,3,4,3,99", 2389, 0)>]
    let ``str calculator with lessThan instructions`` (instructions:string, input:bigint, expectation:int) =
        let result = instructions |> compute input 
        let (_, outputs) = result.Value
        outputs |> should equivalent [expectation]
    
    [<Theory>]
    [<InlineData("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 0, 0)>]
    [<InlineData("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 0, 0)>]
    [<InlineData("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 42, 1)>]
    [<InlineData("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 69, 1)>]
    [<InlineData("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", -1, 1)>]
    [<InlineData("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", -45678, 1)>]
    let ``str calculator with jump instructions`` (instructions:string, input:bigint, expectation:int) =
        let result = instructions |> compute input 
        let (_, outputs) = result.Value
        outputs |> should equivalent [expectation]
    
    
    [<Theory>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 7, 999)>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 0, 999)>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", -42, 999)>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 8, 1000)>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 9, 1001)>]
    [<InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 34567, 1001)>]
    let ``ACCEPTANCE TESTS: compute with all instructions`` (instructions:string, input:bigint, expectation:int) =
        let result = instructions |> compute input 
        let (_, outputs) = result.Value
        outputs |> should equivalent [expectation]
    



