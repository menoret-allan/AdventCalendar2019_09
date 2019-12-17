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
        let result = instructions |> compute 1
        let (instructions, _) = result.Value
        instructions |> should equivalent (expectation.Split ',' |> Seq.map int)
    
    [<Fact>]
    let ``read output simple instruction`` () = 
        findInstruction 1 [|4;4;2;3;69|] 0 0 |> should equal (Some (Output(69)))
    
    [<Fact>]
    let ``read output with more digit simple instruction`` () = 
        findInstruction 1 [|1004;4;2;3;42|] 0 0 |> should equal (Some (Output(42)))
    
    [<Fact>]
    let ``read output with more digit simple instruction with 1 mode`` () = 
        findInstruction 1 [|1104;42|] 0 0 |> should equal (Some (Output(42)))
    
    [<Fact>]
    let ``Check the first input instruction`` () =
        findInstruction 1 [|3;225|] 0 0 |> should equal (Some (Input(1,225)))
    
    [<Fact>]
    let ``Check the second input instruction`` () =
        findInstruction 1 [|3;7;1;7;6;6;1100;1|] 2 0 |> should equal (Some (Add(1, 1100, 6)))
    
    [<Fact>]
    let ``Check the third input instruction`` () =
        findInstruction 1 [|3;12;1;12;6;6;1101;1;238;12;104;0;1|] 6 0 |> should equal (Some (Add(1, 238, 12)))
    
    [<Fact>]
    let ``Check the fourth input instruction`` () =
        findInstruction 1 [|3;11;1;11;6;6;1101;1;238;225;104;0|] 10 0 |> should equal (Some (Output(0)))
    
    [<Theory>]
    [<InlineData("", 1)>]
    [<InlineData("", 2)>]
    [<InlineData("", 3)>]
    [<InlineData("0", 1)>]
    [<InlineData("10", 2)>]
    [<InlineData("110", 3)>]
    let ``Get mode X return Position`` (list: string) (pos: int) = 
        let input = list |> Seq.map (string) |> Seq.map (int) |> Seq.toList 
        let result = getModeX input pos
        result |> should equal Position
    
    [<Theory>]
    [<InlineData("1", 1)>]
    [<InlineData("01", 2)>]
    [<InlineData("001", 3)>]
    let ``Get mode X return Immediate`` (list: string) (pos: int) = 
        let input = list |> Seq.map (string) |> Seq.map (int) |> Seq.toList 
        let result = getModeX input pos
        result |> should equal Immediate
    
    [<Theory>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 8, 1)>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 1, 0)>]
    [<InlineData("3,9,8,9,10,9,4,9,99,-1,8", 42, 0)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 8, 1)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 1, 0)>]
    [<InlineData("3,3,1108,-1,8,3,4,3,99", 9, 0)>]
    let ``str calculator with equal instruction`` (instructions:string, input:int, expectation:int) =
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
    let ``str calculator with lessThan instructions`` (instructions:string, input:int, expectation:int) =
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
    let ``str calculator with jump instructions`` (instructions:string, input:int, expectation:int) =
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
    let ``ACCEPTANCE TESTS: compute with all instructions`` (instructions:string, input:int, expectation:int) =
        let result = instructions |> compute input 
        let (_, outputs) = result.Value
        outputs |> should equivalent [expectation]
    



