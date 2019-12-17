namespace Tests

open Xunit
open FsUnit
open Core.Computer

module Unit =

    [<Fact>]
    let ``Test relative with output`` () = 
        findInstruction 1 [|109;1;204;-1|] 2 1 |> should equal (Some (Output(109)))
  
    [<Fact>]
    let ``Test getMode2 on relative`` () =
        let instructions = [|109;1;204;-1|]
        let pos = 2
        let relative = 1
        let result = getMode2 (getValue instructions) pos relative [2] 1
        result |> should equal 0

    [<Fact>]
    let ``Test getModeX on relative`` () =
        getModeX [2] 1 |> should equal Relative

    [<Fact>]
    let ``Test fill with empty value`` () =
        let result = fillWithNewEmpty [|1;2|] 3
        result |> should haveLength 5
        result |> should equivalent [|1;2;0;0;0|]
        result |> Array.head |> should equal 1
        result |> Array.last |> should equal 0

    [<Fact>]
    let ``setValue in the array`` () =
        let result = setValue [|1;2;3|] 42 1
        result |> should haveLength 3
        result |> should equivalent [|1;42;3|]
        result |> Array.head |> should equal 1
        result.[1] |> should equal 42
        result |> Array.last |> should equal 3


    [<Fact>]
    let ``setValue outside the array`` () =
        let result = setValue [|1;2;3|] 42 10
        result |> should haveLength 11
        result |> should equivalent [|1;42;3;0;0;0;0;0;0;0;2|] // order does not matter
        result |> Array.head |> should equal 1
        result.[1] |> should equal 2
        result.[2] |> should equal 3
        result.[3] |> should equal 0
        result.[6] |> should equal 0
        result.[9] |> should equal 0
        result.[10] |> should equal 42
        result |> Array.last |> should equal 42
