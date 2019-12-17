namespace Tests

open Xunit
open FsUnit
open Core.Computer

module Unit =

    [<Fact>]
    let ``Test relative with output`` () = 
        findInstruction 1I [|109I;1I;204I;-1I|] 2I 1I |> should equal (Some (Output(109I)))
  
    [<Fact>]
    let ``Test getMode2 on relative`` () =
        let instructions = [|109I;1I;204I;-1I|]
        let pos = 2I
        let relative = 1I
        let result = getMode2 (getValue instructions) pos relative [2] 1
        result |> should equal 0I

    [<Fact>]
    let ``Test getModeX on relative`` () =
        getModeX [2] 1 |> should equal Relative

    [<Fact>]
    let ``Test fill with empty value`` () =
        let result = fillWithNewEmpty [|1I;2I|] 3I
        result |> should haveLength 5
        result |> should equivalent [|1I;2I;0I;0I;0I|]
        result |> Array.head |> should equal 1I
        result |> Array.last |> should equal 0I

    [<Fact>]
    let ``setValue in the array`` () =
        let result = setValue [|1I;2I;3I|] 42I 1I
        result |> should haveLength 3
        result |> should equivalent [|1I;42I;3I|]
        result |> Array.head |> should equal 1I
        result.[1] |> should equal 42I
        result |> Array.last |> should equal 3I


    [<Fact>]
    let ``setValue outside the array`` () =
        let result = setValue [|1I;2I;3I|] 42I 10I
        result |> should haveLength 11
        result |> should equivalent [|1I;42I;3I;0I;0I;0I;0I;0I;0I;0I;2I|] // order does not matter
        result |> Array.head |> should equal 1I
        result.[1] |> should equal 2I
        result.[2] |> should equal 3I
        result.[3] |> should equal 0I
        result.[6] |> should equal 0I
        result.[9] |> should equal 0I
        result.[10] |> should equal 42I
        result |> Array.last |> should equal 42I
