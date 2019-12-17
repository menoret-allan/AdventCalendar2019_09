namespace Tests

open Xunit
open FsUnit
open Core.Computer

module Acceptance =
    [<Fact>]
    let ``Auto copy tests`` () =
        let input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        let result  = input |> compute 0
        let (_, outputs) = result.Value
        outputs |> should equivalent (input.Split ',' |> Seq.map int)

    [<Fact>]
    let ``16 digits number output`` () =
        let input = "1102,34915192,34915192,7,4,7,99,0"
        let result  = input |> compute 0
        let (_, outputs) = result.Value
        outputs |> Seq.head |> string |> should haveLength 16
    
    [<Fact>]
    let ``Output the big number in the middle`` () =
        let input = "104,1125899906842624,99"
        let result  = input |> compute 0
        let (_, outputs) = result.Value
        outputs |> Seq.head |> should equal  1125899906842624L


