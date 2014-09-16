#if INTERACTIVE
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#r "../../packages/FsUnit.1.3.0.1/Lib/Net40/FsUnit.NUnit.dll"
#r "../../packages/FSharp.Data.2.0.9/lib/net40/FSharp.Data.dll"
#I "../../bin"
#r "FSharp.Data.dll"
#r "Foogle.Charts.dll"
#else
module Foogle.Tests.FormattingTests
#endif
open FsUnit
open FSharp.Data
open NUnit.Framework
open Foogle
open Google
open System.Reflection
open System.Text

[<Test>]
let ``Can format simple table`` () =
  let actual =
    { Labels = [ "Country"; "Popularity "] 
      Rows = 
        [ [ "Germany"; 200 ]
          [ "France"; 300 ]
          [ "United States"; 400 ] ] }
    |> Google.Formatting.formatTable

  let expected =
    JsonValue.Array
      [|JsonValue.Array [|JsonValue.String "Country"; JsonValue.String "Popularity "|];
        JsonValue.Array [|JsonValue.String "Germany"; JsonValue.Number 200M|];
        JsonValue.Array [|JsonValue.String "France"; JsonValue.Number 300M|];
        JsonValue.Array [|JsonValue.String "United States"; JsonValue.Number 400M|]|]
  actual |> should equal expected

[<Test>]
let `` Test JSON values`` () =
    let tasks = 
          [ "Work", 11; "Eat", 2; "Commute", 2
            "Watch TV", 2; "Sleep", 7 ]

    let chart = Chart.PieChart(tasks, Label = "Hours per Day")
                |> Chart.WithTitle(Title = "Daily activities")
                |> Chart.WithPie(PieHole = 0.5)

    let str = chart.Options |> toJSON

    let pInfos = chart.Options.GetType().GetProperties();

    let str = pInfos.[0].Name
    let valu = pInfos.[0].GetValue(chart.Options)
    let str2 = valu |> toJSON

    let opts_str = new StringBuilder()
    let opts =
        [|
            match chart.Chart with
            | GeoChart(g) -> 
                let p_chart = g.GetType().GetProperties()
                for i in 0 .. p_chart.Length - 1 do
                    yield! [p_chart.[i].Name, (p_chart.[i].GetValue(g) |> toJSON)] 
        
            | PieChart(p) ->
                let p_chart = p.GetType().GetProperties()
                for i in 0 .. p_chart.Length - 1 do
                    yield! [p_chart.[i].Name, (p_chart.[i].GetValue(p) |> toJSON )] 
        
            let p_gen = chart.Options.GetType().GetProperties()
            for i in 0 .. p_gen.Length - 1 do
                if p_gen.[i].Name <> "Empty" then
                    yield! [p_gen.[i].Name, (p_gen.[i].GetValue(chart.Options) |> toJSON )]

        |]
        |> Seq.iter(fun it ->
            opts_str.AppendFormat("{0}:{1},", fst it, snd it ) |> ignore
        )

    let s = opts_str.ToString()
    let s2 = s.Substring(0, s.Length-1)
    let s3 = "{ "+s2+"}"


    true |> should equal true

[<Test>]
let ``Testing scatter chart``() =
    let ageWieght =
      [ 8.0, 12.0
        4.0, 3.5
        11.0, 14.0
        4.0, 5.0
        3.0, 3.5
        6.5, 7.0]
    let chart = Chart.ScatterChart(ageWieght, "Age", "Weight", Labels = ["Age"; "Weight"])
                |> Chart.WithTitle(Title = "Age vs Weight comparison")

    true |> should equal true
