module Puz.Input

open System.IO
open FSharpx.String

let readText path = trim (File.ReadAllText path)

/// Get the list of integers from the input string.
/// Assumes the string is one line entirely made up of
/// numerical characters
let readCharsToNums (path: string) =
    readText path
    |> _.Trim()
    |> Seq.map System.Char.GetNumericValue
    |> Seq.map int
    |> Seq.toList

/// Read each line in the string as a list of integers separated by the character provided
let readLinesAsInts (path: string) (separator: char) : int list list =
    readText path
    |> _.Trim()
    |> _.Split('\n')
    |> Array.toList
    |> List.map (fun line -> line.Trim() |> _.Split(separator) |> Array.toList |> List.map int)
