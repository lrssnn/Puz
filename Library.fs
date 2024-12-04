﻿namespace Puz
open FSharpx.String

module Convenience =
    let inbounds a2d x y =
        x >= 0 && x < Array2D.length1 a2d &&
        y >= 0 && y < Array2D.length2 a2d
        
    let tupleEquals a b =
        fst a = fst b && snd a = snd b
        
    let splitOnce char input =
        let seq = splitChar' [| char |] 2 input
        (Array.head seq, Array.exactlyOne (Array.tail seq))
        
    /// Generate all possible neighbours of the given coordinates, as a sequence of (row * col) tuples
    /// Only returns diagonals if the withDiagonals arg is true
    let generateNeighbours withDiagonals row col =
        seq {
            yield (row - 1, col)
            yield (row, col - 1)
            yield (row, col + 1)
            yield (row + 1, col)

            if withDiagonals then
                yield (row - 1, col - 1)
                yield (row + 1, col - 1)
                yield (row + 1, col + 1)
                yield (row - 1, col + 1)
        }
        
module Pathfinding =
    open Convenience
    
    // Convert a path through the grid of characters into a sequence. Assumes that the grid contains
    // exactly one path with no branching, i.e. for any given non-space character, it has exactly
    // Two non-space neighbours (the one we came from, and the one we will go to
    let rec pathFollower (grid: char array2d) current cameFrom acc terminator =
        let acc = grid[fst current, snd current] :: acc
        if current ||> Array2D.get grid = terminator then List.rev acc
        else
            let (tx, ty) = current
            let nextCoord =
                [
                    (tx - 1, ty + 0)
                    (tx + 1, ty + 0)
                    (tx + 0, ty - 1)
                    (tx + 0, ty + 1)
                ]
                |> List.filter (fun (x, y) -> (inbounds grid x y) && (not (tupleEquals (x, y) cameFrom)) && not ((grid[x, y] =  ' ')))
                |> List.exactlyOne
            pathFollower
                grid
                nextCoord
                current
                acc
                terminator
        
