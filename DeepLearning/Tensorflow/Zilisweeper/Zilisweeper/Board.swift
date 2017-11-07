//
//  Board.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import Foundation

class Board {
    var rows: Int = 12
    var cols: Int = 12
    
    var mines: [Bool]
    var adjacency: [Int]
    
    init() {
        mines = Array(repeating: false, count: rows * cols)
        adjacency = Array(repeating: -1, count: rows * cols)
    }
    
    func randomiseMines(mineProbability: Double) {
        for i in 0 ..< mines.count {
            mines[i] = drand48() < mineProbability
        }
    }
    
    func reset() {
        for i in 0 ..< adjacency.count {
            adjacency[i] = -1
        }
    }
    
    func indexFrom(row:Int, col:Int) -> Int {
        return row * cols + col
    }
    
    func cellFrom(index: Int) -> (Int, Int) {
        return (index / cols, index % cols)
    }
    
    func markFree(row:Int, col:Int) -> Bool {
        let index = indexFrom(row: row, col: col)
        guard mines[index] == false else { return true }
        guard adjacency[index] == -1 else { return false }
        var count: Int = 0
        for r in row-1 ... row+1 {
            for c in col-1 ... col+1 {
                if 0 <= r && r < rows && 0 <= c && c < cols {
                    count += mines[indexFrom(row: r, col: c)] ? 1 : 0
                }
            }
        }
        
        if count == 0 {
            adjacency[index] = 0
            for r in row-1 ... row+1 {
                for c in col-1 ... col+1 {
                    if 0 <= r && r < rows && 0 <= c && c < cols {
                        _ = markFree(row: r, col: c)
                    }
                }
            }
        }
        else {
            adjacency[index] = count
        }
        
        return false
    }
    
    func markMine(row:Int, col:Int) {
    }
}
