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
        adjacency[0] = 1
        adjacency[1] = 1
        adjacency[cols] = 1
    }
    
    func indexFrom(row:Int, col:Int) -> Int {
        return row * cols + col
    }
    
    func cellFrom(index: Int) -> (Int, Int) {
        return (index / cols, index % cols)
    }
    
    func markFree(row:Int, col:Int) {
        guard adjacency[indexFrom(row: row, col: col)] == -1 else { return }
    }
    
    func markMine(row:Int, col:Int) {
    }
}
