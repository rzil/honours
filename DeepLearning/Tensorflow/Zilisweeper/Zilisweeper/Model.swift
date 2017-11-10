//
//  Model.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import Foundation
import CoreML

class Model {
    static var shared:Model = Model()
    
    let model = Minesweeper8x8()
    
    private init() {}
    
    func boardSuggest(board:Board) -> (free:Bool, row: Int, col: Int)? {
        var x:[Float] = Array(repeating: -1, count: board.rows*board.cols*10)
        
        for (i,a) in board.adjacency.enumerated() {
            var oneHotVec:[Float] = Array(repeating: 0, count: 10)
            oneHotVec[a+1] = 1
            for j in 0 ..< 10 {
                x[j + i*10] = oneHotVec[j]
            }
        }
        
        guard let mlMultiArray = try? MLMultiArray(shape:[NSNumber(value: board.rows*board.cols*10)], dataType:MLMultiArrayDataType.double) else {
            fatalError("Unexpected runtime error. MLMultiArray")
        }
        for (index, element) in x.enumerated() {
            mlMultiArray[index] = NSNumber(floatLiteral: Double(element))
        }
        guard let movePredictions = try? model.prediction(board_state_one_hot: mlMultiArray) else {
            fatalError("Unexpected runtime error.")
        }
        
        var y:[Double] = Array(repeating: 0, count: board.rows*board.cols*2)
        for index in y.indices {
            y[index] = movePredictions.move_guesses[index].doubleValue
        }
        
        guard let max = y.max() else { return nil }
        guard let i = y.index(of: max) else { return nil }
        print(i)
        if i < board.rows*board.cols {
            return (free: true, row: i / board.cols, col: i % board.cols)
        }
        else {
            let j = i - board.rows*board.cols
            return (free: false, row: j / board.cols, col: j % board.cols)
        }
    }
}
