//
//  ViewController.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    
    @IBOutlet weak var boardView: BoardView!
    
    var board = Board()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        boardView.board = board
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    @IBAction func tappedButton(_ sender: UIButton) {
        var x:[Float] = Array(repeating: -1, count: 1440)
        x[0] = 1
        x[1] = 1
        x[12] = 1
        guard let y = Model.shared.run(input: x), let max = y.max() else { return }
        guard let i = y.index(of: max) else { return }
        print(i)
        if i < 144 {
            print("free at \((i / 12, i % 12))")
        }
        else {
            let j = i - 144
            print("mine at \((j / 12, j % 12))")
        }
    }
    
    @IBAction func tappedBoardView(_ sender: UITapGestureRecognizer) {
        let location = sender.location(in: boardView)
        let tappedRow = Int(location.y / boardView.bounds.height * CGFloat(board.rows))
        let tappedCol = Int(location.x / boardView.bounds.width * CGFloat(board.cols))
        board.markFree(row: tappedRow, col: tappedCol)
    }
}
