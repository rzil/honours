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
        guard let result = Model.shared.boardSuggest(board: board) else { return }
        print(result)
    }
    
    @IBAction func tappedBoardView(_ sender: UITapGestureRecognizer) {
        let location = sender.location(in: boardView)
        let tappedRow = Int(location.y / boardView.bounds.height * CGFloat(board.rows))
        let tappedCol = Int(location.x / boardView.bounds.width * CGFloat(board.cols))
        board.markFree(row: tappedRow, col: tappedCol)
    }
}
