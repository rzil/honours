//
//  BoardView.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import UIKit

class BoardView: UIView {
    weak var board: Board!
    var dead: Bool = false {
        didSet {
            backgroundColor = dead ? .red : .lightGray
        }
    }
    
    var guess: (free: Bool, row: Int, col: Int)?
    
    override func awakeFromNib() {
        super.awakeFromNib()
        layer.borderWidth = 1
        dead = false
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        
        let rowHeight = bounds.height / CGFloat(board.rows)
        let colWidth = bounds.width / CGFloat(board.cols)
        
        guard let ctx = UIGraphicsGetCurrentContext() else { return }
        
        // draw guess
        
        if let (free,row,col) = self.guess {
            let rect = CGRect(x: colWidth * CGFloat(col), y: rowHeight * CGFloat(row), width: colWidth, height: rowHeight)
            if free {
                UIColor.blue.setFill()
            }
            else {
                UIColor.magenta.setFill()
            }
            UIRectFill(rect)
        }
        
        // draw cell outlines
        
        for r in 1 ..< board.rows {
            ctx.move(to: CGPoint(x: 0, y: rowHeight * CGFloat(r)))
            ctx.addLine(to: CGPoint(x: bounds.width, y: rowHeight * CGFloat(r)))
            ctx.strokePath()
        }
        
        for c in 1 ..< board.cols {
            ctx.move(to: CGPoint(x: colWidth * CGFloat(c), y: 0))
            ctx.addLine(to: CGPoint(x: colWidth * CGFloat(c), y: bounds.height))
            ctx.strokePath()
        }
        
        // draw adjacency info
        
        let paragraphStyle = NSMutableParagraphStyle()
        paragraphStyle.alignment = .center
        
        let attrs = [NSAttributedStringKey.font: UIFont.preferredFont(forTextStyle: .callout),
                     NSAttributedStringKey.paragraphStyle: paragraphStyle]
        
        for (i,a) in board.adjacency.enumerated() {
            guard a >= 0 else { continue }
            let (r,c) = board.cellFrom(index: i)
            let string = "\(a)"
            string.draw(with: CGRect(x: colWidth * CGFloat(c), y: rowHeight * CGFloat(r), width: colWidth, height: rowHeight), options: .usesLineFragmentOrigin, attributes: attrs, context: nil)
        }
    }
}
