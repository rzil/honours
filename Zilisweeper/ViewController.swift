//
//  ViewController.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import UIKit
import MetalPerformanceShaders

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    @IBAction func tappedButton(_ sender: UIButton) {
        let x:[Float] = Array(repeating: 1, count: 1440)
        guard let y = Model.shared.run(input: x), let max = y.max() else { return }
        guard let i = y.index(of: max) else { return }
        print(i)
        print((i / 12, i % 12))
    }
}
