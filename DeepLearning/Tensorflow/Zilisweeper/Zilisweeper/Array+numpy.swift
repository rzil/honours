//
//  Array+numpy.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import Foundation

extension Array where Element == Double {
    func npy2array(filename: String) {
        let data = NSData(contentsOfFile: filename)
        
        // Get header length
        var headerLength : Int = 0
        data?.getBytes(&headerLength, range: NSRange(location: 8,length: 2))
        
        // Read header string
        let header = String(data: (data?.subdata(with: NSRange(location: 10, length: headerLength)))!, encoding: .utf8)
        
        // Parse shape
        let shapeStart = (header?.characters.indexOf(Character("("))?.advanced(by: 1))!
        let shapeEnd = (header?.characters.index(of: Character(")")))!
        var shape = header![shapeStart ..< shapeEnd].characters.split(Character(",")).map(String.init)
        for i in shape.indices {
            shape[i] = shape[i].stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet())
        }
        
        // Number of elements in array
        let count = shape.reduce(1, combine: {$0 * Int($1)!})
        
        // Actually read data to array
        var result = Array<T>(count: count, repeatedValue: sample)
        data?.getBytes(&result, range: NSRange(location: 10 + headerLength, length: count * sizeof(T)))
        
//        return result
    }}
