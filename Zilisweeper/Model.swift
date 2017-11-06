//
//  Model.swift
//  Zilisweeper
//
//  Created by Ruben Zilibowitz on 6/11/17.
//  Copyright © 2017 Ruben Zilibowitz. All rights reserved.
//

import Foundation
import MetalPerformanceShaders

class Model {
    /*
     Matrix multiplication parameters.  This code performs the operation:
     
     C = A * B
     
     Where C is M x N, A is M x K, and B is K x N.
     
     MPSMatrixMultiplication kernels are initialized with parameters for a
     generalized matrix multiplication in the same sense as the C BLAS
     level 3 *gemm routines.
     
     This operation uses alpha = 1.0 and beta = 0.0.  No matrices are
     transposed.
     
     MPSMatrix objects use row-major ordering.
     */
    var M:Int
    var N:Int
    var K:Int
    let alpha = 1.0
    let beta = 0.0
    
    // Use the default system device and an associated command queue.
    let device = MTLCreateSystemDefaultDevice()!
    var commandQueue:MTLCommandQueue?
    var sgemmKernel:MPSMatrixMultiplication?
    var commandBuffer:MTLCommandBuffer?
    
    var A:MPSMatrix?
    var B:MPSMatrix?
    var C:MPSMatrix?
    
    var b_elements: [Float]?
    
    static var shared:Model = Model()
    
    private init() {
        commandQueue = device.makeCommandQueue()
        
        M = 0
        N = 0
        K = 0
        
        do {
            guard let W_url = Bundle.main.url(forResource: "W", withExtension: "npy", subdirectory: nil, localization: nil) else { return }
            let W_npy = try Npy(contentsOf: W_url)
            let W_shape = W_npy.shape
            let W_elements: [Float] = W_npy.elements()
//            let W_isFortranOrder = W_npy.isFortranOrder
            
            guard let b_url = Bundle.main.url(forResource: "b", withExtension: "npy", subdirectory: nil, localization: nil) else { return }
            let b_npy = try Npy(contentsOf: b_url)
            let b_shape = b_npy.shape
            b_elements = b_npy.elements()
//            let b_isFortranOrder = b_npy.isFortranOrder
            
            M = 1
            N = W_shape[1]
            K = W_shape[0]
            
            print("W_shape \(W_shape)")
            print("b_shape \(b_shape)")
            
            /*
             A MPSMatrixDescriptor object will be used to specify the matrix
             properties to the MPSMatrix initialization routines.
             */
            var matrixDescriptor: MPSMatrixDescriptor

            // Each row of A has K values.
            let ARowBytes = MPSMatrixDescriptor.rowBytes(forColumns: K, dataType: MPSDataType.float32)
            
            // Each row of B has N values.
            let BRowBytes = MPSMatrixDescriptor.rowBytes(forColumns: N, dataType: MPSDataType.float32)
            
            // Each row of C has N values.
            let CRowBytes = MPSMatrixDescriptor.rowBytes(forColumns:N, dataType: MPSDataType.float32)
            
            // Create the buffers with the recommended sizes.
            let ABuffer = device.makeBuffer(length: M * ARowBytes)
            let BBuffer = device.makeBuffer(bytes: W_elements, length: K * BRowBytes)
            let CBuffer = device.makeBuffer(length: M * CRowBytes)
            
            
            /*
             All buffers are encapsulated in MPSMatrix objects.  Each MPSMatrix
             object is created with its associated buffer and an MPSMatrixDescriptor
             object which specifies dimension and type information for the matrix.
             */
            
            // The 'A' matrix.
            matrixDescriptor = MPSMatrixDescriptor(rows: M,
                                                   columns: K,
                                                   rowBytes: ARowBytes,
                                                   dataType: MPSDataType.float32)
            A = MPSMatrix(buffer: ABuffer!, descriptor: matrixDescriptor)
            
            // The 'B' matrix.
            matrixDescriptor.rows = K
            matrixDescriptor.columns = N
            matrixDescriptor.rowBytes = BRowBytes
            B = MPSMatrix(buffer: BBuffer!, descriptor: matrixDescriptor)
            
            // The 'C' matrix.
            matrixDescriptor.rows = M
            matrixDescriptor.rowBytes = CRowBytes
            C = MPSMatrix(buffer: CBuffer!, descriptor: matrixDescriptor)
            
            /*
             Create a kernel to perform generalized matrix multiplication on the
             system device using the desired parameters.
             */
            sgemmKernel = MPSMatrixMultiplication(device: device,
                                                  transposeLeft: false,
                                                  transposeRight: false,
                                                  resultRows: M,
                                                  resultColumns: N,
                                                  interiorColumns: K,
                                                  alpha: alpha,
                                                  beta: beta)
            
            // Create a command buffer in the queue.
            commandBuffer = commandQueue?.makeCommandBuffer()
        }
        catch {
            print(error.localizedDescription)
        }
    }
    
    func run(input: [Float]) -> [Float]? {
        
        guard let A = self.A else { return nil }
        guard let B = self.B else { return nil }
        guard let C = self.C else { return nil }
        
        guard let b_elements = self.b_elements else { return nil }
        
        guard input.count == M * K else { return nil }
        
        A.data.contents().copyBytes(from: input, count: input.count * MemoryLayout<Float>.stride)
        
        // Encode the kernel to the command buffer.
        sgemmKernel?.encode(commandBuffer:commandBuffer!,
                            leftMatrix: A,
                            rightMatrix: B,
                            resultMatrix: C)
        
        // Commit the buffer and wait for it to complete.
        commandBuffer?.commit()
        commandBuffer?.waitUntilCompleted()
        
        let result = C.data.contents().bindMemory(to: Float.self, capacity: N)
        for i in 0 ..< N {
            result[i] += b_elements[i]
        }
        
        return Array(UnsafeBufferPointer(start: result, count: N))
    }
}
