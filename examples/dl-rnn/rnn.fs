module rnn

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif

let Glorot (rnd:System.Random) x y = 
    let l = sqrt(6.0 / (float (x + y)))
    build2 x y (fun _ _ -> l*(2.0*rnd.NextDouble() - 1.0) |> Float)


//https://github.com/tensorflow/tensorflow/blob/master/tensorflow/python/ops/rnn_cell_impl.py#L351        
type GRU = 
    { Wg:Mat          //gate kernel
      bg:Vec          //gate bias
      Wc:Mat          //candidate kernel
      bc:Vec          //candidate bias      
      a:Mat -> Mat    //activation            
      size :int
    }
    
    static member init (rnd:System.Random) nh nx = 
        { Wg = Glorot rnd (nh+nx) (2*nh)            //DM.init (nh+nx) (2*nh) (fun _ _ -> -0.5 + rnd.NextDouble())
          bg = build (2*nh) (fun _ -> Float 1.0)    //DV.init (2*nh) (fun _ -> -0.5 + rnd.NextDouble())
          Wc = Glorot rnd (nh+nx) nh                //DM.init (nh+nx) nh (fun _ _ -> -0.5 + rnd.NextDouble())
          bc = build nh (fun _ -> Float 1.0)        //DV.init nh (fun _ -> -0.5 + rnd.NextDouble())                         
          a = tanh                    
          size = nh          
        }        

    //TODO: The following function is much cleaner with DiffSharp (e.g. 1-A, A.*A', A*A' for Mat A,A')
    static member evalSinglePrecomp (Bg:Mat) (Bc:Mat) (l:GRU) (h:Mat) (x:Mat) =         
        let s = MatAppendCols x h                        
        let g = (MatMul s l.Wg) + Bg |> sigmoid
        let r = g |> MatGetSlice(None,None,None, Some (l.size-1))        
        let u = g |> MatGetSlice(None,None,Some l.size, None)    
                
        let rs = ElemProd r h
        let rs' = MatAppendCols x rs
        let c = (MatMul rs' l.Wc) + Bc |> l.a
        (ElemProd u h) + (ElemProd (Minus 1.0 u) c)

    static member evalSingle (l:GRU) (h:Mat) (x:Mat) =         
        let Bg = CreateRows (Rows x) l.bg
        let Bc = CreateRows (Rows x) l.bc
        GRU.evalSinglePrecomp Bg Bc l h x
        
    static member eval (input:Mat[]) (l:GRU) = 
        let k = Rows input.[0]
        let Bg = CreateRows k l.bg
        let Bc = CreateRows k l.bc
        let gruEval = GRU.evalSinglePrecomp Bg Bc l        
        let h0 = build2 k l.size (fun _ _ -> Float 0.0)
        Array.fold gruEval h0 input                                

    member this.Eval (input:Mat[]) = GRU.eval input this

    static member VectorLength (gru:GRU) = 
        let l_Wg = (Rows gru.Wg)*(Cols gru.Wg)
        let l_Wc = (Rows gru.Wc)*(Cols gru.Wc)
        l_Wg + gru.bg.Length + l_Wc + gru.bc.Length

    static member Encode (gru:GRU) = 
        [ gru.Wg |> MatToVec
          gru.bg
          gru.Wc |> MatToVec
          gru.bc
        ]
        |> VecConcat

    static member Decode (gru0:GRU) (V:Vec) =  
        let n1 = (Rows gru0.Wg)*(Cols gru0.Wg)
        let n1' = n1+gru0.bg.Length
        let Wg = V.[0..n1-1] |> VecToMat (Rows gru0.Wg)
        let bg = V.[n1..n1'-1]
        
        let n2 = n1' + (Rows gru0.Wc) * (Cols gru0.Wc)
        let n2' = n2+gru0.bc.Length
        let Wc = V.[n1'..n2-1] |> VecToMat (Rows gru0.Wc)
        let bc = V.[n2..n2'-1]

        {gru0 with Wg=Wg; bg=bg; Wc=Wc; bc=bc}
