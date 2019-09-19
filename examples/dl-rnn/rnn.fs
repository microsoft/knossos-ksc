module rnn

#if DiffSharp 

//DiffSharp interface
open F2K_DiffSharp

#else

//Knossos Interface
open Knossos

#endif


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
        { Wg = Glorot rnd (nh+nx) (2*nh)       //DM.init (nh+nx) (2*nh) (fun _ _ -> -0.5 + rnd.NextDouble())
          bg = Vec_one (2*nh)                  //DV.init (2*nh) (fun _ -> -0.5 + rnd.NextDouble())
          Wc = Glorot rnd (nh+nx) nh           //DM.init (nh+nx) nh (fun _ _ -> -0.5 + rnd.NextDouble())
          bc = Vec_one nh                      //DV.init nh (fun _ -> -0.5 + rnd.NextDouble())                         
          a = tanh                    
          size = nh          
        }        

    static member evalSinglePrecomp (Bg:Mat) (Bc:Mat) (l:GRU) (h:Mat) (x:Mat) =         
        let s = MatAppendCols x h                        
        let g = s * l.Wg + Bg |> sigmoid
        let r = g.GetSlice(None,None,None, Some (l.size-1))        
        let u = g.GetSlice(None,None,Some l.size, None)    
                
        let rs = r .* h
        let rs' = MatAppendCols x rs
        let c = rs' * l.Wc + Bc |> l.a
        u .* h + (1.0 - u) .* c

    static member evalSingle (l:GRU) (h:Mat) (x:Mat) =         
        let Bg = Mat_createRows x.Rows l.bg
        let Bc = Mat_createRows x.Rows l.bc
        GRU.evalSinglePrecomp Bg Bc l h x
        
    static member eval (input:Mat[]) (l:GRU) = 
        let k = input.[0].Rows
        let Bg = Mat_createRows k l.bg
        let Bc = Mat_createRows k l.bc
        let gruEval = GRU.evalSinglePrecomp Bg Bc l        
        let h0 = Mat_zero k l.size
        Array.fold gruEval h0 input                                

    member this.Eval (input:Mat[]) = GRU.eval input this

    static member VectorLength (gru:GRU) = 
        let l_Wg = gru.Wg.Rows*gru.Wg.Cols
        let l_Wc = gru.Wc.Rows*gru.Wc.Cols
        l_Wg + gru.bg.Length + l_Wc + gru.bc.Length

    static member Encode (gru:GRU) = 
        [ gru.Wg |> Vec_ofMat
          gru.bg
          gru.Wc |> Vec_ofMat
          gru.bc
        ]
        |> Vec_concat

    static member Decode (gru0:GRU) (V:Vec) =  
        let n1 = gru0.Wg.Rows*gru0.Wg.Cols
        let n1' = n1+gru0.bg.Length
        let Wg = V.[0..n1-1] |> Mat_ofVec gru0.Wg.Rows
        let bg = V.[n1..n1'-1]
        
        let n2 = n1' + gru0.Wc.Rows * gru0.Wc.Cols
        let n2' = n2+gru0.bc.Length
        let Wc = V.[n1'..n2-1] |> Mat_ofVec gru0.Wc.Rows
        let bc = V.[n2..n2'-1]

        {gru0 with Wg=Wg; bg=bg; Wc=Wc; bc=bc}
