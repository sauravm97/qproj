namespace Microsoft.Research.Liquid

module UserSample =
    open System
    open Util
    open Operations
    open System.Collections.Generic
    //open Native             // Support for Native Interop
    open HamiltonianGates   // Extra gates for doing Hamiltonian simulations
    //open Tests              // All the built-in tests
    

    let pow2 (n: int) = 1 <<< n

    let (+|) (xs: 'a array) (x: 'a) = Array.append xs [|x|]
    let (++|) (xs: 'a array) (ys: 'a array) = Array.append xs ys
    let (+.) (xs: 'a list) (x: 'a) = List.append xs [x]
    let (++.) (xs: 'a list) (ys: 'a list) = List.append xs ys

    let CombineGates (gates: (Qubits -> unit) array) (qs: Qubits) =        
        let mutable mat = CSMat(pow2 qs.Length)
        for gate in gates do
            let gMat = (!< gate qs).Mat
            mat <- mat * gMat
        let gate = new Gate(Mat = mat)
        gate.Run qs

    let ControlGate (gate: Qubits -> unit) (controls: int list) (gateIndices: int list) (qs: Qubits) =
        let g = !< gate qs.[..gateIndices.Length-1]
        let gLen = int (Math.Log(float g.Mat.Length, 2.))
        for gateIndex in gateIndices do
            assert (not (List.contains gateIndex controls))
        let neutralLen = qs.Length - gLen - controls.Length
        let neutralIndices = Array.replicate (pow2 neutralLen) 0
        let mutable neutralCounter = 0

        for i in 0..qs.Length - 1 do
            if not (List.contains i controls || List.contains i gateIndices) then
                let v = pow2 (qs.Length - i - 1)
                for j in pow2 neutralCounter..pow2 (neutralCounter + 1)..neutralIndices.Length-1 do
                    for k in 0..pow2 neutralCounter-1 do
                        neutralIndices.[j+k] <- neutralIndices.[j+k] + v
                neutralCounter <- neutralCounter + 1

        let mat = CSMat(pow2 qs.Length)
        let mutable controlIndex = 0
        for control in controls do
            controlIndex <- controlIndex + pow2 (qs.Length - control - 1)

        for j in 0..mat.Length-1 do
            if j &&& controlIndex = controlIndex then
                mat.[j,j] <- Complex.Zero

        for (i, j) in g.Mat.Filled() do
            for neutralIndex in neutralIndices do
                let mutable row = controlIndex + neutralIndex
                let mutable column = controlIndex + neutralIndex
                let mutable shift = 1
                for k in 0..gateIndices.Length-1 do
                    if i &&& shift = shift then
                        row <- row + pow2 (qs.Length - gateIndices.[gateIndices.Length-k-1] - 1)
                    if j &&& shift = shift then
                        column <- column + pow2 (qs.Length - gateIndices.[gateIndices.Length-k-1] - 1)
                    shift <- shift <<< 1
                mat.[row,column] <- g.Mat.[i,j]

        let gate = new Gate(Mat = mat)
        gate.Run qs

    let ControlOrGate (gate: Qubits -> unit) (controls: int list) (gateIndices: int list) (qs: Qubits) =
        let g = !< gate qs.[..gateIndices.Length-1]
        let gLen = int (Math.Log(float g.Mat.Length, 2.))
        for gateIndex in gateIndices do
            assert (not (List.contains gateIndex controls))
        let neutralLen = qs.Length - gLen - controls.Length
        let neutralIndices = Array.replicate (pow2 neutralLen) 0
        let mutable neutralCounter = 0
        for i in 0..qs.Length - 1 do
            if not (List.contains i controls || List.contains i gateIndices) then
                let v = pow2 (qs.Length - i - 1)
                for j in pow2 neutralCounter..pow2 (neutralCounter + 1)..neutralIndices.Length-1 do
                    for k in 0..pow2 neutralCounter-1 do
                        neutralIndices.[j+k] <- neutralIndices.[j+k] + v
                neutralCounter <- neutralCounter + 1
        let controlIndices = Array.replicate (pow2 controls.Length - 1) 0
        let mutable controlCounter = 0
        for i in 0..controls.Length-1 do
            let v = pow2 (qs.Length - controls.[i] - 1)
            for j in pow2 controlCounter..pow2 (controlCounter + 1)..controlIndices.Length do
                for k in 0..pow2 controlCounter-1 do
                    controlIndices.[j+k-1] <- controlIndices.[j+k-1] + v
            controlCounter <- controlCounter + 1

        let mat = CSMat(pow2 qs.Length)
        let andControlIndex = controlIndices.[controlIndices.Length-1]

        for j in 0..mat.Length-1 do
            if j &&& andControlIndex > 0 then
                mat.[j,j] <- Complex.Zero

        for (i, j) in g.Mat.Filled() do
            for neutralIndex in neutralIndices do
                for controlIndex in controlIndices do
                    let mutable row = controlIndex + neutralIndex
                    let mutable column = controlIndex + neutralIndex
                    let mutable shift = 1
                    for k in 0..gateIndices.Length-1 do
                        if i &&& shift = shift then
                            row <- row + pow2 (qs.Length - gateIndices.[gateIndices.Length-k-1] - 1)
                        if j &&& shift = shift then
                            column <- column + pow2 (qs.Length - gateIndices.[gateIndices.Length-k-1] - 1)
                        shift <- shift <<< 1
                    mat.[row,column] <- g.Mat.[i,j]

        let gate = new Gate(Mat = mat)
        gate.Run qs

    let ApplyGate (gate: Qubits -> unit) (indices: int list) (qs: Qubits) = 
        let g = !< gate qs.[..indices.Length-1]
        let gLen = int (Math.Log(float g.Mat.Length, 2.))
        let neutralLen = qs.Length - gLen
        let neutralIndices = Array.replicate (pow2 neutralLen) 0
        let mutable neutralCounter = 0
        for i in 0..qs.Length - 1 do
            if not (List.contains i indices) then
                let v = pow2 (qs.Length - i - 1)
                for j in pow2 neutralCounter..pow2 (neutralCounter + 1)..neutralIndices.Length-1 do
                    for k in 0..pow2 neutralCounter-1 do
                        neutralIndices.[j+k] <- neutralIndices.[j+k] + v
                neutralCounter <- neutralCounter + 1

        let mat = CSMat(pow2 qs.Length, true)

        for (i, j) in g.Mat.Filled() do
            for neutralIndex in neutralIndices do
                let mutable row = neutralIndex
                let mutable column = neutralIndex
                let mutable shift = 1
                for k in 0..indices.Length-1 do
                    if i &&& shift = shift then
                        row <- row + pow2 (qs.Length - indices.[indices.Length-k-1] - 1)
                    if j &&& shift = shift then
                        column <- column + pow2 (qs.Length - indices.[indices.Length-k-1] - 1)
                    shift <- shift <<< 1
                mat.[row,column] <- g.Mat.[i,j]

        let gate = new Gate(Mat = mat)
        gate.Run qs

    let numeralize (qs:Qubits) =
        let mutable v = 0
        for q_idx in 0..qs.Length-1 do
            v <- v + (pow2 q_idx) * (qs.[q_idx].Bit.v)
        v

    let ket_numeralize (ket:Ket) =
        let parts = ket.ToString().Split [|'\n'|]
        let mutable max = 0.
        let mutable number = 0
        for i in 2..parts.Length-2 do
            let mutable s = parts.[i]
            s <- s.Trim()
            s <- s.TrimStart [|'('|]
            s <- s.TrimEnd [|')'|]
            let mutable lastE = true
            let mutable temp = 0
            let mutable magnitude = 0.
            for j in 0..s.Length-1 do
                let c = s.Chars(j)
                if (c='-' && not lastE) || c='+' then
                    let float_s = new string (s.ToCharArray().[temp..j-1])
                    let coefficient = float float_s
                    magnitude <- magnitude + coefficient * coefficient
                    temp <- j+1
                lastE <- c = 'e'
            let j = if s.Chars(s.Length-1) = 'i' then s.Length-2 else s.Length-1
            let float_s = new string (s.ToCharArray().[temp..j])
            let coefficient = float float_s
            magnitude <- magnitude + coefficient * coefficient
            if magnitude > max then
                max <- magnitude
                number <- i
        max

    let rec initializeC (nbits:int) (n:int) =
        if nbits = 0 then 
            [] 
        else 
            let rest = initializeC (nbits-1) (n/2)
            (if n % 2 = 0 then Zero else One) :: rest

    let initialize (nbits:int) (n:int) (ket:Ket) = 
        for bit in initializeC nbits n do
            ket.Add bit |> ignore

    /// <summary>
    /// Do QFT (bits are reversed on exit)
    /// </summary>
    /// <param name="qsPE">PE qubits, 0=LSB</param>
    let QFT (qsPE: Qubits) =
        let mutable gates = [||]
        for bit in qsPE.Length-1..-1..0 do
            gates <- gates +| ApplyGate H [bit]
            for ctrl in bit-1..-1..0 do
                let k       = 1+bit-ctrl
                gates <- gates +| ApplyGate (Cgate (R k)) [ctrl;bit]
        CombineGates gates qsPE

    let Add (qs: Qubits) =
        let length = qs.Length / 2
        let mutable gates = [||]
        gates <- gates +| QFT
        for x_idx in length-1 .. -1 .. 0 do
            for y_idx in x_idx .. -1 .. 0 do
                let k = x_idx-y_idx+1
                gates <- gates +| ApplyGate (Cgate (R k)) [length+y_idx; x_idx]
        gates <- gates +| Adj QFT
        CombineGates gates qs

    let AddC (cs: Bit list) (qs: Qubits) =
        let mutable gates = [||]
        gates <- gates +| QFT
        for q_idx in cs.Length-1 .. -1 .. 0 do
            for c_idx in q_idx .. -1 .. 0 do
                let k = q_idx-c_idx+1
                let c = cs.[c_idx]
                if c = One then
                    gates <- gates +| ApplyGate (R k) [q_idx]

        gates <- gates +| Adj QFT
        CombineGates gates qs

    let phaseEstimation (precision:int) (U:CSMat) (qs:Qubits) =
        let register = qs.[..precision-1]
        let phi = qs.[precision..]
        H >< register
        let mutable mat = U
        for k in 0..precision-1 do
            let gate = 
                let name = sprintf "U^%d" (pow2 k)
                new Gate(
                    Name = name,
                    Mat = mat,
                    Draw = sprintf "\\multigate{#%d}{%s}" (phi.Length - 1) name
                )
            mat <- mat * mat
            Cgate gate.Run (List.append [register.[k]] phi)
        Adj QFT register
        M >< register

    let C0gate (gate: Qubits -> unit) (qs: Qubits) =
        let g = !< gate qs.[1..]
        let mat = CSMat(g.Mat.Length * 2, true)
        for i in 0..g.Mat.Length-1 do
            mat.[i+g.Mat.Length, i+g.Mat.Length] <- Complex.One
        for (i, j) in g.Mat.Filled() do
            mat.[i,j] <- g.Mat.[i,j] 
        let cg = new Gate(Mat = mat)
        cg.Run qs

    let C1gate (gate: Qubits -> unit) (qs: Qubits) =
        let g = !< gate qs.[1..]
        let mat = CSMat(g.Mat.Length * 2, true)
        for i in 0..g.Mat.Length - 1 do
            mat.[i, i] <- Complex.One
        for (i, j) in g.Mat.Filled() do
            mat.[i+g.Mat.Length,j+g.Mat.Length] <- g.Mat.[i,j] 
        let cg = new Gate(Mat = mat)
        cg.Run qs

    let C01gate (gate0: Qubits -> unit) (gate1: Qubits -> unit) (qs: Qubits) =
        let g0 = !< gate0 qs.[1..]
        let g1 = !< gate1 qs.[1..]
        let mat = CSMat(g0.Mat.Length * 2, true)
        for (i, j) in g0.Mat.Filled() do
            mat.[i,j] <- g0.Mat.[i,j]
        for (i, j) in g1.Mat.Filled() do
            mat.[i+g0.Mat.Length,j+g0.Mat.Length] <- g1.Mat.[i,j]
        let cg = new Gate(Mat = mat)
        cg.Run qs

    let RAM (cellSize: int) (addressLength: int) (qs: Qubits) =
        let cells = (qs.Length - cellSize - addressLength) / cellSize
        assert (cells > 0)
        let rec RAMSearch (index: int) (n: int) (qs: Qubits) =
            if addressLength = n then
                let idx = index * cellSize
                let mutable gates = [||]
                for i in 0..cellSize-1 do
                    let cx = ControlGate X [idx+i] [qs.Length-cellSize+i]
                    let xc = ControlGate X [qs.Length-cellSize+i] [idx+i]
                    gates <- gates ++| [|cx; xc; cx|]
                CombineGates gates qs
            else
                let gate0 = RAMSearch index (n+1)
                let gate1 = RAMSearch (index + pow2 n) (n+1)
                if index + pow2 n >= cells then
                    C0gate gate0 qs
                else
                    C01gate gate0 gate1 qs
        RAMSearch 0 0 qs

    let phi (alpha: int) (d: int) (qs: Qubits) =
        let aLength = int (Math.Ceiling(Math.Log(float(d+1))))
        let SLength = d
        let rec phiS (ws: int list) (n: int) (qs: Qubits) =
            if SLength = n then
                let mag = Math.Sqrt(float (alpha * ws.Length + 1))
                let v = Math.Sqrt(float alpha)
                let vec = Array.replicate aLength 0.
                vec.[aLength-1] <- 1. / mag
                for w in ws do
                    vec.[w] <- v / mag
                let mat = CSMat(aLength, true)
                for i in 0..vec.Length-1 do
                    for j in 0..aLength-1 do
                        if i = j then
                            mat.[i,j] <- Complex.One * (1. - vec.[i] * vec.[j] * 2.)
                        else
                            mat.[i,j] <- Complex.One * (-vec.[i] * vec.[j] * 2.)
                let gate = new Gate(Mat = mat)
                gate.Run qs
            else
                let gate0 = phiS ws (n+1)
                let gate1 = phiS (n :: ws) (n+1)
                C01gate gate0 gate1 qs
        phiS [] 0 qs

    let P (cell_size: int) (ret: int) (qs: Qubits) =
        ControlGate X [1; cell_size] [ret] qs

    let Ra (n:int) (d:int) (P:Qubits->unit) (qs:Qubits) = 
        let N_length = int(Math.Ceiling(Math.Log(float (n+1), 2.)))
        let d_length = int(Math.Ceiling(Math.Log(float d, 2.)))
        let D_length = int(Math.Ceiling(Math.Log(float (d+1), 2.)))
        let mutable start = 0
        let o = [start .. start]
        start <- start + 1
        let l = [start .. start + N_length - 1]
        start <- start + N_length
        let i = [start .. start + n*N_length - 1]
        start <- start + n * N_length
        let v = [start .. start + n*D_length - 1]
        start <- start + n * D_length
        let H_anc = [start .. start + D_length - 1] //a
        start <- start + D_length
        let H_next = [start .. start + N_length - 1] //j
        start <- start + N_length
        let H_children = [start .. start + d - 1] //S
        start <- start + d
        let w_length = max N_length D_length
        let work = [start .. start + w_length - 1]
        start <- start + w_length
        let ancilla1 = [start .. start]
        start <- start + 1
        let ancilla2 = [start .. start]

        let one_bits = initializeC l.Length 1

        //Step 1: If P(x) is true, return.
        let step1 (otherSteps: Qubits -> unit) (qs: Qubits) =
            CombineGates [|
                ApplyGate P (v ++. ancilla1)
                ApplyGate X ancilla1
                ControlGate otherSteps ancilla1 [0..qs.Length-1]
                ApplyGate X ancilla1
                Adj (ApplyGate P (v ++. ancilla1))
            |] qs

        //Step 2: If l is odd, subtract h((i1, v1), ..., (il−1, vl−1)) from il and swap a with vl.
        let step2 (qs: Qubits) =
            CombineGates [|
                ControlGate X [l.[0]] o
                ControlGate (RAM N_length N_length) o (l ++. i ++. work)
                ControlGate (Adj Add) o (work ++. l)
                ControlGate (RAM N_length N_length) o (l ++. i ++. work)
                ControlGate (RAM D_length N_length) o (l ++. v ++. H_anc)
                ControlGate X [l.[0]] o
            |] qs

        //Step 3: If a!=*, subtract 1 from l. Now l is even and (il+1, vl+1) = (0, *).
        let step3 (qs: Qubits) =
            CombineGates [|
                ApplyGate (Adj (AddC one_bits)) l
                ControlGate (AddC one_bits) H_anc l
                |] qs

        //Step 4: Add h((i1,v1),...,(il,vl)) to j.
        let step4 (qs: Qubits) =
            CombineGates [|
                ApplyGate Add (H_next ++. l)
                ApplyGate (AddC one_bits) H_next
                |] qs

        //Step 5: For each w ∈ [d]:
        //        If P((i1,v1),...,(il,vl),(j,w)) is not false, set S=S U {w}.
        let step5 (qs: Qubits) =
            let mutable gates = [||]
            for w in 0..d-1 do
                gates <- gates ++| [|
                    ApplyGate P (v ++. ancilla2)
                    ControlGate X ancilla2 [H_children.[w]]
                    Adj (ApplyGate P (v ++. ancilla2))|]
            CombineGates gates qs

        //Step 6: If l = 0, perform the operation I − 2|φn,S⟩⟨φn,S| on Hanc.
        //        Otherwise, perform the operation I − 2|φ1,S⟩⟨φ1,S| on Hanc.
        let step6 (qs: Qubits) = 
            ApplyGate (phi 1 d) (H_children ++. H_anc) qs

        //Step 7: Uncompute S and j by reversing steps 5 and 4.
        let step7 (qs: Qubits) =
            CombineGates [|
                Adj step5
                Adj step4
                |] qs

        //Step 8: If a!=*, add 1 to l.
        //        If l is now odd, add h((i1,v1),...,(il−1,vl−1)) to il and swap vl with a. (Now a = * again.)
        let step8 (qs: Qubits) =
            CombineGates [|
                ApplyGate (AddC one_bits) l
                ControlGate (Adj (AddC one_bits)) H_anc l

                ControlGate X [l.[0]] o
                ControlGate (RAM N_length N_length) o (l ++. i ++. work)
                ControlGate Add o (work ++. l)
                ControlGate (RAM N_length N_length) o (l ++. i ++. work)
                ControlGate (RAM D_length N_length) o (l ++. v ++. H_anc)
                ControlGate X [l.[0]] o
                |] qs

        step1 (CombineGates [|step2; step3; step4; step5; step6; step7; step8|]) qs

    let Rb (n:int) (d:int) (P:Qubits->unit) (qs:Qubits) = 
        let N_length = int(Math.Ceiling(Math.Log(float (n+1), 2.)))
        let d_length = int(Math.Ceiling(Math.Log(float d, 2.)))
        let D_length = int(Math.Ceiling(Math.Log(float (d+1), 2.)))
        let mutable start = 0
        let e = [start .. start]
        start <- start + 1
        let l = [start .. start + N_length - 1]
        start <- start + N_length
        let i = [start .. start + n*N_length - 1]
        start <- start + n * N_length
        let v = [start .. start + n*D_length - 1]
        start <- start + n * D_length
        let H_anc = [start .. start + D_length - 1] //a
        start <- start + D_length
        let H_next = [start .. start + N_length - 1] //j
        start <- start + N_length
        let H_children = [start .. start + d - 1] //S
        start <- start + d
        let w_length = max N_length D_length
        let work = [start .. start + w_length - 1]
        start <- start + w_length
        let ancilla1 = [start .. start]
        start <- start + 1
        let ancilla2 = [start .. start]

        let one_bits = initializeC l.Length 1

        //Step 1: If P(x) is true or l=0, return.
        let step1 (otherSteps: Qubits -> unit) (qs: Qubits) =
            CombineGates [|
                ApplyGate P (v ++. ancilla1)
                ApplyGate X ancilla1
                ControlGate (ControlOrGate otherSteps l [0..qs.Length-1]) ancilla1 [0..qs.Length-1]
                ApplyGate X ancilla1
                Adj (ApplyGate P (v ++. ancilla1))
            |] qs

        //Step 2: If l is even, subtract h((i1, v1), ..., (il−1, vl−1)) from il and swap a with vl.
        let step2 (qs: Qubits) =
            CombineGates [|
                ApplyGate X [l.[0]]
                ControlGate X [l.[0]] e
                ControlGate X e [l.[0]]
                ControlGate (RAM N_length N_length) e (l ++. i ++. work)
                ControlGate (Adj Add) e (work ++. l)
                ControlGate (RAM N_length N_length) e (l ++. i ++. work)
                ControlGate (RAM D_length N_length) e (l ++. v ++. H_anc)
                ControlGate X e [l.[0]]
                ControlGate X [l.[0]] e
                ApplyGate X [l.[0]]
            |] qs

        //Step 3: If a!=*, subtract 1 from l. Now l is odd and (il+1, vl+1) = (0, *).
        let step3 (qs: Qubits) =
            CombineGates [|
                ApplyGate (Adj (AddC one_bits)) l
                ControlGate (AddC one_bits) H_anc l
                |] qs

        //Step 4: Add h((i1,v1),...,(il,vl)) to j.
        let step4 (qs: Qubits) =
            CombineGates [|
                ApplyGate Add (H_next ++. l)
                ApplyGate (AddC one_bits) H_next
                |] qs

        //Step 5: For each w ∈ [d]:
        //        If P((i1,v1),...,(il,vl),(j,w)) is not false, set S=S U {w}.
        let step5 (qs: Qubits) =
            let mutable gates = [||]
            for w in 0..d-1 do
                gates <- gates ++| [|
                    ApplyGate P (v ++. ancilla2)
                    ControlGate X ancilla2 [H_children.[w]]
                    Adj (ApplyGate P (v ++. ancilla2))|]
            CombineGates gates qs

        //Step 6: perform the operation I − 2|φ1,S⟩⟨φ1,S| on Hanc.
        let step6 (qs: Qubits) = 
            ApplyGate (phi 1 d) (H_children ++. H_anc) qs

        //Step 7: Uncompute S and j by reversing steps 5 and 4.
        let step7 (qs: Qubits) =
            CombineGates [|
                Adj step5
                Adj step4
                |] qs

        //Step 8: If a!=*, add 1 to l.
        //        If l is now even, add h((i1,v1),...,(il−1,vl−1)) to il and swap vl with a. (Now a = * again.)
        let step8 (qs: Qubits) =
            CombineGates [|
                ApplyGate (AddC one_bits) l
                ControlGate (Adj (AddC one_bits)) H_anc l

                ApplyGate X [l.[0]]
                ControlGate X [l.[0]] e
                ControlGate X e [l.[0]]
                ControlGate (RAM N_length N_length) e (l ++. i ++. work)
                ControlGate Add e (work ++. l)
                ControlGate (RAM N_length N_length) e (l ++. i ++. work)
                ControlGate (RAM D_length N_length) e (l ++. v ++. H_anc)
                ControlGate X e [l.[0]]
                ControlGate X [l.[0]] e
                ApplyGate X [l.[0]]
                |] qs

        step1 (CombineGates [|step2; step3; step4; step5; step6; step7; step8|]) qs

    [<LQD>]
    let __UserSample() =
        let address_length = 2
        let cell_size = 1
        let memory_length = pow2 address_length
        let circ = Circuit.Compile (RAM cell_size address_length) (Ket(address_length + (memory_length + 1) * cell_size).Qubits)
        for address in 0..memory_length - 1 do
            let ket = Ket()
            initialize address_length address ket
            for i in 1..memory_length * cell_size do
                ket.Add Zero |> ignore
            for i in 1..cell_size do
                ket.Add One |> ignore
            let memory = ket.Qubits.[address_length..address_length+memory_length*cell_size-1]
            let ancilla = ket.Qubits.[address_length+memory_length*cell_size..]
            circ.Run ket.Qubits
            M >< ket.Qubits
            show "Address: %d" address
            show "-- Memory"
            for i in 0..memory_length-1 do
                show "."
                for j in 0..cell_size-1 do
                    show "%d" memory.[i*cell_size+j].Bit.v
            show "-- Ancilla"
            show "."
            for i in 0..cell_size-1 do
                show "%d" ancilla.[i].Bit.v
            show "----------"
            show ""

        //let X_mat = (!< X (Ket(1).Qubits)).Mat

        //let g0 = new Gate(Mat = X_mat.Kron(2))
        //let cg0 = !< (Cgate g0.Run) (Ket(3).Qubits)
        //show "%s" (cg0.Mat.ToString())

        //let g1 = new Gate(Mat = CSMat(2).Kron(X_mat))
        //let cg1 = !< (Cgate g1.Run) (Ket(3).Qubits)
        //show "%s" (cg1.Mat.ToString())

        //show "HELLO"
        //let precision = 4
        //let psi_length = 2
        //show "%d" (precision+psi_length)
        //let U = CSMat(pow2 psi_length)
        //let ket = Ket(precision+psi_length)
        //let circ = Circuit.Compile (phaseEstimation precision U) ket.Qubits
        //let circ = circ.GrowGates(ket)
        //circ.Run ket.Qubits
        //for k in 0..precision-1 do
            //show "%d" ket.Qubits.[k].Bit.v
        //show "%s" (ket.Qubits.[precision].ToString())
        //circ.Dump()
        //circ.RenderHT("phaseEstimation", 0)

        //let bits = 3
        //for a in 0..pow2 bits - 1 do
            //for b in 0..pow2 bits - 1 do
                //let ket = Ket()
                //initialize bits a ket
                //initialize bits b ket
                //subtract ket.Qubits
                ////let circ = Circuit.Compile add ket.Qubits
                ////circ.Run ket.Qubits
                ////circ.RenderHT("add", 1)
                ////show "%f" (ket_numeralize ket)
                //M >< ket.Qubits
                //let c = numeralize ket.Qubits.[..bits-1]
                //show "%d - %d = %d" a b c

        //let ket = Ket(2)
        //let g = !< (ControlGate X [1] 0) ket.Qubits
        //for i in 0..g.Mat.Length - 1 do
            //for j in 0..g.Mat.Length - 1 do
            //    printf "%s " (g.Mat.[i,j].ToString())
            //printf "\n"

module Main =
    open App

    /// <summary>
    /// The main entry point for Liquid.
    /// </summary>
    [<EntryPoint>]
    let Main _ =
        RunLiquid ()
