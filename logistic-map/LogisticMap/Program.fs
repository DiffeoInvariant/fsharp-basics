
open Argu

let LogisticMap (r: double) (x: double) = 
    r * x * (1.0 - x)

let MapAverage1D map_fn (pars: double) (initial_state: double) (niter: int) (ndiscard: int) = 
    let mutable total = 0.0
    let mutable state = initial_state
    for i in 0 .. ndiscard do
        state <- map_fn pars state 

    for i in 0 .. niter do
        state <- map_fn pars state 
        total <- total + state 

    total / (double niter)

let MapAverage1DCumulative map_fn (pars: double) (niter_per: int) (ndiscard_per: int) (initial_states: double array) =
    let mutable totmean = 0.0
    for i in 0 .. initial_states.Length - 1 do 
        totmean <- totmean + (MapAverage1D map_fn pars (double initial_states.[i]) niter_per ndiscard_per)

    totmean / (double initial_states.Length)

type CLIArgs = 
    | R of r:double 
    | Niter of n:int 
    | Ndiscard of d:int 
    | Nstates of ns:int

    interface IArgParserTemplate with 
        member this.Usage = 
            match this with
            | R _ -> "Logistic map r parameter"
            | Niter _ -> "Number of iterations per initial state"
            | Ndiscard _ -> "Number of burn-in iterations per initial state"
            | Nstates _ -> "Number of initial states between 0 and 1"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArgs>(programName="Chaos.exe")
    let options = parser.Parse argv
    let r: double = options.GetResult (R, defaultValue=1.01)
    let niter: int = options.GetResult (Niter, defaultValue=2000)
    let ndiscard = options.GetResult (Ndiscard, defaultValue=1000)
    let nstates = options.GetResult (Nstates, defaultValue=50)
    let initial_states = [| for i in 0 .. nstates -> (double i)/ (double nstates)|]
    let hypothesis = (r - 1.0) / r
    let hypothesis_2 = (r * r - 1.0) / (r ** 3.0)
    let estimate = MapAverage1DCumulative LogisticMap r (int niter) (int ndiscard) initial_states
    printfn "Hypothesis (r^2 - 1) / r^3 is %g" hypothesis_2
    printfn "Hypothesis is average value is %g; estimate from %i initial states, %i iterations (plus %i discarded) per state: %g" hypothesis initial_states.Length niter ndiscard estimate
    0 