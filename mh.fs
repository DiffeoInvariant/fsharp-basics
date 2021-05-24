open Argu 
open System 

type Univariate1D = double -> double
type Bivariate1D = double -> double -> double

let DependentMHRatio pi q x y =
    let denom = pi x * q y x
    if denom = 0.0 then
        1.0
    else 
        let num = pi y * q x y
        Math.Min(1.0, num / denom)

let IndependentMHRatio pi q x y =
    let denom = pi x * q y
    if denom = 0.0 then
        1.0
    else 
        let num = pi y * q x
        Math.Min(1.0, num / denom)

// takes in two uniform random numbers, a mean, and a standard deviation
let BoxMullerTransform u1 u2 mu sd =
    let r = Math.Sqrt(-2.0 * Math.Log(u1))
    let theta = 2.0 * Math.PI * u2 
    (sd*(r * Math.Cos(theta) + mu), sd*(r * Math.Sin(theta) + mu))

// proportional to a Normal pdf
let NormalPDF mu std x = 
    Math.Exp(-Math.Pow(x - mu, 2.0)/Math.Pow(std, 2.0))




type IndependentNormalMHSampler(target: Univariate1D, q_mu: double, q_sd: double) =
    let mutable curr_sample = q_mu  

    member this.pi = target 
    member this.qpdf = NormalPDF q_mu q_sd
    member this.gen = new Random()
    member this.qmu = q_mu 
    member this.qstd = q_sd
    

    member this.Sample() = 
        let mutable accepted = false
        let mutable u1 = this.gen.NextDouble() 
        let mutable u2 = this.gen.NextDouble()  
        let step curr cand ui = 
            let alpha = IndependentMHRatio this.pi this.qpdf curr cand 
            if ui < alpha then
                (cand, true)
            else 
                (curr, false)
        let mutable curr = curr_sample
        while not accepted do
            let q1, q2 = BoxMullerTransform u1 u2 this.qmu this.qstd 
            let U_i = this.gen.NextDouble()
            // we get two candidate samples at a time, so we should try another step if the first fails
            let state, accept_this = step curr q1 U_i 
            curr <- state 
            accepted <- accept_this 
            u1 <- this.gen.NextDouble()
            u2 <- this.gen.NextDouble()

        curr_sample <- curr
        curr 

(*
let rec parseArgs args existing_options = 
    match args with 
    | [] -> 
        existing_options

    | "--target-mean"::xs ->
        match xs with 
        | x::xxs ->
            match x with 
            | :? double as x -> 
                let options = {existing_options with mu_t = x}
                parseArgs xxs options
            | _ ->
                failwith "--target-mean argument must be convertible to double!"
        | _ ->
                failwith "--target-mean argument must be convertible to double!"

    | "--candidate-mean"::xs ->
        match xs with 
        | x::xxs -> 
            match x with 
            | :? double as x ->
                let options = {existing_options with mu_q = x}
                parseArgs xxs options
            | _ ->
                failwith "--candidate-mean argument must be convertible to double!"
        | _ ->
            failwith "--candidate-mean argument must be convertible to double!"

    | "--samples"::xs ->
        match xs with 
        | x::xxs  ->
            match x with 
            | :? int as n ->
                let options = {existing_options with n = x}
                parseArgs xxs options 
            | _ ->
                failwith "--samples argument must be convertible to int!"
        | _ ->
            failwith "--samples argument must be convertible to int!"
    
    | "--burn-in"::xs ->
        match xs with 
        | x::xxs ->
            match x with 
            | :? int as b ->
                let options = {existing_options with b = x}
                parseArgs xxs options 
            | _ ->
                failwith "--burn-in argument must be convertible to int!"   
        | _ ->
            failwith "--burn-in argument must be convertible to int!"
     
    | x::xs ->
        eprintfn "Unrecognized option '%s'" x 
        parseArgs xs existing_options
*)

type CliArgs = 
    | Target_Mean of mu_t:double
    | Candidate_Mean of mu_q:double 
    | Samples of n:int 
    | Burn_In of b:int

    interface IArgParserTemplate with 
        member this.Usage = 
            match this with 
            | Target_Mean _ -> "Target mean."
            | Candidate_Mean _ -> "Independent Normal andidate mean."
            | Samples _ -> "How many samples?"
            | Burn_In _ -> "How many burn-in samples?"


[<EntryPoint>]
let main args = 
    //let mu_t = try double <| args.[0] with _ -> -0.5
    //let target = NormalPDF mu_t 1.0
    //let mu_q = try double <| args.[1] with _ -> 1.0
    let defaults = {
        mu_t = -0.5;
        mu_q = 1.0;
        n = 100;
        b = 100;
    }
    let parser = ArgumentParser.Create<CliArgs>(programName="mh.exe")
    let options = parser.Parse args  
    let target = NormalPDF (options.GetResult (Target_Mean, defaultValue=-0.5)) 1.0
    let sampler = new IndependentNormalMHSampler(target, (options.GetResult (Candidate_Mean, defaultValue=1.0)), 5.0)
    let mutable mu = 0.0
    //let n = try int <| args.[2] with _ -> 100
    //let b = try int <| args.[3] with _ -> 100
    for i in 0 .. (options.GetResult (Burn_In, defaultValue=100)) do
        ignore <| sampler.Sample() 

    for i in 0 .. (options.GetResult (Samples, defaultValue=100)) do
        let sample = sampler.Sample()
        mu <- mu + sample 

    mu <- mu / (double(n))
    System.Console.WriteLine("The sample mean is {0:f9}", mu)
    0
