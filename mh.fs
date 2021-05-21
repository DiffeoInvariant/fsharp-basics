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
    member this.pi = target 
    member this.qpdf = NormalPDF q_mu q_sd
    member this.gen = new Random()
    member this.qmu = q_mu 
    member this.qstd = q_sd
    let mutable curr_sample = q_mu 

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


[<EntryPoint>]
let main args = 
    let mu_t = try double <| args.[0] with _ -> -0.5
    let target = NormalPDF mu_t 1.0
    let mu_q = try double <| args.[2] with _ -> 1.0
    let sampler = IndependentNormalMHSampler(target, mu_q, 5.0)
    let mutable mu = 0.0
    let n = try int <| args.[1] with _ -> 100
    for i in 0 .. n do
        let sample = sampler.Sample()
        mu <- mu + sample 

    mu <- mu / (double(n))
    System.Console.WriteLine("The sample mean is {0:f9}", mu)
    0
