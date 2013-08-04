module NominalScheduleTests

open System
open Xunit
open FsUnit.Xunit
open NominalScheduleGenerator

let initialNominal = 1000000.0
let residual       =  500000.0
let guessPayment   =  200000.0
let dates          = [
                         new DateTime(2013,01,01)
                         new DateTime(2013,02,01)
                         new DateTime(2013,03,01)
                         new DateTime(2013,04,01)
                         new DateTime(2013,05,01)
                         new DateTime(2013,06,01)
                     ]
let r     = 0.1 // Assume an interest rate of 10% SIMP
let basis = 365 // Standard ZA Money Market Basis

[<Fact>] 
let ``should be able to get the last balance for amortizing loan``() =
    let finalNominalBalance = getFinalNominalBalance initialNominal r basis (dates:DateTime list) guessPayment
    finalNominalBalance |> should (equalWithin 1e-7) 307147.6192407597

[<Fact>] 
let ``should be able to find payment for amortizing loan``() =
    let requiredPayment = findPaymentForAmortizingLoan initialNominal 0.0 r basis (dates:DateTime list)  
    requiredPayment |> should (equalWithin 1e-7) 252020.140557893

[<Fact>] 
let ``should be able to find payment for amortizing loan with residual``() =
    let requiredPayment = findPaymentForAmortizingLoan initialNominal residual r basis (dates:DateTime list) 
    requiredPayment |> should (equalWithin 1e-7) 167337.503774834


//----------------------------------------------------------------------------------------------------------
// list based schedule generator ... this is a better design.
//----------------------------------------------------------------------------------------------------------
[<Fact>] 
let ``should be able to get the last balance for amortizing loan - list``() =
    let schedule = getNominalSchedule initialNominal r basis (dates:DateTime list) guessPayment
    schedule.Head.NominalBalance |> should (equalWithin 1e-7) 307147.6192407597
    
[<Fact>] 
let ``should be able to find payment for amortizing loan with residual - list``() =
    let schedule = getAmortizingLoanSchedule initialNominal residual r basis (dates:DateTime list)
    //schedule |> List.iter (fun item -> printfn "%A" item)
    schedule.Head.Payment |> should (equalWithin 1e-7) 167337.503774834
    schedule.Head.NominalBalance |> should (equalWithin 1e-7) residual

