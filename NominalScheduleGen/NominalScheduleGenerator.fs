module NominalScheduleGenerator

open System

type paymentScheduleItem = {
        Date                : DateTime
        NominalBalance      : float
        Payment             : float
        Interest            : float
        PrincipleRedpemtion : float 
    }
type nominalPaymentSchedule = paymentScheduleItem list 

///--------------------------------------------------------
/// Get the final nominal balance for an amortizing loan
/// with the given residual.  Note that this returns the 
/// schedule in reverse order so that the last item in 
/// it is at the head of the returned list.
///--------------------------------------------------------
let getNominalSchedule (initialNominal:float) 
                       (rate:float) 
                       (basis:int)
                       (dates:DateTime list)
                       (payment:float) =
    dates.Tail // skip the first one (tail skips the first entry in a list, that being the head, leaving it's tail)
    |> List.fold 
        (fun (schdl:nominalPaymentSchedule) date -> 
                let previous            = schdl.Head
                let tau                 = (float (date - previous.Date).Days)/(float basis) 
                let interest            = tau * previous.NominalBalance
                let newNominalBalance   = previous.NominalBalance + interest - payment
                let principleRedemption = previous.NominalBalance - newNominalBalance 
                let nextSchedulItem     = { Date = date; NominalBalance = newNominalBalance; Payment = payment; Interest = interest; PrincipleRedpemtion = principleRedemption }
                nextSchedulItem::schdl)           
        [ { Date                = dates.Head      // Prime the schedule with the initial entry in to the list.  This is the state for the fold.
            NominalBalance      = initialNominal
            Payment             = 0.0
            Interest            = 0.0
            PrincipleRedpemtion = 0.0 } ]


///--------------------------------------------------------
/// Get an amortizing loan schedule for the given initial  
/// nominal, residual, interest rate (SIMP), MM basis and 
/// payment dates.
///--------------------------------------------------------
let getAmortizingLoanSchedule
        (initialNominal:float) 
        (residual      :float)
        (rate          :float)
        (basis         :int) 
        (dates         :DateTime list) =

    let getNominalSchedule = getNominalSchedule initialNominal rate basis dates                                  // note the partial application here.
    let getFinalBalanceFor = getNominalSchedule >> (fun (sch:nominalPaymentSchedule) -> sch.Head.NominalBalance) // note the function composition here.
    let noPayments         = float (dates.Length-1)
    let p1                 = initialNominal/noPayments // Get 2 points on the line.
    let p2                 = p1*1.5                    // and use that to solve for required payment. Actual values don't matter
    let finalBalance1      = getFinalBalanceFor p1
    let finalBalance2      = getFinalBalanceFor p2
    let m                  = (p2-p1)/(finalBalance2-finalBalance1)
    let requiredPayment    = p1 - m*(finalBalance1 - residual)
    getNominalSchedule requiredPayment  // Generate the final schedule.





//------------------------------------------------------------------------------------------------------------------------
// Initial attempt 
// 
// This works but its not a great design.  The design above is quite a bit nicer.
//------------------------------------------------------------------------------------------------------------------------

type private accum = { 
    mutable NominalBalance : float 
    mutable PreviousDate   : DateTime 
    }

///--------------------------------------------------------
/// Get the final nominal balance for an amortizing loan
/// with the given residual
///--------------------------------------------------------
let getFinalNominalBalance (initialNominal:float) 
                           (rate:float) 
                           (basis:int)
                           (dates:DateTime list)
                           (payment:float) =
    let final = 
        dates.Tail
        |> List.fold 
            (fun (acc:accum) date -> 
                    let tau                = (float (date - acc.PreviousDate).Days)/(float basis) 
                    let accrualFactor      = 1.0 + tau
                    let nominalAndInterest = accrualFactor * acc.NominalBalance 
                    let newNominalBalance  = nominalAndInterest - payment
                    acc.PreviousDate       <- date
                    acc.NominalBalance     <- newNominalBalance
                    acc) 
            { NominalBalance = initialNominal; PreviousDate = dates.Head }
    final.NominalBalance


///--------------------------------------------------------
/// Find the fixed payment required for an amortizing loan
/// with the given residual
///--------------------------------------------------------
let findPaymentForAmortizingLoan
        (initialNominal:float) 
        (residual      :float)
        (rate          :float)
        (basis         :int) 
        (dates         :DateTime list) =

    let getFinalBalanceFor = getFinalNominalBalance initialNominal rate basis dates // Note the partial appliation here ... lovelly.
    let noPayments         = float (dates.Length-1)
    let p1                 = initialNominal/noPayments // Get 2 points on the line.
    let p2                 = p1*1.5                    //
    let finalBalance1      = getFinalBalanceFor p1
    let finalBalance2      = getFinalBalanceFor p2
    let m                  = (p2-p1)/(finalBalance2-finalBalance1)
    let requiredPayment    = p1 - m*(finalBalance1 - residual)
    requiredPayment
  