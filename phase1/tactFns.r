# Title: tactFns.r
# Objective: central script containing most tactical acquisition and turn fns
# Created by: NR
# Additional editors:


tactAcq01 <- function(eval.obj){
  eval.obj$threshold <- with(eval.obj,Open-2*med.daily.sd) # maybe replace open w/predictor from a mod
  eval.obj$flag.buy <- with(eval.obj,Last<threshold)
  eval.obj <- eval.obj[with(eval.obj,order(cvhat,decreasing=T)),]
  return(eval.obj)
}

tactTurn01 <- function(eval.obj){
  eval.obj$sell_threshold <- with(eval.obj,price+1*med.daily.sd)
  eval.obj$stop_loss <- with(eval.obj, Open-3*med.daily.sd)
  eval.obj$flag_sell <- with(eval.obj,Last>sell_threshold | Last < stop_loss)
  return(eval.obj)
}
