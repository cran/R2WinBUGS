print.bugs <- function (x, digits.summary = 1, ...){
    cat ('Inference for Bugs model at \"', x$model.file, "\"\n ",
         x$n.chains, " chains, each with ", x$n.iter, " iterations (first ",
         x$n.burnin, " discarded)", sep="")
    if (x$n.thin > 1) cat (", n.thin =", x$n.thin)
    cat ("\n n.sims =", x$n.sims, "iterations saved\n")
    print (round (x$summary, digits.summary), ...)
    if (!is.null(x$DIC)){
      cat (" pD =", round(x$pD, 1), "and DIC =", round(x$DIC, 1),
           "(using the rule, pD = var(deviance)/2)\n")
      if(x$n.chains > 1){
        cat("\n For each parameter, n.eff is a crude measure of effective sample size,")
        cat("\n and Rhat is the potential scale reduction factor (at convergence, Rhat=1).")
      }
      cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n") 
    }
}
