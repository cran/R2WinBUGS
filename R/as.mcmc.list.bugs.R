## Function as.mcmc.list.bugs contributed by Mike Meredith, 12 Feb 2009:
as.mcmc.list.bugs <- function(x, ...) {
    if(!inherits(x, "bugs")) 
        stop("Method as.mcmc.list.bugs() is only intended for bugs objects.")
    if(dim(x$sims.array)[2] != x$n.chains)
        stop("Inconsistancy in bug object regarding the number of chains.")
    mclis <- vector("list", x$n.chains)
    strt <- x$n.burnin%/%x$n.thin + 1
    end <- x$n.iter%/%x$n.thin
    ord <- order(dimnames(x$sims.array)[[3]])
    if(end - strt + 1 < nrow(x$sims.array[,1,ord])) end <- end + 1
    for(i in 1:x$n.chains) {
        tmp1 <- x$sims.array[,i,ord]
        rownames(tmp1) <- strt:end
        mclis[[i]] <- mcmc(tmp1, strt, end, 1)
    }
    as.mcmc.list(mclis)
} 
