"mcChi" <-
function(Y, R0, eps = 0.000001)
  {
    rsum <- rowSums(Y)
    csum <- colSums(Y)
    tot <- sum(rsum)
    Kn <- csum / sum(csum)
    Keps <- Kn * (Kn > eps) + eps * (Kn < eps)
    Q <- diag(1 / rsum) %*% Y %*% diag(1 / Keps) - 1
    .R0 <- R0 / sum(R0)
    Ychi <- diag(sqrt(.R0)) %*% Q %*% diag(sqrt(Kn))
    rownames(Ychi) <- rownames(Y)
    colnames(Ychi) <- colnames(Y)
    retval <- list(Ychi = Ychi, Kn = Kn)
    class(retval) <- "mcChi"
    return(retval)
  }

