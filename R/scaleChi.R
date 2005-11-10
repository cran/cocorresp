"scaleChi" <-
function(Y, Kn, R0, eps = 0.000001)
  {
    R <- as.matrix(rowSums(Y))
    R0 <- as.matrix(R0)
    Keps <- Kn * (Kn > eps) + eps * (Kn < eps)
    Yr <- diag(1 / R) %*% Y %*% diag(1 / Keps) - 1
    Yr <- diag(sqrt(R0)) %*% Yr %*% diag(sqrt(Kn))
    return(Yr)
  }

