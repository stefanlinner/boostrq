boostrq <- function(y, X, mstop = 100, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn") {

  betahat <- vector("numeric", length = dim(X)[2] - 2)
  betahat[1] <- quantile(y, offset)
  appearances <- vector("integer", length = mstop)

  coefpath <- lapply(1:(dim(X)[2] - 1), function(x){

    path <- matrix(data = 0, nrow = 2, ncol = mstop)
    rownames(path) <- c("Intercept", "b")
    path

  })

  names(coefpath) <- paste("x", 1:(dim(X)[2] - 1), sep = "")

  risk <- vector("numeric", length = dim(X)[2] - 1)

  for(m in seq_len(mstop)) {

    f <- as.vector(X %*% betahat)
    q.ngradient <- ngradient(y = y, f = f, tau = tau)

    qr.res <- lapply(X = 1:(dim(X)[2] - 1), FUN = function(x) {
      qreg <- rq(ngradient ~ X[, x + 1], tau = tau, method = method)

      risk[x] <<- loss(y = q.ngradient, f = qreg$fitted.values, tau = tau)

      coef(qreg)
    })

    if(sum(risk == min(risk)) > 1){
      warning(paste("Warning: There is no unique best base learner in iteration", m))
    }

    betahat[1] <- betahat[1] + qr.res[[which.min(risk)]][1] * nu
    betahat[which.min(risk) + 1] <- betahat[which.min(risk) + 1] + qr.res[[which.min(risk)]][2] * nu

    if(abs(round(qr.res[[which.min(risk)]][2], 10)) > 0){
      appearances[m] <- which.min(risk)
    } else {
      appearances[m] <- 0
    }

  }

  list(coefficients = betahat, covariates = appearances, coefpath = coefpath)

}
