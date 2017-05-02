#' fit all possible regressions
#' 
#' This function runs a linear regression for all combinations of variables. It
#' calculates a number of different comparative metrics
#' 
#' @param dat matrix or dataframe containing all variables. The response variable
#' must be named 'y' and the predictor variables as 'x1', 'x2', ... 'xk'.
#' @param k number of predictor variables
#' @return dataframe with a row for each model and columns with the model formula,
#' df.residual, p, SSE, MSE, R2, R2.adj, AIC, AICc, and BIC.
#' @keywords regression, multiple, all, possible
#' 
all.possible.regressions <- function(dat, k){
  n <- nrow(dat)
  regressors <- paste("x", 1:k, sep="")
  lst <- rep(list(c(T, F)), k)
  regMat <- expand.grid(lst);
  names(regMat) <- regressors
  formular <- apply(regMat, 1, function(x)
    as.character(paste(c("y ~ 1", regressors[x]), collapse="+")))
  allModelsList <- apply(regMat, 1, function(x)
    as.formula(paste(c("y ~ 1", regressors[x]),collapse=" + ")) )
  allModelsResults <- lapply(allModelsList,
                             function(x, data) lm(x, data=data), data=dat)
  n.models <- length(allModelsResults)
  extract <- function(fit) {
    df.sse <- fit$df.residual
    p <- n - df.sse -1
    sigma <- summary(fit)$sigma
    MSE <- sigma^2
    R2 <- summary(fit)$r.squared
    R2.adj <- summary(fit)$adj.r.squared
    sse <- MSE*df.sse
    aic <- n*log(sse) + 2*(p+2)
    aicc <- (n*log(sse) + 2*(p+2)) + (2*p*(p+1))/(n-p-1)
    bic <- n*log(sse) + log(n)*(p+2)
    out <- data.frame(df.sse=df.sse, p=p, SSE=sse, MSE=MSE,
                      R2=R2, R2.adj=R2.adj, AIC=aic, AICc=aicc, BIC=bic)
    return(out)
  }
  result <- lapply(allModelsResults, extract)
  result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T))
  result <- cbind(formular, result)
  rownames(result) <- NULL
  colnames(result) <- c("model", "df.sse", "p", "SSE", "MSE", "R2",
                        "R2.adj", "AIC", "AICc", "BIC")
  return(result)
}
