cumulative_tab <- function (x, data = NULL) 
{
  res <- as.data.frame(unclass(x)[c("time", "n.risk", 
                                             "n.event", "n.censor")])
  if (inherits(x, "survfitms")) {
    surv <- 1-x$prev
    upper <- 1-x$upper
    lower <- 1-x$lower
    res <- cbind(res, surv = c(surv), std.err = c(x$std.err), 
                 upper = c(upper), lower = c(lower))
    res$state <- rep(x$states, each = nrow(surv))
  }
  else {
    if (is.matrix(x$surv)) {
      ncurve <- ncol(x$surv)
      res <- data.frame(time = rep(x$time, ncurve), n.risk = rep(x$n.risk, 
                                                                 ncurve), n.event = rep(x$n.event, ncurve), n.censor = rep(x$n.censor, 
                                                                                                                           ncurve))
      res <- cbind(res, surv = .flat(1-x$surv), std.err = .flat(1-x$std.err), 
                   upper = .flat(1-x$upper), lower = .flat(1-x$lower))
      res$strata <- as.factor(rep(colnames(1-x$surv), each = nrow(1-x$surv)))
    }
    else res <- cbind(res, surv = 1-x$surv, std.err = 1-x$std.err, 
                      upper = 1-x$upper, lower = 1-x$lower)
  }
  if (!is.null(x$strata)) {
    data <- .get_data(x, data = data)
    res$strata <- rep(names(x$strata), x$strata)
    res$strata <- .clean_strata(res$strata, x)
    variables <- .get_variables(res$strata, x, data)
    for (variable in variables) res[[variable]] <- .get_variable_value(variable, 
                                                                       res$strata, x, data)
  }
  structure(res, class = c("data.frame", "surv_summary"))
  attr(res, "table") <- as.data.frame(summary(x)$table)
  res
}
