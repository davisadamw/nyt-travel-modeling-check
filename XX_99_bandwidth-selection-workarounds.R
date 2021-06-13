# create the model matrix to hand over to the cross validation routine 
# ... adapted from gwr.sel 
# ... for simplicity here, also let's just keep model data and coords separate
# ... this is bad practice generally, but good practice would mean refactoring this all over to sf
gwr_sel_mm_create <- function(formula, data = list()) {
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  dp.n <- length(model.extract(mf, "response"))
  
  # weights won't be used here, so we'll just make a ones vector of the appropriate length
  weights <- rep(as.numeric(1), dp.n)
  
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  return(list(x = x, y = y, weights = weights))
}

# function to grab the cv score from a set of inputs
compute_cv_score <- function(bandwidth, y, x, coords, weights, longlat = TRUE, gweight = gwr.Gauss) {
  spgwr:::gwr.cv.f(bandwidth = bandwidth,
                   y = y,
                   x = x,
                   coords = coords,
                   weights = weights,
                   longlat = longlat,
                   gweight = gweight,
                   verbose = FALSE) 
}



gwr_sel_broken <- function (formula, data = list(), coords, adapt = FALSE, gweight = gwr.Gauss, 
                            verbose = TRUE, longlat = NULL, 
                            tol = .Machine$double.eps^0.25, show.error.messages = FALSE) 
{

  # end model prep, start optimization
  if (!adapt) {
    bbox <- cbind(range(coords[, 1]), range(coords[, 2]))
    difmin <- spDistsN1(bbox, bbox[2, ], longlat)[1]
    if (any(!is.finite(difmin))) 
      difmin[which(!is.finite(difmin))] <- 0
    beta1 <- difmin/1000
    beta2 <- difmin
    opt <- optimize(gwr.cv.f, lower = beta1, upper = beta2, 
                    maximum = FALSE, y = y, x = x, coords = coords, 
                    gweight = gweight, verbose = verbose, longlat = longlat, 
                    RMSE = FALSE, weights = weights, show.error.messages = show.error.messages, 
                    tol = tol)
    bdwt <- opt$minimum
    res <- bdwt
  }
  else {
    beta1 <- 0
    beta2 <- 1
    opt <- optimize(gwr.cv.adapt.f, lower = beta1, upper = beta2, 
                    maximum = FALSE, y = y, x = x, coords = coords, 
                    gweight = gweight, verbose = verbose, longlat = longlat, 
                    RMSE = FALSE, weights = weights, show.error.messages = show.error.messages, 
                    tol = tol)
    q <- opt$minimum
    res <- q
  }
  if (isTRUE(all.equal(beta2, res, tolerance = .Machine$double.eps^(1/4)))) 
    warning("Bandwidth converged to upper bound:", beta2)
  res
}
