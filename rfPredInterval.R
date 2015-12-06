#function (rf, newdata, calibrate = TRUE, used.trees = NULL) 
#{

  used.trees = NULL
  calibrate = TRUE
  if (is.null(rf$inbag)) {
    stop("Random forest must be trained with keep.inbag = TRUE")
  }
  if (length(levels(factor(colSums(rf$inbag)))) > 1) {
    stop("The keep.inbag field must store the number of times each observation was used")
  }
  if (is.null(used.trees)) {
    used.trees = 1:rf$ntree
  }
  no.replacement = (max(rf$inbag) == 1)
  B = length(used.trees)
  n = length(rf$y)
  s = sum(rf$inbag)/rf$ntree
  predictions = predict(rf, newdata, predict.all = TRUE)
  pred = predictions$individual[, used.trees]
  class(pred) = "numeric"
  y.hat = rowMeans(pred)
  pred.centered = pred - rowMeans(pred)
  N = Matrix::Matrix(rf$inbag[, used.trees], sparse = TRUE)
  N.avg = Matrix::rowMeans(N)
  if (B^2 > n * nrow(newdata)) {
    C = Matrix::tcrossprod(N, pred.centered) - Matrix::Matrix(N.avg, 
                                                              nrow(N), 1) %*% Matrix::Matrix(rowSums(pred.centered), 
                                                                                             1, nrow(pred.centered))
    raw.IJ = Matrix::colSums(C^2)/B^2
  }
  else {
    NTN = Matrix::crossprod(N, N)
    NTNPT_T = Matrix::tcrossprod(pred.centered, NTN)
    T1 = Matrix::rowSums(pred.centered * NTNPT_T)
    RS = rowSums(pred.centered)
    NbarTN = Matrix::crossprod(N.avg, N)
    T2 = RS * Matrix::tcrossprod(NbarTN, pred.centered)
    T3 = sum(N.avg^2) * RS^2
    raw.IJ = as.numeric(T1 - 2 * T2 + T3)/B^2
  }
  N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var = rowSums(pred.centered^2)/B
  bias.correction = n * N.var * boot.var/B
  vars = raw.IJ - bias.correction
  if (no.replacement) {
    variance.inflation = 1/(1 - mean(rf$inbag))^2
    vars = variance.inflation * vars
  }
  results = data.frame(y.hat = y.hat, var.hat = vars)
  if (nrow(results) <= 20) {
    calibrate = FALSE
    warning("No calibration with n <= 20")
  }
  if (calibrate) {
    calibration.ratio = 2
    n.sample = ceiling(B/calibration.ratio)
    results.ss = randomForestInfJack(rf, newdata, calibrate = FALSE, 
                                     used.trees = sample(used.trees, n.sample))
    sigma2.ss = mean((results.ss$var.hat - results$var.hat)^2)
    delta = n.sample/B
    sigma2 = (delta^2 + (1 - delta)^2)/(2 * (1 - delta)^2) * 
      sigma2.ss
    vars.calibrated = calibrateEB(vars, sigma2)
    results$var.hat = vars.calibrated
  }
 # return(results)
#}
