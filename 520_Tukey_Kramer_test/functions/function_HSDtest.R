#
# HSD.test shamelessly copied from package agricolae
#   (to avoid package isntallation)
#
HSD.test <- function (y, trt, DFerror, MSerror, alpha = 0.05, group = TRUE, 
          main = NULL, unbalanced = FALSE, console = FALSE) {
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  if (is.null(main)) 
    main <- paste(name.y, "~", name.t)
  clase <- c("aov", "lm")
  if ("aov" %in% class(y) | "lm" %in% class(y)) {
    if (is.null(main)) 
      main <- y$call
    A <- y$model
    DFerror <- df.residual(y)
    MSerror <- deviance(y)/DFerror
    y <- A[, 1]
    ipch <- pmatch(trt, names(A))
    nipch <- length(ipch)
    for (i in 1:nipch) {
      if (is.na(ipch[i])) 
        return(if (console) cat("Name: ", trt, "\n", 
                                names(A)[-1], "\n"))
    }
    name.t <- names(A)[ipch][1]
    trt <- A[, ipch]
    if (nipch > 1) {
      trt <- A[, ipch[1]]
      for (i in 2:nipch) {
        name.t <- paste(name.t, names(A)[ipch][i], sep = ":")
        trt <- paste(trt, A[, ipch[i]], sep = ":")
      }
    }
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  Mean <- mean(junto[, 1])
  CV <- sqrt(MSerror) * 100/Mean
  medians <- tapply.stat(junto[, 1], junto[, 2], stat = "median")
  for (i in c(1, 5, 2:4)) {
    x <- tapply.stat(junto[, 1], junto[, 2], function(x) quantile(x)[i])
    medians <- cbind(medians, x[, 2])
  }
  medians <- medians[, 3:7]
  names(medians) <- c("Min", "Max", "Q25", "Q50", "Q75")
  means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
  sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
  nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
  means <- data.frame(means, std = sds[, 2], r = nn[, 2], medians)
  names(means)[1:2] <- c(name.t, name.y)
  ntr <- nrow(means)
  Tprob <- qtukey(1 - alpha, ntr, DFerror)
  nr <- unique(nn[, 2])
  nr1 <- 1/mean(1/nn[, 2])
  if (console) {
    cat("\nStudy:", main)
    cat("\n\nHSD Test for", name.y, "\n")
    cat("\nMean Square Error: ", MSerror, "\n\n")
    cat(paste(name.t, ",", sep = ""), " means\n\n")
    print(data.frame(row.names = means[, 1], means[, 2:6]))
    cat("\nAlpha:", alpha, "; DF Error:", DFerror, "\n")
    cat("Critical Value of Studentized Range:", Tprob, "\n")
  }
  HSD <- Tprob * sqrt(MSerror/nr)
  statistics <- data.frame(MSerror = MSerror, Df = DFerror, 
                           Mean = Mean, CV = CV, MSD = HSD)
  if (group & length(nr) == 1 & console) 
    cat("\nMinimun Significant Difference:", HSD, "\n")
  if (group & length(nr) != 1 & console) 
    cat("\nGroups according to probability of means differences and alpha level(", 
        alpha, ")\n")
  if (length(nr) != 1) 
    statistics <- data.frame(MSerror = MSerror, Df = DFerror, 
                             Mean = Mean, CV = CV)
  comb <- utils::combn(ntr, 2)
  nn <- ncol(comb)
  dif <- rep(0, nn)
  sig <- NULL
  LCL <- dif
  UCL <- dif
  pvalue <- rep(0, nn)
  for (k in 1:nn) {
    i <- comb[1, k]
    j <- comb[2, k]
    dif[k] <- means[i, 2] - means[j, 2]
    sdtdif <- sqrt(MSerror * 0.5 * (1/means[i, 4] + 1/means[j, 
                                                            4]))
    if (unbalanced) 
      sdtdif <- sqrt(MSerror/nr1)
    pvalue[k] <- round(1 - ptukey(abs(dif[k])/sdtdif, ntr, 
                                  DFerror), 4)
    LCL[k] <- dif[k] - Tprob * sdtdif
    UCL[k] <- dif[k] + Tprob * sdtdif
    sig[k] <- " "
    if (pvalue[k] <= 0.001) 
      sig[k] <- "***"
    else if (pvalue[k] <= 0.01) 
      sig[k] <- "**"
    else if (pvalue[k] <= 0.05) 
      sig[k] <- "*"
    else if (pvalue[k] <= 0.1) 
      sig[k] <- "."
  }
  if (!group) {
    tr.i <- means[comb[1, ], 1]
    tr.j <- means[comb[2, ], 1]
    comparison <- data.frame(difference = dif, pvalue = pvalue, 
                             signif. = sig, LCL, UCL)
    rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
    if (console) {
      cat("\nComparison between treatments means\n\n")
      print(comparison)
    }
    groups = NULL
  }
  if (group) {
    comparison = NULL
    Q <- matrix(1, ncol = ntr, nrow = ntr)
    p <- pvalue
    k <- 0
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        k <- k + 1
        Q[i, j] <- p[k]
        Q[j, i] <- p[k]
      }
    }
    groups <- orderPvalue(means[, 1], means[, 2], alpha, 
                          Q, console)
    names(groups)[1] <- name.y
    if (console) {
      cat("\nTreatments with the same letter are not significantly different.\n\n")
      print(groups)
    }
  }
  parameters <- data.frame(test = "Tukey", name.t = name.t, 
                           ntr = ntr, StudentizedRange = Tprob, alpha = alpha)
  rownames(parameters) <- " "
  rownames(statistics) <- " "
  rownames(means) <- means[, 1]
  means <- means[, -1]
  output <- list(statistics = statistics, parameters = parameters, 
                 means = means, comparison = comparison, groups = groups)
  class(output) <- "group"
  invisible(output)
}

#
# Helper functions  -----  
#

#
# helper function 'tapply.stat'  
#

tapply.stat <- function (y, x, stat = "mean") {
  k <- 0
  numerico <- NULL
  if (is.null(ncol(x))) {
    if (is.numeric(x)) {
      k <- 1
      numerico[1] <- 1
    }
  }
  else {
    ncolx <- ncol(x)
    for (i in 1:ncolx) {
      if (is.numeric(x[, i])) {
        k <- k + 1
        numerico[k] <- i
      }
    }
  }
  cx <- deparse(substitute(x))
  cy <- deparse(substitute(y))
  x <- data.frame(c1 = 1, x)
  y <- data.frame(v1 = 1, y)
  nx <- ncol(x)
  ny <- ncol(y)
  namex <- names(x)
  namey <- names(y)
  if (nx == 2) 
    namex <- c("c1", cx)
  if (ny == 2) 
    namey <- c("v1", cy)
  namexy <- c(namex, namey)
  for (i in 1:nx) {
    x[, i] <- as.character(x[, i])
  }
  z <- NULL
  for (i in 1:nx) {
    z <- paste(z, x[, i], sep = "&")
  }
  w <- NULL
  for (i in 1:ny) {
    m <- tapply(y[, i], z, stat)
    m <- as.matrix(m)
    w <- cbind(w, m)
  }
  nw <- nrow(w)
  c <- rownames(w)
  v <- rep("", nw * nx)
  dim(v) <- c(nw, nx)
  for (i in 1:nw) {
    for (j in 1:nx) {
      v[i, j] <- strsplit(c[i], "&")[[1]][j + 1]
    }
  }
  rownames(w) <- NULL
  junto <- data.frame(v[, -1], w)
  junto <- junto[, -nx]
  names(junto) <- namexy[c(-1, -(nx + 1))]
  if (k == 1 & nx == 2) {
    junto[, numerico[1]] <- as.character(junto[, numerico[1]])
    junto[, numerico[1]] <- as.numeric(junto[, numerico[1]])
    junto <- junto[order(junto[, 1]), ]
  }
  if (k > 0 & nx > 2) {
    for (i in 1:k) {
      junto[, numerico[i]] <- as.character(junto[, numerico[i]])
      junto[, numerico[i]] <- as.numeric(junto[, numerico[i]])
    }
    junto <- junto[do.call("order", c(junto[, 1:(nx - 
                                                   1)])), ]
  }
  rownames(junto) <- 1:(nrow(junto))
  return(junto)
}


orderPvalue <- function (treatment, means, alpha, pvalue, console) {
  n <- length(means)
  z <- data.frame(treatment, means)
  letras <- c(letters[1:26], LETTERS[1:26], 1:9, c(".", 
                                                   "+", "-", "*", "/", "#", 
                                                   "$", "%", "&", "^", "[", 
                                                   "]", ":", "@", ";", "_", 
                                                   "?", "!", "=", "#", rep(" ", 
                                                                           2000)))
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  k1 <- 0
  j <- 1
  i <- 1
  cambio <- n
  cambio1 <- 0
  chequeo = 0
  M[1] <- letras[k]
  q <- as.numeric(rownames(w))
  while (j < n) {
    chequeo <- chequeo + 1
    if (chequeo > n) 
      break
    for (i in j:n) {
      s <- pvalue[q[i], q[j]] > alpha
      if (s) {
        if (lastC(M[i]) != letras[k]) 
          M[i] <- paste(M[i], letras[k], sep = "")
      }
      else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        for (jj in cambio:n) M[jj] <- paste(M[jj], "", 
                                            sep = "")
        M[cambio] <- paste(M[cambio], letras[k], sep = "")
        for (v in ja:cambio) {
          if (pvalue[q[v], q[cambio]] <= alpha) {
            j <- j + 1
            cambio1 <- 1
          }
          else break
        }
        break
      }
    }
    if (cambio1 == 0) 
      j <- j + 1
  }
  w <- data.frame(w, stat = M)
  trt <- as.character(w$treatment)
  means <- as.numeric(w$means)
  output <- data.frame(means, groups = M)
  rownames(output) <- trt
  if (k > 81) 
    cat("\n", k, "groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
  invisible(output)
}

#
# lastC
#
# Setting the last character of a chain
# A special function for the group of treatments in the multiple 
#   comparison tests. Use plot.group.
#
lastC <- function (x) {
    y <- sub(" +$", "", x)
    p1 <- nchar(y)
    cc <- substr(y, p1, p1)
    return(cc)
}

