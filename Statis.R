Statis <- function(X, transform = NULL) {

  # -------------------------------------------------------
  # List of objects that the function returns
  # -------------------------------------------------------
  statis_ <- list(X = X,
                 tab_names = names(X),
                 X_Transformed = NULL,
                 Scalar_Products = NULL,
                 Cosine = NULL,
                 Interstructure = NULL,
                 Weight = NULL,
                 Compromise = NULL,
                 Quality = NULL,
                 Scores = NULL,
                 Projection_Matrix = NULL,
                 Coordinates = NULL)

  # -------------------------------------------------------
  # Check X class
  # -------------------------------------------------------
  X_class <- is.list(X)
  if (X_class != TRUE) {
    stop("X is not a list") }

  # -------------------------------------------------------
  # Check row names
  # -------------------------------------------------------
  for (i in 1:length(X)) {
    if (i != length(X)) {
      res <- identical(row.names(as.data.frame(X[i])), row.names(as.data.frame(X[i+1])))
      if (res != TRUE){
        stop("Row names are not the same") }
    }
  }
  n_rows <- dim(X[[1]])[1]

  # -------------------------------------------------------
  # Transform the data
  # -------------------------------------------------------
  trf <- is.null(transform)
  if (trf != TRUE) {
    if (transform == 'center') {
      CoS <- FALSE
    }
    if (transform == 'scale') {
      CoS <- TRUE
    }
    X_tr <- list()
    for (i in 1:length(X)) {
      X_tr[[i]] <- as.data.frame(scale(as.data.frame(X[[i]]), scale = CoS))
    }
    names(X_tr) <- names(X)
    X <- X_tr
  } else {
    X_tr <- NULL
  }

  # -------------------------------------------------------
  # Dimension names
  # -------------------------------------------------------
  PCs <- vector()
  for (i in 1:dim(X[[1]])[1]){
    npc <- vector()
    npc <- paste(c("Dim",i), collapse = "")
    PCs <- cbind(PCs, npc)
  }

  # -------------------------------------------------------
  # Scalar Product Matrix
  # -------------------------------------------------------
  S <- list()
  for (i in 1:length(X)) {
    S[[i]] <- as.matrix(X[[i]]) %*% as.matrix(t(X[[i]]))
  }
  names(S) <- names(X)

  # -------------------------------------------------------
  # Cosine Matrix
  # -------------------------------------------------------
  Cosine <- matrix(0, nrow = length(X), ncol = length(X))
  row.names(Cosine) <- names(X)
  colnames(Cosine) <- names(X)
  diag(Cosine) <- 1
  library("FactoMineR")
  for (i in 1:length(X)) {
    for (j in 2:length(X)) {
      if (i<j) {
        RV = coeffRV(S[[i]], S[[j]])$rv
        Cosine[i, j] <- round(RV, 4)
        Cosine[j, i] <- round(RV, 4)
      }
    }
  }

  # -------------------------------------------------------
  # Inter-structure
  # -------------------------------------------------------
  Cosine_svd <- svd(Cosine)
  Interstructure <- data.frame(Cosine_svd$u %*% diag(Cosine_svd$d^0.5))
  rownames(Interstructure) <- names(X)
  colnames(Interstructure) <- PCs[1:ncol(Interstructure)]

  # -------------------------------------------------------
  # Weight vector
  # -------------------------------------------------------
  theta <- svd(Cosine)$v
  alpha <- rep(0,length(X))
  for(i in 1:length(X)) {
    alpha[i] <- theta[i]/sum(theta)
  }

  # -------------------------------------------------------
  # Compromise Matrix
  # -------------------------------------------------------
  SC <- matrix(0, nrow = n_rows, ncol = n_rows)
  for (i in 1:length(X)) {
    SC <- SC + (alpha[i] * S[[i]])
  }

  # -------------------------------------------------------
  # Quality of compromise
  # -------------------------------------------------------
  sigma <- eigen(Cosine)$values
  Q <- round(sigma/sum(sigma), 4)

  # -------------------------------------------------------
  # Scores
  # -------------------------------------------------------
  SC_svd <- svd(SC)
  ## SC_svd$v = SC_svd$u porque El compromiso es una matriz simetrica
  F_ <- SC_svd$v %*% diag(SC_svd$d^0.5)
  rownames(F_) <- rownames(X[[1]])

  # -------------------------------------------------------
  # Coordinates
  # -------------------------------------------------------
  G <- SC_svd$v%*%diag(SC_svd$d^(-0.5))
  Coordinates <- list()
  for (i in 1:length(X)) {
    Coordinates[[i]] <- data.frame(S[[i]] %*% G)
    colnames(Coordinates[[i]]) <- PCs
  }
  setNames(Coordinates, statis_$tab_names)
  # names(Coordinates) <-

  # -------------------------------------------------------
  # Objects returned by the function
  # -------------------------------------------------------
  statis_$X_Transformed <- X_tr
  statis_$Scalar_Products <- S
  statis_$Cosine <- Cosine
  statis_$Interstructure <- Interstructure
  statis_$Weight <- alpha
  statis_$Compromise <- SC
  statis_$Quality <- Q
  statis_$Scores <- F_
  statis_$Projection_Matrix <- G
  statis_$Coordinates <- Coordinates


  statis_

}
