Plot_Intrastructure = function(X, centroid_col = "red", centorid_fill = "red", centroid_shape = 8) {

  # -------------------------------------------------------
  # Dependences
  # -------------------------------------------------------
  library(ggplot2)
  library(ggrepel)
  
  # -------------------------------------------------------
  # Quality
  # -------------------------------------------------------
  Proporciones <- svd(X$Compromise)$d / sum(svd(X$Compromise)$d)
  Porcentajes <- round(Proporciones * 100, 2)

  # -------------------------------------------------------
  # Plot points
  # -------------------------------------------------------
  coords <- X$Coordinates
  p <- ggplot() +
    # Draw axis
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # Axis labels
    xlab(label = paste0("Dim 1 (", Porcentajes[1], "%)")) +
    ylab(label = paste0("Dim 2 (", Porcentajes[2], "%)")) 

  # -------------------------------------------------------
  # Add lines
  # -------------------------------------------------------
  x1 <- X$Scores[, 1]
  y1 <- X$Scores[, 2]
  for (i in 1:length(coords)){
    x2 <- coords[[i]][, 1]
    y2 <- coords[[i]][, 2]
    CO <- data.frame(x1, y1, x2, y2)
    # Draw lines between centroids and projections
    p <- p +
      geom_segment(
        data = CO,
        aes(
          x = x1,
          xend = x2,
          y = y1,
          yend = y2),
        arrow = arrow(length = unit(0, "cm")),
        colour = "black"
        )
  }

  # -------------------------------------------------------
  # Add projections
  # -------------------------------------------------------
  len <- dim(X$X[[1]])[2]
  for (i in 1:length(coords)) {
    dat <- coords[[i]]
    labs <- rep(X$tab_names[i], len)
    # Add coordinates
    p <- p +
      # Add projections
      geom_point(
        data = dat,
        aes(
          x = Dim1,
          y = Dim2)
        ) +
      # Add cordinate labels
      geom_text_repel(
        data = dat,
        aes(
          x = Dim1,
          y = Dim2),
        label = labs, size=2.0
        )
  }

  # ------------------------------------------------------
  # Set color of the text
  # ------------------------------------------------------
  if (centroid_shape > 20 & centroid_shape < 26) {
    col = centorid_fill
  } else {
    col = centroid_col
  }

  # ------------------------------------------------------
  # Add centroids
  # ------------------------------------------------------
  p <- p +
    # Add centroids
    geom_point(
      data = data.frame(X$Scores),
      aes(
        x = X$Scores[, 1],
        y = X$Scores[, 2]
        ),
      shape = centroid_shape,
      color = centroid_col,
      fill = centorid_fill
      ) +
    # Add centroid labels
    geom_text_repel(
      aes(
        x = X$Scores[, 1],
        y = X$Scores[, 2],
        label = rownames(X$Scores), 
        ), size= 3.0,
      col = col
      )

  p

}


