plot_Compromise = function(X, shape = 21, color = "black", fill = "red", size = 3) {

  # -------------------------------------------------------
  # Dependences
  # -------------------------------------------------------
  library(ggplot2)
  library(ggrepel)

  # -------------------------------------------------------
  # Set color of the text
  # -------------------------------------------------------
  if (shape > 20 & shape < 26) {
    col <- fill
  } else {
    col <- color
  }
  
  # -------------------------------------------------------
  # Quality
  # -------------------------------------------------------
  Proporciones <- svd(X$Compromise)$d / sum(svd(X$Compromise)$d)
  Porcentajes <- round(Proporciones * 100, 2)

  # -------------------------------------------------------
  # Plot
  # -------------------------------------------------------
  ggplot(data.frame(X$Scores),
         aes(x = X$Scores[, 1],
             y = X$Scores[, 2])) +
    # Draw axis
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # Axis labels
    xlab(label = paste0("Dim 1 (", Porcentajes[1], "%)")) +
    ylab(label = paste0("Dim 2 (", Porcentajes[2], "%)")) +
    # Add points
    geom_point(
      shape = shape,
      fill = fill,
      color = color,
      size = size
      ) +
    #Add names
    geom_text_repel(
      aes(
        label = rownames(X$Scores)
        ),
      col = col
      )

}

