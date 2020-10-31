  Plot_Interstructure = function(X, color = "black") {

  # -------------------------------------------------------
  # Dependences
  # -------------------------------------------------------
  library(ggplot2)
  library(ggforce)
  library(ggpolypath)
  library(ggrepel)

  ##### >Parameters #####
  Interstructure <- X$Interstructure

  ##### >>Prepare circle parameters #####
  circle <- max(abs(X$Interstructure[,1:2])) + 0.03
  radius <- circle
  theta <- seq(0, 2 * pi, length = 500)
  x_ci <- radius * cos(theta)
  y_ci <- radius * sin(theta)
  tag <- rep("Circle", length(x_ci))

  ###### >>Prepare square parameters #####
  square <- circle + 0.1
  x_sq <- c(-square, -square, square, square)
  y_sq <- c(-square, square, square, -square)
  tag2 <- rep("Square", length(x_sq))

  ##### >>Polygons data frame #####
  X <- append(x_ci, x_sq)
  Y <- append(y_ci, y_sq)
  id <- append(tag, tag2)
  polygons <- data.frame(X, Y, id)

  ###### >Plot Interstructure #####
  ggplot() +
    # Add white space
    geom_polypath(
      data = polygons,
      aes(
        x = X,
        y = Y,
        group = id
        ),
      fill = "white",
      alpha = 0.998
      ) +
    scale_x_continuous(
      expand = c(0, 0)
      ) +
    scale_y_continuous(
      expand = c(0, 0)
      ) +
    # Axis labels
    xlab(label = "") +
    ylab(label = "") +
    # Draw x axis
    geom_segment(
      aes(
        x = -circle,
        xend = circle,
        y = 0,
        yend = 0
      ),
      colour = "black"
    ) +
    # Draw y axis
    geom_segment(
      aes(
        x = 0,
        xend = 0,
        y = -circle,
        yend = circle
        ),
      colour = "black"
      ) +
    # Draw circle
    geom_circle(
      aes(
        x0 = 0,
        y0 = 0,
        r = circle)
      ) +
    # Draw arrows for interstructure
    geom_segment(
      data = Interstructure,
      aes(
        x = 0,
        xend = Dim1,
        y = 0,
        yend =  Dim2
        ),
      arrow = arrow(length = unit(0.5, "cm")),
      colour = color) +
    # Add interstructure labels
    geom_text_repel(
      aes(
        x = Interstructure[,"Dim1"],
        y = Interstructure[,"Dim2"],
        label = rownames(Interstructure)
        ), size=3, direction = "y", hjust=1, vjust=0.5, 
      col = color) +
    theme(panel.border = element_blank())

  }

  