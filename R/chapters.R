# Common R definitions


# libraries
library("kableExtra")


# set the palette
palette("Okabe-Ito")


# modified plot function
ecd_plot <- function(
    ...,
    pch = 21,
    col = 6, # blue
    bg = 3, # skyblue
    cex = 1.25,
    mgp = c(2.5, 1, 0)
) {
  plot(
    ...,
    pch = pch,
    col = col,
    bg = bg,
    cex = cex,
    mgp = mgp
  )
}


# modified points function
ecd_points <- function(
    ...,
    pch = 21,
    col = 6, # blue
    bg = 3, # skyblue
    cex = 1.25
) {
  points(
    ...,
    pch = pch,
    col = col,
    bg = bg,
    cex = cex
  )
}



# modified barplot function
ecd_barplot <- function(
    ...,
    mgp = c(2.5, 1, 0),
    col = 3 # skyblue
) { 
  barplot(
    ...,
    mgp = mgp,
    col = col
  )
}


# modified hist function
ecd_hist <- function(
    ...,
    mgp = c(2.5, 1, 0),
    col = 3 # skyblue
) { 
  hist(
    ...,
    mgp = mgp,
    col = col
  )
}


# modified boxplot function
ecd_boxplot <- function(
    ...,
    outpch = 21,
    col = 3 # skyblue
) {
  boxplot(
    ...,
    outpch = outpch,
    col = col,
    outbg = col
  )
}


# modified grid function
ecd_grid <- function(
    ...,
    lty = "solid"
) {
  grid(
    ...,
    lty = lty
  )
}


# modified venn diagram
ecd_venn <- function(
    ...,
    zcolor = 5,
    col = 5,
    lwd = 2,
    opacity = 0.05,
    omega.inside = TRUE
) {
  venn::venn(
    ...,
    zcolor = zcolor,
    col = col,
    lwd = lwd,
    opacity = opacity
  )
  
  # put omega at top right
  text(ifelse(omega.inside, 950, 1030), 950, expression(Omega), cex = 1.5)
  
}