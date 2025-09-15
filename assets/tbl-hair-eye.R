# TODO: needs some formating

# extract eye/hair color data
t <- apply(HairEyeColor, c(2, 1), sum)
t <- t[order(rownames(t)), ]
t <- t[, order(colnames(t))]
names(dimnames(t)) <- c("Olhos", "Cabelo")

dt <- data.frame(
  Olhos = c(rownames(t))
  )
dt <- cbind(dt, as.data.frame(t))
dt <- cbind(dt, data.frame(Total = rowSums(t)))
dt <- rbind(dt, data.frame(Olhos = "Total", apply(dt[, -1], 2, sum, simplify = F)))
row.names(dt) <- NULL

# make table
kbl(dt) |>
  kable_paper(
    c("striped"),
    html_font = "\"Lato\"",
    font_size = 15,
    full_width = F
    ) |>
  add_header_above(c(" " = 1, "Cabelo" = 4, " " = 1)) |>
  row_spec(nrow(dt), bold = TRUE) |>
  column_spec(ncol(dt), bold = TRUE) |> 
  add_footnote("Intencionalmente, os nomes das cores não foram traduzidos.")

