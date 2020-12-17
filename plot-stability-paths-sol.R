library(checkmate)
library(ggplot2)
library(reshape2)

# Es tut mir sehr lied fuer 'library(reshape2)', es schien mir nur 
# so viel einfacher zu sein :/

plot_stability_paths <- function(stability_paths) {
  
  # check if the matrix is numeric, any missings, rows & columns are named
  # names are necessary for legend
  checkmate::assert_matrix(stability_paths, mode = "numeric",
            any.missing = FALSE, row.names = "named", col.names = "named")
  
  ggplot(melt(stability_paths), aes(x = Var1, y = value, col = Var2)) +
    geom_point(size = 3) + 
    geom_line() +
    scale_x_continuous(name = "# covariates", breaks = seq(0, nrow(stability_paths))) +
    scale_y_continuous(name = expression(Pi), breaks = seq(0, 1, 0.2), 
                       limits = c(0, 1)) +
    theme_classic() +
    theme(legend.title = element_blank())
}
