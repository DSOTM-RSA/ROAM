# my custom ggplot2 blank theme

theme_basic <- function(base_size = 16, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid = element_line(linetype = 0)
    )
}

theme_plain <- function(base_size = 18, base_family = "Ubuntu")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid = element_line(linetype = 0)
    )
}