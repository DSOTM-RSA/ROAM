# grouped AVG -
# uses tidyeval from dplyr 0.6

grpPlot_AVG <- function(dataset,grpVar,comVar){
  grpVar <- enquo(grpVar)
  comVar <- enquo(comVar)
  com_name <- paste0("mean_",comVar)[2]
  dataset %>% 
    group_by(!!grpVar) %>% 
    nest() %>% 
    mutate(plot = map2(data, !!grpVar, ~ggplot(data=dataset,
                                               aes_string(rlang::quo_text(grpVar),
                                                          rlang::quo_text(comVar))) +
                         geom_point()
    ))
}


# grouped XY -
# uses tidyeval from dplyr 0.6
grpPlot_XY <- function(dataset,grpVar,xVar,yVar){
  grpVar <- enquo(grpVar)
  xVar <- enquo(xVar)
  yVar <- enquo(yVar)
  dataset %>% 
    group_by(!!grpVar) %>% 
    nest() %>% 
    mutate(plot = map2(data, !!grpVar, ~ggplot(data=.x,
                                               aes_string(rlang::quo_text(xVar),
                                                          rlang::quo_text(yVar))) +
                         geom_point()
    ))
}

# in conjuction with map2
# grouped_XY <- grpPlot_XY(dataset = small_pwt,country,pop,avh)
# map2(paste0(qC$country, ".pdf"), qC$plot, ggsave)