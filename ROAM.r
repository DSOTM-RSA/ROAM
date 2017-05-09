# ROAM
# R Open Abstraction Module
###########################


# Plotting ----


# Ordered facetPanel Chart
# prepares data for a multi-panel, ordered barchart
# useful for comparing -ve and +ve values per-group
# arguments are.....
# data.frame (df)
# the facetPanel (i.e. grouping variable)
# the barCategory (i.e. observation type)
# the value (i.e numeric value; proportion, count etc.)
# requires tidyverse (ggplot2, dplyr, magrittr, layzeval)


ofp_Figure <- function(df, facetPanel, barCategory, value){
  require(lazyeval) # NSE function which takes named arguments
  df %>% 
    mutate_(barCategory = interp(~reorder(x, y), x = as.name(barCategory), y = as.name(value))) %>% 
    group_by_(facetPanel) %>% 
    filter_(interp(~min_rank(desc(abs(x))) <= 10, x = as.name(value))) %>% 
    group_by_(facetPanel, barCategory) %>% 
    arrange_(interp(~desc(x), x = as.name(value))) %>% 
    ungroup() %>% 
    mutate_(barCategory = interp(
      ~factor(paste(x, y, sep = "__"), levels = rev(paste(x, y, sep = "__"))),
      x = as.name(barCategory), y = as.name(facetPanel)))
  
  # example usage of this function :: source ROAM_exampleData.R
  
  # generate the  output data.frame
  # TWO TYPES of OUPUT FIGURE :: perSite/Sample or perCategory comparisons
  
  ##
  # ex01_perSite<-ofp_Figure(ex01_Figure,"word1","word2","n") 
  
  # ggplot(aes(barCategory, n, fill = n * score>=0), data = ex01_Output) + 
  # geom_bar(stat = "identity", show.legend = FALSE) + 
  # facet_wrap(~ word1, scales = "free") + 
  # xlab("Words preceded by negation") + 
  # ylab("Sentiment score * # of occurrences") + 
  # theme_bw() + coord_flip() +
  # scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) # generate neat labels
  
  # plotting the figure (default is absolute values)
  # note :: using "mutate(nonAbs = (n*score)/abs(score))" on the input data 
  # one could easily create a waterfall plot (neg-pos scale)
  
  ##
  # ex01_perCategory<-ofp_Figure(ex01_Figure,"word2","word1","n") 
  
  # USE word1 as first  argument aesthetic 
  # AND facet_wrap(~word2)
  # AND drop scale_x_discrete(labels.....) in ggplot call
  
  



  
}



# My custom ggplot theme
# sets sensible defaults for plotting
# defaults are good for presensations and posters
# requires extrafont (correctly setup once!)


theme_plain <- function(base_size = 18, base_family = "Ubuntu")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid = element_line(linetype = 0)
    )
}




# Object Management ----



