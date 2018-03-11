library(pwt9)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

data("pwt9.0")
head(pwt9.0)

country_list <- c("Botswana", "South Africa", 
                  "Germany", "United States of America", "Switzerland")

small_pwt <- pwt9.0 %>%
  filter(country %in% country_list)

small_pwt <- small_pwt %>%
  mutate(country = factor(country, levels = country_list, ordered = TRUE))

# Preferred approach
plots <- small_pwt %>%
  group_by(country) %>%
  nest() %>%
  mutate(plot = map2(data, country, ~ggplot(data = .x)  +
                       geom_line(aes(y = rgdpe, x = year)) +
                       ggtitle(.y) +
                       ylab("Year") +
                       xlab("Average annual hours worked by persons engaged")))

small_pwt %>%
  group_by(country) %>%
  nest() %>%
  head()

print(plots)

map2(paste0(plots$country, ".pdf"), plots$plot, ggsave)


# tidyeval grpCom

grpCom <- function(dataset,grpVar,comVar){
  grpVar <- enquo(grpVar)
  comVar <- enquo(comVar)
  com_name <- paste0("mean_",comVar)[2]
  dataset %>% 
    group_by(!!grpVar) %>% 
    summarise(!!com_name := mean(!!comVar,na.rm = TRUE)) -> dataset
  return(dataset)
}

B <- function(dataset,grpVar,comVar){
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



qB <- B(dataset = small_pwt,currency,avh)
map2(paste0(qB$currency, ".pdf"), qB$plot, ggsave)


multiPlot <- function(dataset,grpVar,xVar,yVar){
  grpVar <- enquo(grpVar)
  xVar <- enquo(xVar)
  yVar <- enquo(yVar)
  #com_name <- paste0("mean_",xVar)[2]
  dataset %>% 
    group_by(!!grpVar) %>% 
    nest() %>% 
    mutate(plot = map2(data, !!grpVar, ~ggplot(data=.x,
                                               aes_string(rlang::quo_text(xVar),
                                                          rlang::quo_text(yVar))) +
                         geom_point()
    ))
}


qC <- multiPlot(dataset = pwt9.0,country,pop,avh)
map2(paste0(qC$country, ".pdf"), qC$plot, ggsave)













grpCom1 <- function(dataset,grpVar,comVar,show=0){
  grpVar <- enquo(grpVar)
  comVar <- enquo(comVar)
  com_name <- paste0("mean_",comVar)[2]
  dataset %>% 
    group_by(!!grpVar) %>% 
    summarise(!!com_name := mean(!!comVar,na.rm = TRUE)) -> dataset
  
  if(show==1){
  ggplot(dataset,aes_string(rlang::quo_text(grpVar),com_name)) + 
      geom_point()
  } else {
  return(dataset)
  }

}



a<-grpCom1(small_pwt,year,pop,show = 1)
a

