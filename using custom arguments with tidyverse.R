library(tidyverse)
library(magrittr)
library(rlang)

rm(list = ls())
data("mtcars")
df <- mtcars %>% as_tibble()


# Selecting and grouping by the variable and renaming the variable  --------

my_test1 <- function(df, xx, yy){
  df %<>% dplyr::select({{ xx }}, {{ yy }}) # select the needed variables
df_raw <- df
df %<>% dplyr::group_by({{ xx }}) %>% # easy - just enclose in {{ }}
  dplyr::summarise(reps = n(), {{ yy }} := mean({{ yy }})) %>% # use := for naming variables
  dplyr::ungroup()
return(list(df, df_raw))
}
t1 <- my_test1(df = df,  xx = cyl, yy = mpg)


# Group by all EXCEPT your variable - can't figure out how this works -----------------------------

# my_test2 <- function(df, xx, yy){
#   
#   df %<>% dplyr::select({{ xx }}, {{ yy }}) # select
#   df_raw <- df
#    df %<>% dplyr::group_by_at(setdiff(names(.), c(??)))# %>% # use enquo to quote your variable???
#     dplyr::summarise(reps = n(), {{ yy }} := mean({{ yy }}))  %>% dplyr::ungroup()
#   return(list(df, df_raw))
#}

# t2 <- my_test2(df = df,  xx = cyl, yy = mpg)
# t1[[1]] == t2[[2]] # these should be the same!

# Plotting ------------------------------------------------------


my_fun <- function(df, df_raw, xx, yy, my_fill){
  p <- ggplot(df_raw, aes(x = {{ xx }}, y = {{ yy }}, fill = {{ my_fill }}))
  geom_bar(data = df, stat = "identity", position = position_dodge2(preserve = "single"), width = 0.5, colour = "black")
  
  print(p)
  return(df)
}
my_fun(df = t1[[1]],  df_raw = t1[[2]], xx = cyl, yy = mpg)
view(df)



# In progress - custom ggplot -----------------------------------



plotting <- function(df_raw,
                     df,
                     leg_title,
                     is_legend){
  p <-
    ggplot(df_raw,
           aes(x = GzmB.pgml, y = Reading, fill = Protocol)) + geom_bar(
             data = df,
             stat = "identity",
             position = position_dodge2(preserve = "single"),
             width = 0.5,
             colour = "black"
           )
  p <- p + facet_wrap(~Medium)
  p <-
    p + scale_fill_manual(values = c("white", "grey40", "grey80"))
  # add limits so the legend can fit in the plot
  library(scales)
  p <-
    p + scale_y_continuous(limits = c(0, max(df$Reading) * 1.75), oob = rescale_none)
  ###
  if (unique(df$Protocol) == "GzmBGreen activity") {
    p <-
      p + labs(
        x = "hGzmB (pg/ml)",
        y = "Relative fluorescence intensity",
        title = "Activity",
        subtitle = paste0(unique(df_raw$Time), ", ", unique(df_raw$Reading.type), sep = ""),
        fill = leg_title
      )
    filename <-
      paste0("./Results/",
             unique(df$Reading.type),
             "_",
             unique(df$Time),
             ".png",
             sep = "")
  } else if (unique(df$Protocol) == "ELISA") {
    p <-
      p + labs(
        x = "hGzmB (pg/ml)",
        y = "Absorbance",
        title = "Abundance",
        subtitle = "",
        fill = leg_title
      )
    filename <- paste0("./Results/elisa.png", sep = "")
  } else{
    print("Protocol is neither GzmBGreen activity or ELISA, change this")
  }
  if (is_legend == TRUE) {
    leg_pos <- c(0, 1)
  } else if (is_legend == FALSE) {
    leg_pos <- "none"
  } else {
    print("Specify if there should be a legend or not")
  }
  p <-
    p  + theme_classic() + theme(
      panel.background = element_rect(colour = "black"),
      plot.subtitle = element_text(size = 10),
      strip.text.y = element_text(size = 8),
      legend.justification = c(0, 1),
      legend.position = leg_pos,
      legend.background = element_rect(colour = "black")
    )
  ggsave(
    filename = filename,
    plot = p,
    width = 15,
    height = 10,
    units = "cm"
  )
  print(p)
}
