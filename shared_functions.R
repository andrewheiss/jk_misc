# ----------------
# Load libraries
# ----------------
# Libraries for ggplot themes
library(ggplot2)
library(grid)
library(scales)
library(Cairo)


# ---------------
# ggplot themes
# ---------------
# For regular plots
theme_clean <- function(base_size=12, base_family="Source Sans Pro Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), 
          axis.line=element_line(colour="grey50", size=0.2),
          #panel.grid=element_blank(), 
          axis.ticks=element_blank(),
          legend.position="bottom", 
          legend.title=element_text(size=rel(0.8)),
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"))
  
  ret
}

# For maps
theme_blank_map <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.9), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(0.8), family="Source Sans Pro Semibold"))
  ret
}


# Append two spaces to all elements in a character vector except the last one
# Adds fake padding between legend keys in a horizontal legend
add.legend.padding <- function(x) {
  x.pad <- sapply(x, FUN=function(y) paste0(y, "  "))
  x.pad[length(x.pad)] <- x[length(x)]  # Remove padding from last element
  x.pad
}


# Add labels to individual columns for use with haven and RStudio
# Rather than fight with lapply or dplyr::mutate_each, just use a dumb for loop 
# to add label attributes to each column
add_labels <- function(df, labs) {
  for (i in 1:ncol(df)) {
    attr(df[[i]], "label") <- labs[i]
  }
  
  df
}
