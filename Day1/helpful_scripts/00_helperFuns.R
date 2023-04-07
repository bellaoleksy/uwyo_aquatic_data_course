
# Base ggplot2 theme ------------------------------------------------------

plot_theme <- function(...) {
  theme_pubr() +
    cowplot::theme_half_open(12) +
    cowplot::background_grid()+
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.caption = element_text(size = 8),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 8, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10, l = -10)),
      axis.title.x = element_text(margin = margin(t = 10, b = -10)),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8
      ),
      axis.text.y = element_text(size = 8),
      # strip.text = element_text(size=8),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 8,
        color = "white",
        fill = "#5D729D",
        box.color = "#4A618C",
        halign = 0.5,
        linetype = 1,
        r = unit(5, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      
      #additional settings passed to theme()
      ...
    )
  
}

#Make default theme for all .R and .Rmd files
theme_set(plot_theme())

# Base ggplot2 theme for maps ------------------------------------------------------

map_theme <- function(caption.hjust=1, caption.vjust=0, ...) {
  theme_void() + theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
    plot.title = element_text(
      size=8,
      face="bold"
    ),
    plot.caption = element_text(
      size=8,
      hjust = caption.hjust,
      vjust = caption.vjust
    ),
    plot.subtitle = element_text(
      size=8
    ),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size=8
    ),
    legend.title = element_text(
      size=8,
      face="bold"
    ),
    legend.text = element_text(
      size=8
    ),
    strip.text = element_text(
      size=8,
      face="bold"
    ),
    #additional settings passed to theme()
    ...
  )
}



# Coefficient of variation (cv) -------------------------------------------


cv <- function(x, na.rm = TRUE)  {
  sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}



# Convert to ts object ----------------------------------------------------


#  function for converting to time series
ts_conv <- function(data){
  data = data %>%
    arrange(date) %>%

    na.trim() %>%
    as.data.frame(.)
  dat_ts = xts(x = data[, "value"],
               order.by = data[, "date"])
  dat_ts = na.trim(na_interpolation(dat_ts, option = "stine"))
}


# Get time indices related to datetime ------------------------------------


# function to get time indices related to datetimes
index_fun <- function(ts_data){
  ts_data = tibble(date = index(ts_data),
                   timeindex = row_number(date),
                   value = as.numeric(ts_data))
}


# select = dplyr::select --------------------------------------------------


#Override select issue
select <- dplyr::select

#Over map from some other package that's driving me crazy
map <- purrr::map
