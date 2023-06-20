library(lubridate)
library(XML)

#' Convert input in month-day-year format to date

as_mdy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  mdy(x)%>% as.Date()
}

#' Convert input in day-month-year format to date

as_dmy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (dmy(x)) %>% as.Date()
}

#' Convert input in year-month-day format to date

as_ymd <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (ymd(x)) %>% as.Date()
}

#' Convert input in year-month-day-hour-minute-second format to date-time

as_ymd_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (ymd_hms(x))
}

#' Convert input in year-month-day-hour-minute format to date-time

as_ymd_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (parse_date_time(x, "%Y%m%d %H%M"))
}

#' Convert input in month-day-year-hour-minute-second format to date-time

as_mdy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (parse_date_time(x, "%m%d%Y %H%M%S"))
}

#' Convert input in month-day-year-hour-minute format to date-time

as_mdy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (parse_date_time(x, "%m%d%Y %H%M"))
}

#' Convert input in day-month-year-hour-minute-second format to date-time

as_dmy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (parse_date_time(x, "%d%m%Y %H%M%S"))
}

#' Convert input in day-month-year-hour-minute format to date-time

as_dmy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (parse_date_time(x, "%d%m%Y %H%M"))
}

#' Convert input in hour-minute-second format to time

as_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (hms(x))
}

#' Convert input in hour-minute format to time
as_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  (hm(x))
}

as_integer <- function(x) {
  if (is.factor(x)) {
    int <- (levels(x) %>% .[x] %>% as.integer())
    if (length(na.omit(int)) == 0) as.integer(x) else int
  } else if (is.character(x)) {
    int <- (as.integer(x))
    if (length(na.omit(int)) == 0) as_integer(as.factor(x)) else int
  } else {
    as.integer(x)
  }
}


as_numeric <- function(x) {
  if (is.factor(x)) {
    num <- (levels(x) %>% .[x] %>% as.numeric())
    if (length(na.omit(num)) == 0) as.numeric(x) else num
  } else if (is.character(x)) {
    num <- (as.numeric(x))
    if (length(na.omit(num)) == 0) as_numeric(as.factor(x)) else num
  } else {
    as.numeric(x)
  }
}

as_factor <- function(x, ordered = FALSE) {
  factor(x, ordered = ordered)
}

as_character <- function(x) {
  as.character(x)
}

as_duration <- function(x) as.numeric(lubridate::as.duration(x))


conversion <- function(classe, x) {
  if (classe == 'as_numeric'){
    as_numeric(x)
  } else if (classe == 'as_integer'){
    as_integer(x)
  } else if (classe == 'as_factor'){
    as_factor(x)
  } else if (classe == 'as_character'){
    as_character(x)
  } else if (classe == 'as_dmy'){
    as_dmy(x)
  } else if (classe == 'as_mdy'){
    as_mdy(x)
  } else if (classe == 'as_ymd'){
    as_ymd(x)
  } else if (classe == 'as_ymd_hm'){
    as_ymd_hm(x)
  } else if (classe == 'as_ymd_hms'){
    as_ymd_hms(varbs)
  } else if (classe == 'as_mdy_hm'){
    as_mdy_hm(x)
  } else if (classe == 'as_mdy_hms'){
    as_mdy_hms(x)
  } else if (classe == 'as_dmy_hm'){
    as_dmy_hm(x)
  } else if (classe == 'as_dmy_hms'){
    as_dmy_hms(x)
  } else if (classe == 'as_hm'){
    as_hm(x)
  } else if (classe == 'as_hms'){
    as_hms(x)
  }
}


custom_glimpse <- function(df) {
  data.frame(
    col_name = colnames(df),
    col_class = as.character(sapply(df, class)),
    row.names = NULL
  )
}


graph_uni <- function(df,x){
  req(df)
  if (is.null(x)) {
    return (NULL)
  } else {
    if ((is.numeric(df[[x]]) == TRUE) | (is.integer(df[[x]]) == TRUE)){
      plt1 <- qplot(df[[x]], geom = "histogram", 
            fill=I("blue"), col=I("black"), alpha=I(0.4))
      
      plt2 <- qplot(df[[x]], geom = "boxplot")
      
      egg::ggarrange(plt1, plt2, heights = 2:1)
      
    } else if (is.factor(df[[x]]) == TRUE ) {
      
      qplot(df[[x]], geom = "bar")  
      
    } 
  }
}


graph_bi <- function(df,x,y){
  req(df)
  if (is.null(x) | is.null(y)) {
    return (NULL)
  } else {
    if ((is.numeric(df[[x]]) == TRUE) & (is.numeric(df[[y]]) == TRUE)){
      ## x,y: continues:
      
      qplot(df[[x]], df[[y]], geom = c("point", "smooth"), method = 'lm')
      
    } else if (((is.numeric(df[[x]]) == TRUE) & (is.factor(df[[y]]) == TRUE)) | ((is.numeric(df[[y]]) == TRUE) & (is.factor(df[[x]]) == TRUE))) {
      ##x:discret, y:continue
      
      qplot(df[[x]], df[[y]], geom = "boxplot")
      
      
    } else if ((is.factor(df[[x]]) == TRUE) & (is.factor(df[[y]]) == TRUE)){
      ### x,y :discrete
      
      df%>%
        ggplot(aes_string(df[[x]], fill =  df[[y]])) + 
        geom_bar(position = "fill") +
        labs("Proportion")+
        scale_fill_brewer(palette = "Set2") 
      
      #qplot(df[[x]], df[[y]], geom = "count")
      
      #qplot(df[[x]] , geom = "bar", fill= df[[y]])
      
      #mosaicplot(df[[x]] ~ df[[y]])
      
      
    }
  }
}


