library(googleAnalyticsR)
library(ggplot2)

ga_auth(no_auto = TRUE)

al <- google_analytics_account_list()
gaid <- 81416156
start <- "2016-12-01"
end <- "2017-01-26"

gadata <- google_analytics_4(gaid,
                             date_range = c(start,end),
                             metrics = "sessions",
                             dimensions = c("nthMinute","sourceMedium","hostname","landingPagePath"),
                             max = -1)

gadata$fullURL <- paste0(gadata$hostname, gadata$landingPagePath)
gadata$timestamp <- as.POSIXct(as.numeric(gadata$nthMinute)*60, origin = as.POSIXct(as.Date(start)))

# 124 URLs
the_urls <- unique(gadata$fullURL)

work_data <- gadata[,c("timestamp","sourceMedium","fullURL")]


## list per URL of sources/min
ref_list <- split(work_data, work_data$fullURL)

timehump_heatmap <- function(the_data){
  
  if(nrow(the_data) < 5){
    return(NULL)
  }
  ## change order of factor levels to order they appear
  the_data$name <- factor(the_data$sourceMedium, 
                          levels = unique(the_data$sourceMedium[order(the_data$timestamp)]))
  
  # make plot
  gg <- ggplot(the_data, aes(x = timestamp, y = name)) + theme_minimal()
  gg <- gg + geom_bin2d()
  gg <- gg + ggtitle(the_data$fullURL[[1]])
  gg <- gg + guides(fill = guide_legend(title = "Session \ncount"))
  gg <- gg + xlab("Session time") + ylab("Source / Medium")
  gg + scale_fill_gradientn(colours = c("#bdc9e1","#74a9cf","#2b8cbe", "#045a8d"))
}

timehump_heatmap(ref_list[[1]])

## make all the plots
lapply(ref_list, timehump_heatmap)
