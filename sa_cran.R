library(dplyr)
library(dlstats)
library(lubridate)
library(RJDemetra)
cran_stats("Rcpp")
ts_download <- function(pkg){
    print(sprintf("Downloading stats on %s", pkg))
    data <- cran_stats(pkg)
    data$start <- ymd(data$start)
    data$year <- year(data$start)
    data$month <- month(data$start)
    data$quarter <- quarter(data$start)
    quarter_data <- data %>%  
        group_by(year,quarter) %>% 
        summarise(downloads = sum(downloads))
    monthly_data <- data[-c(1, nrow(data)),]
    quarter_data <- quarter_data[-c(1, nrow(quarter_data)), ]
    
    first_month <- as.numeric(c(monthly_data$year[1], monthly_data$month[1]))
    first_quarter <- as.numeric(c(quarter_data$year[1], quarter_data$quarter[1]))
    
    monthly_ts <- ts(monthly_data$downloads, start = first_month, frequency = 12)
    quaterly_ts <- ts(quarter_data$downloads, start = first_quarter, frequency = 4)
    list(monthly = monthly_ts, quaterly = quaterly_ts)
}
tidyverse_packages <- c("broom", "cli", "crayon", "dplyr", "dbplyr", "forcats", "ggplot2", 
                        "haven", "hms", "httr", "jsonlite", "lubridate", "magrittr", 
                        "modelr", "purrr", "readr", "readxl", "reprex", "rlang", 
                        "rstudioapi", "rvest", "stringr", "tibble", "tidyr", "xml2", 
                        "tidyverse", "Rcpp")
tidyverse_downloads_ts <- lapply(tidyverse_packages, ts_download)
names(tidyverse_downloads_ts) <- tidyverse_packages
saveRDS(tidyverse_downloads_ts,"ts_tidyverse.RDS")

spec <- x13_spec(spec = "RSA3", tradingdays.option = "WorkingDays")
tidyverse_sa_monthly_models <- lapply(tidyverse_downloads_ts, function(x){
    # The time-series have to contains at least 4 years data
 
    tryCatch(x13(x[["monthly"]], spec),
             error = function(e){
                 NULL
             })
})
list_null <- sapply(tidyverse_sa_monthly_models, is.null)
tidyverse_sa_monthly_models <- tidyverse_sa_monthly_models[!list_null]
saveRDS(tidyverse_sa_monthly_models,"D:/ZW20hj/Mes Documents/tidyverse_sa_monthly_models.RDS")
tidyverse_sa_monthly_models[[1]]$diagnostics$variance_decomposition
tidyverse_sa_contribution <- lapply(names(tidyverse_sa_monthly_models), function(x){
   result <- tidyverse_sa_monthly_models[[x]]$diagnostics$variance_decomposition
   colnames(result) <- x
   result <- 100 * result/ result[6,1]
   round(result,1)
})
tidyverse_sa_contribution <- do.call(cbind, tidyverse_sa_contribution)
# tidyverse_sa_contribution <- do.call(cbind, lapply(tidyverse_sa_monthly_models,function(x) x$diagnostics$variance_decomposition))
tidyverse_sa_contribution[,order(tidyverse_sa_contribution[2,],decreasing = T)]
plot(tidyverse_sa_monthly_models[["readxl"]]$decomposition)
plot(tidyverse_sa_monthly_models[["magrittr"]]$decomposition)
plot(tidyverse_sa_monthly_models[["rvest"]]$decomposition)
plot(tidyverse_sa_monthly_models[["ggplot2"]]$decomposition)

plot(tidyverse_sa_monthly_models[["readxl"]], type_chart = "sa-trend")
plot(tidyverse_sa_monthly_models[["magrittr"]], type_chart = "sa-trend")
plot(tidyverse_sa_monthly_models[["rvest"]], type_chart = "sa-trend")
plot(tidyverse_sa_monthly_models[["ggplot2"]], type_chart = "sa-trend")



