library(htmltab)
library(lubridate)
library(dplyr)
url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
ukLang <- htmltab(doc = url)
ukLang[,"Date"] <- ymd(ukLang[,"Date"])
ukLang$month <- month(ukLang$Date)
ukLang$year <- year(ukLang$Date)
ukLang$quarter <- quarters(ukLang$Date)

result <- ukLang %>%  group_by(year,month) %>% count()
result_ts <- ts(result$n, start = c(2006, 3), frequency = 12)
library(AQLTools)
plot(diff(result_ts))
graph_ts(result_ts,diviserParPeriode = TRUE)
plot(result_ts)

year(ymd(ukLang[1:3,1]))
library(RJDemetra)
mysa <- x13_def(result_ts)
plot(mysa)

detach("package:RJDemetra", unload=TRUE)
result <- ukLang %>%  group_by(year, quarter) %>% count()
result_ts <- ts(result$n, start = c(2006, 1), frequency = 4)
library(AQLTools)
plot(diff(result_ts))
graph_ts(result_ts,diviserParPeriode = TRUE)
plot(result_ts)

year(ymd(ukLang[1:3,1]))
library(RJDemetra)
mysa <- x13_def(result_ts)
plot(mysa$decomposition)


library(dplyr)
library(dlstats)
library(lubridate)
library(RJDemetra)

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
                        "tidyverse")
tidyverse_downloads_ts <- lapply(tidyverse_packages, ts_download)
names(tidyverse_downloads_ts) <- tidyverse_packages
saveRDS(tidyverse_downloads_ts,"D:/ZW20hj/Mes Documents/ts_tidyverse.RDS")

tidyverse_sa_monthly_models <- lapply(tidyverse_downloads_ts, function(x){
    # The time-series have to contains at least 4 years data
    tryCatch(x13_def(x[["monthly"]]),
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
   result
})
tidyverse_sa_contribution <- do.call(cbind, tidyverse_sa_contribution)
plot(tidyverse_sa_monthly_models[["magrittr"]]$decomposition)
plot(tidyverse_sa_monthly_models[["readxl"]]$decomposition)
plot(tidyverse_sa_monthly_models[["magrittr"]], type_chart = "sa-trend")
plot(tidyverse_sa_monthly_models[["readxl"]], type_chart = "sa-trend")

pkg <- "dplyr"
dplyr_ts <- ts_download("dplyr")
ggplot2_ts <- ts_download("ggplot2")
datatable_ts <- ts_download("data.table")

plot(ts.union(dplyr_ts[[1]], datatable_ts[[1]]))
library(RJDemetra)

mysa_dplyr <- x13_def(dplyr_ts[[1]])
mysa_dt <- x13_def(datatable_ts[[1]])

plot(result)
plot(mysa_dplyr$decomposition, caption = "S-I ratio {dplyr}")
plot(mysa_dt$decomposition, caption = "S-I ratio {data.table}")
mysa_dplyr$diagnostics
mysa_dt$diagnostics

plot(mysa_dplyr, type_chart = c("sa-trend", "cal-seas-irr"),ask = FALSE)
plot(mysa_dt, type_chart = c("sa-trend", "cal-seas-irr"),ask = FALSE)


