username <- "AQLT"
password <- rstudioapi::askForPassword()

# referrers
res <- system(sprintf("curl -u %s:%s https://api.github.com/repos/jdemetra/rjdemetra/traffic/popular/referrers", username , password),
              intern = TRUE)
res <- res[-(1:(grep("},",res)[1]))]
referrers <- data.frame(referrer = grep("referrer", res, value = TRUE),
                     count = grep("count", res, value = TRUE),
                     uniques = grep("uniques", res, value = TRUE),stringsAsFactors = FALSE
)
referrers[,1] <- gsub("(^ .)|(\"referrer\": \")|(\",)","", referrers[,1])
referrers[,2] <- as.numeric(gsub("(^ .)|(\"count\":)|(\",)|(,)","", referrers[,2]))
referrers[,3] <- as.numeric(gsub("(^ .)|(\"uniques\":)|(\",)|(,)","", referrers[,3]))

# paths
res <- system(sprintf("curl -u %s:%s https://api.github.com/repos/jdemetra/rjdemetra/traffic/popular/paths", username , password),
              intern = TRUE)
res <- res[-(1:(grep("},",res)[1]))]
paths <- data.frame(path = grep("path", res, value = TRUE),
                    title = grep("title", res, value = TRUE),
                        count = grep("count", res, value = TRUE),
                        uniques = grep("uniques", res, value = TRUE),stringsAsFactors = FALSE
)
paths[,1] <- gsub("(^ .)|(\"path\": \")|(\",)","", paths[,1])
paths[,2] <- gsub("(^ .)|(\"title\": \")|(\",)","", paths[,2])
paths[,3] <- as.numeric(gsub("(^ .)|(\"count\":)|(\",)|(,)","", paths[,3]))
paths[,4] <- as.numeric(gsub("(^ .)|(\"uniques\":)|(\",)|(,)","", paths[,4]))

# views
res <- system(sprintf("curl -u %s:%s https://api.github.com/repos/jdemetra/rjdemetra/traffic/views", username , password),
              intern = TRUE)
res <- res[-(1:(grep("},",res)[1]))]
views <- data.frame(timestamp = grep("timestamp", res, value = TRUE),
                   count = grep("count", res, value = TRUE),
                   uniques = grep("uniques", res, value = TRUE),stringsAsFactors = FALSE
                   )
views[,1] <- gsub("(^ .)|(\"timestamp\": \")|(\",)","", views[,1])
views[,2] <- as.numeric(gsub("(^ .)|(\"count\":)|(\",)|(,)","", views[,2]))
views[,3] <- as.numeric(gsub("(^ .)|(\"uniques\":)|(\",)|(,)","", views[,3]))

# views
res <- system(sprintf("curl -u %s:%s https://api.github.com/repos/jdemetra/rjdemetra/traffic/clones", username , password),
              intern = TRUE)
res <- res[-(1:(grep("},",res)[1]))]
clones <- data.frame(timestamp = grep("timestamp", res, value = TRUE),
                    count = grep("count", res, value = TRUE),
                    uniques = grep("uniques", res, value = TRUE),stringsAsFactors = FALSE
)
clones[,1] <- gsub("(^ .)|(\"timestamp\": \")|(\",)","", clones[,1])
clones[,2] <- as.numeric(gsub("(^ .)|(\"count\":)|(\",)|(,)","", clones[,2]))
clones[,3] <- as.numeric(gsub("(^ .)|(\"uniques\":)|(\",)|(,)","", clones[,3]))

write.csv(referrers,file = sprintf("referrers/referrers_%s.csv",Sys.Date()),row.names = FALSE)
write.csv(paths,file = sprintf("paths/paths_%s.csv",Sys.Date()),row.names = FALSE)
write.csv(views,file = sprintf("views/views_%s.csv",Sys.Date()),row.names = FALSE)
write.csv(clones,file = sprintf("clones/clones_%s.csv",Sys.Date()),row.names = FALSE)

