#Pollutant Mean Questions

pollutantmean <- function(directory, pollutant, id = 1:332){
  data <- data.frame();
  files <- list.files(directory, full.names = TRUE);
  
  for (index in files){
    data <- rbind(data, read.csv(index, comment.char = ""))
  }
  
  neededMonitors <- subset (data, ID %in% id);
  pollutantmean <- mean(neededMonitors[[pollutant]], na.rm =TRUE);
  pollutantmean;
}


#Answer to questions
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")


#Complete Code 

complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names = TRUE);
  completeCases <- data.frame();
  
  for (index in id) {
    data <- read.csv(files[index], comment.char = "");
    c <- complete.cases(data);
    naRm <- data[c, ];
    completeCases <- rbind(completeCases, c(index, nrow(naRm)));
  }
  
  names(completeCases) <- c("id", "nobs");
  completeCases;
}

#Answer to Questions
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


#Code for correlation 

corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE);
  correlationList <- c();
  index <- 1;
  
  while (index <= length(files)) {
    completeCases <- complete(directory, index);
    
    if (completeCases$nobs > threshold) {
      data <- read.csv(files[index], comment.char = "");
      correlationList <- c(correlationList, cor(data$sulfate, data$nitrate, use = "complete.obs"));
    }
    index <- index + 1;
  }
  
  correlationList;
}

#Answer to questions

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))