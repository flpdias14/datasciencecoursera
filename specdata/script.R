library(readr)

pollutantmean <- function(directory, pollutant, id = 1:332){
  # verify if directory is not a empty string
  separator <- if (directory != "") "/" else ""
  
  sumPollutant = 0
  countOcurrencies = 0
  
  for (i in id) {
    
    # verify if is necessary put zeros on the left
    leftzero <- ifelse(i > 9,ifelse(i > 99,"","0"),"00")
    
    # read the file
    Xi <- read_csv(paste(directory,
                         separator,
                         leftzero, 
                         i,".csv", sep = ""))
    if (pollutant == "sulfate"){
      sumPollutant <- sumPollutant + sum(Xi$sulfate, na.rm = TRUE)
      countOcurrencies <- countOcurrencies + sum(!is.na(Xi$sulfate))
    }
    else if (pollutant == "nitrate"){
      sumPollutant <- sumPollutant + sum(Xi$nitrate, na.rm = TRUE)
      countOcurrencies <- countOcurrencies + sum(!is.na(Xi$nitrate))
    }
  }
  sumPollutant/countOcurrencies
} 

complete <- function(directory, id = 1:332){
  
  # verify if directory is not a empty string
  separator <- if (directory != "") "/" else ""
  
  
  countOcurrencies = 0
  

  result <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "nobs"))))
  
  for (i in id) {
    
    # verify if is necessary put zeros on the left
    leftzero <- ifelse(i > 9,ifelse(i > 99,"","0"),"00")
    
    # read the file
    Xi <- read_csv(paste(directory,
                         separator,
                         leftzero, 
                         i,".csv", sep = ""))
    countOcurrencies <- sum(!is.na(Xi$Date) & !is.na(Xi$sulfate) & !is.na(Xi$nitrate) & !is.na(Xi$ID), na.rm = TRUE)
    result <- rbind(result , c(i, countOcurrencies))
  }
  names(result)[names(result) == "X1L"] <- "id"
  names(result)[names(result) == "X117L"] <- "nobs"
  result
}

corr <- function(directory, threshould = 0){
  # verify if directory is not a empty string
  separator <- if (directory != "") "/" else ""
  
  
  countOcurrencies = 0
  
  
  result <- data.frame(matrix(ncol=0,nrow=1))
  com <- complete("")

  correlation <- c()
  for (i in 1:dim(com)[1]) {

    if(com[i,"nobs"] > threshould){
      
      leftzero <- ifelse(com[i,"id"] > 9,ifelse(com[i,"id"] > 99,"","0"),"00")
      
      Xi <- read_csv(paste(directory,
                           separator,
                           leftzero, 
                           com[i,"id"],".csv", sep = ""))
      correlation <- append(correlation, 
                            cor(Xi$sulfate, Xi$nitrate, use = "pairwise.complete.obs"))
    }
  }
  if(length(correlation) == 0){
    result <- 0
  }
  else{
    result <- rbind(result,  correlation)
  }
  result
}

cr <- corr("", 2000)                
n <- length(cr)                
cr <- corr("", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))