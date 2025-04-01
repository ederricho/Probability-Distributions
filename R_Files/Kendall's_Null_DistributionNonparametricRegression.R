# ------------------------------------------------------------------
#------------ Null Distribution for Kendall's Statistic ------------
# ------------------------------------------------------------------

# This is Kendall's statistic for nonparametric regression.

# Make a Histogram of Kendal's Statistic using Monte Carlo
iterations <- 10000
superSumVec <- c()
sampleMax <- 1000
sampleSize <- 4 # <--------------------- Input Sample Size of Data
xSamples <- c(1:sampleMax)

for(k in 1:iterations){
  #---------- Alpha and Beta Values ----------
  alpha <- runif(n = 1, min = 1, max = 100) # Value of Alpha
  beta <- runif(n = 1, min = 1, max = 100) # Value of Beta
  
  # ---------- Residuals ----------
  data <- sample(c(1:sampleMax), sampleSize, replace = TRUE)
  resid <- data - median(data) # Create Residuals with MEdian = 0
  #cat("Median of Residuals: ", median(resid))
  
  # ---------- Make Data ----------
  x_i <- sample(xSamples, sampleSize, replace = TRUE) # Random Sample of X_i
  y_i <- alpha + beta * x_i + resid # Y_i
  
  # ---------- Calculate d_i and signs ----------
  d_i <- y_i - beta * x_i # Calculate d_i
  signs <- sign(d_i) # Convert to Signs
  
  sumVec <- c()
  for(i in 1:(sampleSize-1)){
    for(j in (i+1):(sampleSize)){
      #cat(j,",",j+1,",",i,",",sampleSize - i)
      sign <- sign(d_i[j] - d_i[i])
      sumVec <- append(sumVec, sign)
      #print(sign)
    }
    sum <- sum(sumVec)   
  }
  superSumVec <- append(superSumVec, sum)
}

hist(superSumVec, breaks = 50, col = "lightblue", main = "Histogram of Kendall's Ks Statistic")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Create a Null distribution based on Sample Size and Compute P-Values
# for P(K >= x)
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

kendallNull <- function(sampleSize){
  # ---------------------------------------------
  # ---------- Null Distribution of Ks ----------
  # ---------------------------------------------
  iterations <- 10000
  superSumVec <- c()
  sampleMax <- 1000
  sampleSize <- sampleSize
  xSamples <- c(1:sampleMax)
  
  for(k in 1:iterations){
    #---------- Alpha and Beta Values ----------
    alpha <- runif(n = 1, min = 0, max = 1) # Value of Alpha
    beta <- runif(n = 1, min = 0, max = 1) # Value of Beta
    
    # ---------- Residuals ----------
    data <- sample(c(1:sampleMax), sampleSize, replace = TRUE)
    resid <- data - median(data) # Create Residuals with MEdian = 0
    #cat("Median of Residuals: ", median(resid))
    
    # ---------- Make Data ----------
    x_i <- c(1:sampleSize) # x_i
    #x_iSample <- sample(xSamples, sampleSize, replace = TRUE) # Random Sample of   X_i
    y_i <- alpha + beta * x_i + resid # Y_i
    
    # ---------- Calculate d_i and signs ----------
    d_i <- y_i - beta * x_i # Calculate d_i
    signs <- sign(d_i) # Convert to Signs
    
    # ---------- Calculate Ks Statistic ----------
    sumVec <- c()
    for(i in 1:(sampleSize-1)){
      for(j in (i+1):(sampleSize)){
        #cat(j,",",j+1,",",i,",",sampleSize - i)
        sign <- sign(d_i[j] - d_i[i])
        sumVec <- append(sumVec, sign)
        #print(sign)
      }
      sum <- sum(sumVec)   
    }
    superSumVec <- append(superSumVec, sum)
  }
  x <- seq(0, 100, 2) # Sequence of x
  probVec <- c() # Empty Vector of p-values
  
  # ---------------------------------------------------
  # ---------- Calculate Probability Ks >= x ----------
  # ---------------------------------------------------
  for(i in 1:length(x)){
    prob <- length(superSumVec[superSumVec >= x[i]])/iterations # p-value
    probVec <- append(probVec, prob) # Append p-value Vector
  }
  
  # ---------------------------------------------------
  # ---------- Create Table of Distributions ----------
  # ---------------------------------------------------
  table <- round(probVec, 3)
  return(table)
}

kendallNull(4)