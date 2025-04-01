# ---------------------------------------------------------------------------
# ---------------- Null Distribution of the Pitman Test ---------------------
# ---------------------------------------------------------------------------

# ----------
# Input Sample and Proposed Median (Theta)
# ----------
sample <- c(12,18,24,26,37,40,42,47,49,49,78,108) # Sample
theta <- 30 # Theta_0

# -----------------------------------------------------------------------
# ------------------- Calulate the Null Distribution --------------------
# -----------------------------------------------------------------------

# ---------- Sample Sizes and Possible Values ----------
n <- length(sample) # Length of Sample
diff <- abs(sample - theta) # Difference Vector
sPlusVector <- numeric(iterations) # Empty Vector of SPlus Values
sMinusVector <- numeric(iterations) # Empty Vector of SMinus Values
indexes <- c(1:n) 

# ---------- Pitman Tests ----------
iterations <- 10000
for(i in 1:iterations){
  ones <- rep(1, n) # Vector of 1's to Multiply
  signs <- sample(x = c(1,-1),
                  size = n,
                  replace = TRUE) # Number of Negative Signs
  modDiff <- diff * signs # Sample with Negative Indexes
  sum <- sum(modDiff[modDiff > 0]) # Sum of Modified Difference Vector
  sumNeg <- abs(sum(modDiff[modDiff < 0])) # Sum of S Minus Vector
  sPlusVector[i] <- sum # Append SPlus Vector with SPlus
  sMinusVector[i] <- sumNeg # Append SMinus Vector with SMinus
}

#---------------------------------------------------------------
# ---------- Right-Tail Critical Values for SPlus ----------
ninetyPercent <- quantile(sPlusVector, 0.90)
ninetyFivePercent <- quantile(sPlusVector, 0.95)
ninetyNinePercent <- quantile(sPlusVector, 0.99)

# ---------- Right-Tail Critical Values for SPlus ----------
ninetyPercentN <- quantile(sMinusVector, 0.90)
ninetyFivePercentN <- quantile(sMinusVector, 0.95)
ninetyNinePercentN <- quantile(sMinusVector, 0.99)
# ---------------------------------------------------------------

# ----------------------------------------------------------------
# ---------- Print Critical Values ----------
cat("---------- For S-Plus ----------","\n",
    "Critical Value at 90%: ", ninetyPercent,"\n",
    "Critical Value at 95%: ", ninetyFivePercent,"\n",
    "Critical Value at 99%: ", ninetyNinePercent, "\n")

# ---------- Print Critical Values ----------
cat("---------- For S-Minus ----------","\n",
    "Critical Value at 90%: ", ninetyPercentN,"\n",
    "Critical Value at 95%: ", ninetyFivePercentN,"\n",
    "Critical Value at 99%: ", ninetyNinePercentN)
# -----------------------------------------------------------------

# ---------- Plot Histograms ----------
hist(sPlusVector)
hist(sMinusVector)

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# ----------
# P-Value for S-Plus
splus <- 95 # S-Plus from Problem
length(sMinusVector[sMinusVector>=splus])/iterations # P-Value

# ----------
# P-Value for S-Minus
sminus <- 40 # S-Plus from Problem
length(sMinusVector[sMinusVector<=sminus])/iterations # P-Value

