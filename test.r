index_vector <- 1:10
mlag <- 4

# Create the matrix M
n <- length(index_vector) # Length of the index vector
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1) # Initialize the matrix

for (i in 1:(mlag + 1)) {
  ## Assignment to all rows in column i of matrix M
  M[, i] <- index_vector[i:(n - mlag + i - 1)]
}

M