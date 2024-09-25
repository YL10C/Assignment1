## Yilin Chen s2134652; Xuanyu Hu s2676244;
##

## comment out of submitted
## setwd("D:/PG-CAM/Statistical Programming") # nolint
setwd("D:\\OneDrive\\文档\\Yilin\\Edinburgh\\Statistical Programming\\Assignment1") # nolint
a <- scan(
  file = "4300-0.txt", # file name
  what = "character", # file are read as character vectors
  skip = 73, # skip the first 73 lines of the file
  nlines = 32858 - 73, # specifies the number of rows to read
  fileEncoding = "UTF-8" # file is read in UTF-8 encoding
)

a <- gsub("_(", "", a, fixed = TRUE) ## replace "_(" by ""


split_punct <- function(a) {
  # index of elements containing punctuation at the end
  ia <- grep("[[:punct:]]$", a)
  # new vector stored words and puncts
  res <- rep("", length(ia) + length(a))
  ## where should punct go in res
  iia <- ia + 1:length(ia)
  # insert puncts
  res[iia] <- substr(a[ia], nchar(a[ia]), nchar(a[ia]))
  # insert words before puncts # nolint
  res[iia - 1] <- substr(a[ia], 1, nchar(a[ia]) - 1)
  # insert words do not contain punct
  res[-c(iia, iia - 1)] <- a[-ia]

  return(res)
}

res <- split_punct(a)


# Converts words in text to lower case
lower_a <- tolower(res)
unique_words <- unique(lower_a)

# Use match to find the index vector
index_vector <- match(lower_a, unique_words)


# Count the number of times each unique word appears in the text
word_counts <- tabulate(index_vector, nbins = length(unique_words))


# Determine the threshold of occurrence times
word_table <- data.frame(Word = unique_words, Count = word_counts)
sorted_word_table <- word_table[order(-word_table$Count), ]


# Create a vector b containing the most common m words
m <- 1000
b <- sorted_word_table[1:m, "Word"]


# Set mlag value
mlag <- 4

# Match the lower case Ulysses text to the common words vector
index_vector <- match(lower_a, b)

# Create the matrix M
n <- length(index_vector) # Length of the index vector
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1) # Initialize the matrix

for (i in 1:(mlag + 1)) {
  ## Assignment to all rows in column i of matrix M
  M[, i] <- index_vector[i:(n - mlag + i - 1)]
}

M