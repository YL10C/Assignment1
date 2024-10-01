## Yilin Chen s2134652; Xuanyu Hu s2676244; Xiaotian Xing s2661110
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
  # insert puncts (extract all trailing punctuation)
  res[iia] <- sub(".*?([[:punct:]]+)$", "\\1", a[ia])
  # insert words before puncts (remove trailing punctuation)
  res[iia - 1] <- sub("([[:punct:]]+)$", "", a[ia])
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
b <- sorted_word_table[ , "Word"]


# Set mlag value
mlag <- 2

# Match the lower case Ulysses text to the common words vector
index_vector <- match(lower_a, b) # include NA

# the function used to create the matrix M
create_matrix <- function(mlag, index_vector) {
  n <- length(index_vector) # Length of the index vector
  M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1) # Initialize the matrix

  for (i in 1:(mlag + 1)) {
    ## Assignment to all rows in column i of matrix M
    M[, i] <- index_vector[i:(n - mlag + i - 1)]
  }

  return(M)
}



# Set the number of words to simulate
nw <- 30

generated_tokens <- rep(NA, nw)

# Identify punctuation in vector b (assuming punctuation is in 'b')
punctuation_indices <- grep("^[[:punct:]]+$", b)  # Find indices of punctuation in 'b'

# Remove NA values and punctuation tokens from initial_tokens
initial_tokens <- na.omit(index_vector) # Remove NA values

# Remove the tokens corresponding to punctuation
non_punct_tokens <- initial_tokens[!initial_tokens %in% punctuation_indices]

# Generate the first token by sampling randomly from the non-punctuation tokens in b
generated_tokens[1] <- sample(non_punct_tokens, 1) # I don't want the first word is a punct

t <- 1

# i for the token going to generate
for (i in 2:nw) {

  # j for the number of tokens we depend on to generate the next
  for (j in mlag:1) if (i > j) { ## skip lags too long for current i

    #create the matrix of order j
    M <- create_matrix(j, index_vector)

    # get the last j tokens in the "generated_tokens"
    history <- generated_tokens[(i - j):(i - 1)]

    # storage the candidate tokens
    candidate <- c()

    # iterate each row of matrix M
    for (row in 1:nrow(M)) {
      current_row <- M[row, ]
      # not_na_index <- !is.na(current_row) # record the index of non-NA
      # current_row <- current_row[!is.na(current_row)]  # remove NA
      row_length <- length(current_row)

      if (!is.na(current_row[row_length])) { # the last element in the row cannot be NA
        # keep the last token for generating
        matching_row <- current_row[1:(row_length - 1)]
        # record the index of non-NA
        not_na_index <- !is.na(matching_row)
        
        if (length(not_na_index) != 0  && all(history[not_na_index] == matching_row[not_na_index])) {
          candidate <- c(candidate, current_row[row_length])
        }
      }
    }

    # if candidate is empty, continue j-loop
    if (length(candidate) > 0) {
      # draw one of the candidates as the generated token
      generated_tokens[i] <- sample(candidate, 1)
      # escape the loop after successfully generating a token
      break
    }

  }
}

cat(b[generated_tokens], sep = " ")
