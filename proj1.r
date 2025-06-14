## Yilin Chen s2134652; Xuanyu Hu s2676244; Xiaotian Xing s2661110
## Question1,2,3,4,8, integrate and fix all the codes (40%); Question5,6,7, generating matrix (30%); Question9,10, simulating words (30%)

## comment out of submitted
## setwd("D:/PG-CAM/Statistical Programming")
## setwd("D:\\OneDrive\\文档\\Yilin\\Edinburgh\\Statistical Programming\\Assignment1")
a <- scan(
  file = "4300-0.txt", # file name
  what = "character", # file are read as character vectors
  skip = 73, # skip the first 73 lines of the file
  nlines = 32858 - 73, # specifies the number of rows to read
  fileEncoding = "UTF-8" # file is read in UTF-8 encoding
)

a <- gsub("_(", "", a, fixed = TRUE) ## replace "_(" by ""
a <- gsub(")_", "", a, fixed = TRUE)

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
b <- sorted_word_table[1:m, "Word"]

# Count whether each word starts with a capital letter
cap_start <- grepl("^[A-Z]", res)  # 'res' is the partitioned original text vector

# Count the frequency of capitalized beginning words
cap_words <- table(tolower(res[cap_start]))
total_words <- table(tolower(res))

# Find the intersection between capital words and all words
valid_words <- intersect(names(cap_words), names(total_words))

# Find words that begin with a capital letter in most cases
most_cap_words <- valid_words[cap_words[valid_words] / total_words[valid_words] > 0.5]


# Capitalize the first letter
capitalize_words <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Replace the word in vector b with uppercase
b_cap <- unname(sapply(b, function(word) {
  if (word %in% most_cap_words) {
    word <- capitalize_words(word)
  }
  return(word)
}))


# Set mlag value
mlag <- 4

# Match the lower case Ulysses text to the common words vector
index_vector <- match(lower_a, b) # include NA

# Create the matrix M
n <- length(index_vector) # Length of the index vector
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1) # Initialize the matrix

for (i in 1:(mlag + 1)) {
  ## Assignment to all rows in column i of matrix M
  M[, i] <- index_vector[i:(n - mlag + i - 1)]
}


# Set the number of words to simulate
nw <- 50

generated_tokens <- rep(NA, nw)

# Identify punctuation in vector b (assuming punctuation is in 'b')
punctuation_indices <- grep("^[[:punct:]]+$", b)  # Find indices of punctuation in 'b'

# Remove NA values from initial_tokens
initial_tokens <- na.omit(index_vector) # Remove NA values

# Remove the tokens corresponding to punctuation
non_punct_tokens <- initial_tokens[!initial_tokens %in% punctuation_indices]

# Generate the first token by sampling randomly from the non-punctuation tokens in b
generated_tokens[1] <- sample(non_punct_tokens, 1) # I don't want the first word is a punct

# i for the token going to generate
for (i in 2:nw) {

  # j for the number of tokens we depend on to generate the next
  for (j in mlag:1) if (i > j) { ## skip lags too long for current i

    # get the last j tokens in the "generated_tokens"
    history <- generated_tokens[(i - j):(i - 1)]

    # storage the candidate tokens
    candidate <- c()

    # iterate each row of matrix M
    for (row in 1:nrow(M)) {
      current_row <- M[row, ]
      current_row <- current_row[!is.na(current_row)]  # remove NA
      row_length <- length(current_row)

      if (row_length > 1) { 
        # keep the last token for generating
        matching_row <- current_row[1:(row_length - 1)]

        # if the length of history is less than or equal to the row,
        # cut the first j tokens from the row
        if (j <= length(matching_row)) {
          matching_row <- matching_row[1:j]

          ## if exact match, add the j + 1 column of the current_row to the candidate vector
          if (all(history == matching_row)) {
            candidate <- c(candidate, current_row[j + 1])
          }
        } else { # for the situation j > length(matching_row), cut the last part from 'history'
          history_trimmed <- history[(j - length(matching_row) + 1):j]

          ## if exact match, add the last column of the current_row to the candidate vector
          if (all(history_trimmed == matching_row)) {
            candidate <- c(candidate, current_row[length(matching_row) + 1])
          }
        }
      }
    }

    # if candidate is empty, continue j-loop
    if (length(candidate) > 0) {
      # Determine whether the previous generated token was a punctuation mark
      if (generated_tokens[i - 1] %in% punctuation_indices) {
        # If the previous token was punctuated, generate a non-punctuated token
        non_punct_candidate <- candidate[!candidate %in% punctuation_indices]

        if (length(non_punct_candidate) > 0) {
          generated_tokens[i] <- sample(non_punct_candidate, 1)
          # escape the loop after successfully generating a token
          break
        }

      } else {
        # If the previous token is not a punctuation mark, it is generated normally
        generated_tokens[i] <- sample(candidate, 1)
        # escape the loop after successfully generating a token
        break
      }

    }

  }
}

# Prompt message
print("Simulating 50-word sections from the model: \n")

# based on b_cap and remove the Spaces before punctuation
output_text_1 <- paste(b_cap[generated_tokens], collapse = " ")
output_text_1 <- gsub(" ([[:punct:]])", "\\1", output_text_1)  # Remove Spaces before punct
cat(output_text_1, "\n\n")

# Prompt message
print("Simulate 50-word sections of text based on the common word frequencies: \n")

# Simulate 50 word based on the common word frequencies
simulate_index <- sample(initial_tokens, nw, replace = TRUE)
output_text_2 <- paste(b_cap[simulate_index], collapse = " ")
output_text_2 <- gsub(" ([[:punct:]])", "\\1", output_text_2)  # Remove Spaces before punct
cat(output_text_2, "\n")