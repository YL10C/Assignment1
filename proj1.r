## Yilin Chen s2134652; Xuanyu Hu s2676244;Xiaotian Xing s2661110
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
b <- sorted_word_table[1:m, "Word"]


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

M

# Set the number of words to simulate
nw <- 50

# Function to sample a word based on the probabilities
sample_word <- function(word_probs) {
  return(sample(b, 1, prob = word_probs, replace = TRUE))
}

# Initialize a vector to store the generated text
simulated_text <- vector("character", nw)

# Generate the first token by sampling randomly from all non-NA tokens
initial_token_index <- sample(which(!is.na(index_vector)), 1)
simulated_text[1] <- b[index_vector[initial_token_index]]

# Now simulate the rest of the tokens
for (i in 2:nw) {
  # Get the previous mlag words
  prev_tokens <- simulated_text[max(1, i - mlag):(i - 1)]
  
  # Find the corresponding indices in M
  last_indices <- match(prev_tokens, b)
  
  # Calculate the probabilities based on the model
  # Here, you can define how probabilities are assigned
  # For simplicity, let's assume uniform probabilities for now
  word_probs <- rep(1/length(b), length(b)) # Adjust this as needed based on your model
  
  # Sample the next word based on the previous tokens
  next_word <- sample_word(word_probs)
  
  # Store the sampled word
  simulated_text[i] <- next_word
}

# Print the generated text
cat(simulated_text, sep = " ")


# Set the number of words to simulate
nw <- 50

# Get the total count of all words for probability calculation
total_word_count <- sum(word_counts)

# Calculate probabilities based on frequencies
word_probs <- word_counts / total_word_count

# Initialize a vector to store the generated text
simulated_text_freq <- vector("character", nw)

# Generate the first token by sampling from all common words
simulated_text_freq[1] <- sample(b, 1, prob = word_probs)

# Now simulate the rest of the tokens
for (i in 2:nw) {
  # Sample the next word independently based on the common word probabilities
  next_word <- sample(b, 1, prob = word_probs)
  simulated_text_freq[i] <- next_word
}

# Print the generated text
cat(simulated_text_freq, sep = " ")


# Find capitalized words in the original text
capitalized_words <- unique(b[grepl("^[A-Z]", b)])

# Function to adjust word casing based on the original text
capitalize_word <- function(word) {
  if (word %in% capitalized_words) {
    return(toupper(substring(word, 1, 1)) %>% paste0(tail(strsplit(word, "")[[1]], -1), collapse = ""))
  }
  return(word)
}

# Create a modified vector b for printing with proper casing
modified_b <- sapply(b, capitalize_word)

# Generate the 50-word section again, but using modified_b for printing
simulated_text_with_cap <- vector("character", nw)
simulated_text_with_cap[1] <- sample(modified_b, 1, prob = word_probs)

for (i in 2:nw) {
  next_word <- sample(modified_b, 1, prob = word_probs)
  simulated_text_with_cap[i] <- next_word
}

# Clean up spaces before punctuation
formatted_text <- gsub(" ([[:punct:]])", "\\1", paste(simulated_text_with_cap, collapse = " "))

# Print the generated text with proper capitalization and punctuation handling
cat(formatted_text)


