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

head(b, 50)
head(b_cap, 50)