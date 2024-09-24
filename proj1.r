## comment out of submitted
setwd("D:/OneDrive/文档/Yilin/Edinburgh/Statistical Programming/Assignment1")

a <- scan(
  "4300-0.txt", # file name
  what = "character", # file are read as character vectors
  skip = 73, # skip the first 73 lines of the file
  nlines = 32858 - 73, # specifies the number of rows to read
  fileEncoding = "UTF-8" # file is read in UTF-8 encoding
)

a <- gsub("_(", "", a, fixed = TRUE) ## replace "_(" by ""


split_punct <- function(a) {
  # index of elements containing punctuation
  ia <- grep("[[:punct:]]", a)
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