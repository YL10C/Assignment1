string <- "abc123xyz"
result <- sub(".*([0-9])", "\\1", string)
print(result)