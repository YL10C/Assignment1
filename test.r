a <- c("_(", "acda,", "bsdfgrgw.", "c")

ia <- grep("[[:punct:]]", a) # index of elements containing punctuation

ia

res <- rep("", length(ia) + length(a)) # new vector stored words and puncts

iia <- ia + 1:length(ia) ## where should punct go in res

res[iia] <- substr(a[ia], nchar(a[ia]), nchar(a[ia]))
res[iia - 1] <- substr(a[ia], 1, nchar(a[ia]) - 1)
res[-c(iia, iia - 1)] <- a[-ia]
res