c <- c(5, 6, 7, 8, 2, 1, 3, 4)
a <- c("a", "b", "c", "d", "e", "f", "g", "h")
t <- c(1, 1, 2, 2, 3, 3, 4, 4 )
cats <- data.frame(c, a, t)

s <- list(11)


s<-rep(s, length(cats$c))
cats$s <- sapply(s, paste0)

