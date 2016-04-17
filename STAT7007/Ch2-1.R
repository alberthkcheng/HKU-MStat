data <- read.table("data/ch2-1.txt", head = TRUE)

# Cross tab
tab <- xtabs(count~treat+outcome,data=data)
# Table Probablity
p <- dhyper(10,12,6,12)
# Fisher exact test
fisher.test(tab, alternative = "greater")
fisher.test(tab, alternative = "less")
fisher.test(tab, alternative = "two.sided")


