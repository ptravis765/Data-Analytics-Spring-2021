#compare to kknn?

library(kknn)
spam.kknn <- kknn(spam~., train, test, distance = 1,
                  kernel = "triangular")
summary(spam.kknn)
# etc....
# other distances and kernels!!