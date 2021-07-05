 data <- read.csv("D:\\8th Semester books\\Soft_Computing\\oct18-24.csv")
data=data[complete.cases(data), ]
plot(data[,9],type="o")
wt = dwt(data[,9],filter="d4")
plot(wt)