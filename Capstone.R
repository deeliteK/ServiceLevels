Sys.time()
library(readxl)
Day1 <- read_excel("Downloads/Capstone.xlsx", 
                     +     sheet = "Sunday", skip = 1, n_max = 12)
View(Day1) 

Sys.time()
Serve1 <- percent(Day1$`Service Level`, accuracy = 1)
Day1[,1] <- Serve1
Day1[,1]

Sys.time()
Day1[,5] <-round(Day1$`Call Volume`, digits = 0)
Day1[,6] <- round(Day1$`Calls Taken`, digits = 0)

Sys.time()
Day1

Sys.time()
library(readxl)
Day2 <- read_excel("Downloads/Capstone.xlsx", 
                     +     sheet = "Monday", skip = 1, n_max = 12)
View(Day2) 

Sys.time()
Serve2 <- percent(Day2$`Service Level`, accuracy = 1)
Day2[,1] <- Serve2
Day2[,1]

Sys.time()
Day2[,5] <-round(Day2$`Call Volume`, digits = 0)
Day2[,6] <- round(Day2$`Calls Taken`, digits = 0)

Sys.time()
Day2

Sys.time()
library(readxl)
Day3 <- read_excel("Downloads/Capstone.xlsx", 
                   sheet = "Tuesday", skip = 1, n_max = 12)
View(Day3)

Sys.time()
Serve3 <- percent(Day3$`Service Level`, accuracy = 1)
Day3[,1] <- Serve3
Day3[,1]

Sys.time()
Day3[,5] <-round(Day3$`Call Volume`, digits = 0)
Day3[,6] <- round(Day3$`Calls Taken`, digits = 0)

Sys.time()
Day3

Sys.time()
library(readxl)
Day4 <- read_excel("Downloads/Capstone.xlsx", 
                   sheet = "Wednesday", skip = 1, n_max = 12)
View(Day4)

Sys.time()
Serve4 <- percent(Day4$`Service Level`, accuracy = 1)
Day4[,1] <- Serve4
Day4[,1]

Sys.time()
Day4[,5] <-round(Day4$`Call Volume`, digits = 0)
Day4[,6] <- round(Day4$`Calls Taken`, digits = 0)

Sys.time()
Day4

Sys.time()
library(readxl)
Day5 <- read_excel("Downloads/Capstone.xlsx", 
                   sheet = "Thursday", skip = 1, n_max = 12)
View(Day5)

Sys.time()
Serve5 <- percent(Day5$`Service Level`, accuracy = 1)
Day5[,1] <- Serve5
Day5[,1]

Sys.time()
Day5[,5] <-round(Day5$`Call Volume`, digits = 0)
Day5[,6] <- round(Day5$`Calls Taken`, digits = 0)

Sys.time()
Day5

Sys.time()
library(readxl)
Day6 <- read_excel("Downloads/Capstone.xlsx", 
                   sheet = "Friday", skip = 1, n_max = 12)
View(Day6)

Sys.time()
Serve6 <- percent(Day6$`Service Level`, accuracy = 1)
Day6[,1] <- Serve6
Day6[,1]

Sys.time()
Day6[,5] <-round(Day6$`Call Volume`, digits = 0)
Day6[,6] <- round(Day6$`Calls Taken`, digits = 0)

Sys.time()
Day6

Sys.time()
library(readxl)
Day7 <- read_excel("Downloads/Capstone.xlsx", 
                   sheet = "Saturday", skip = 1, n_max = 12)
View(Day7)

Sys.time()
Serve7 <- percent(Day7$`Service Level`, accuracy = 1)
Day7[,1] <- Serve7
Day7[,1]

Sys.time()
Day7[,5] <-round(Day7$`Call Volume`, digits = 0)
Day7[,6] <- round(Day7$`Calls Taken`, digits = 0)

Sys.time()
Day7

Sys.time()
Week1 <- rbind(Day1, Day2, Day3, Day4, Day5, Day6, Day7)
View(Week1)

Sys.time()
ServiceLevel <- as.numeric(sub("%", "", Week1$`Service Level`,fixed=TRUE))/100
ServiceLevel
Week1[,1] <- ServiceLevel

Sys.time()
Time <- Week1$Time
Week1[,2] <- Time
Agents <- Week1$Agents
Week1[,3] <- Agents
WaitTime <- Week1$`Wait Time (min)`
Week1[,4] <- WaitTime
CallVol <- Week1$`Call Volume`
Week1[,5] <- CallVol
CallTake <- Week1$`Calls Taken`
Week1[,6] <- CallTake

Sys.time()
Week1

Sys.time()
summary(Week1)

Sys.time()
training.df <- data.frame(ServiceLevel,Time, Agents, WaitTime, CallVol, CallTake)
Passing <- mutate(training.df, Pass.Fail = ifelse(ServiceLevel >= 0.9, "PASS", "FAIL"))
Passing

Sys.time()
Passing$Pass.Fail <- ifelse(Passing$Pass.Fail=="PASS",1,0)
Passing

Sys.time()
pass.graph <- ggplot(Passing, aes(x=Pass.Fail, y=CallVol)) + geom_point()

Sys.time()
pass.graph

Sys.time()
pass.graph + 
  theme_classic() +
  labs(title = "Passing Service Level vs Call Volume",
       x = "Fail(0) Pass(1)",
       y = "Call Volume")

Sys.time()
pass.graph <- ggplot(Passing, aes(x=Agents, y=CallVol)) + geom_point()

Sys.time()
pass.graph

Sys.time()
pass.graph <- pass.graph + geom_smooth(method = "lm", col = "purple")

Sys.time()
pass.graph + 
  theme_classic() +
  labs(title = "Agents vs Call Volume",
       x = "Number of Agents",
       y = "Call Volume")

