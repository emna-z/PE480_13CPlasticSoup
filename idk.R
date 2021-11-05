library(tidyverse)
tab <- t(read.delim("./length.txt", header = F))
colnames(tab) <- c("reads_length", "reads_count")
tab <- as_tibble(tab)
ggplot(tab, aes(x=reads_length, y = reads_count)) + geom_histogram( colour="#009999", fill="#009999", position = "identity") 
 # geom_vline(data=tab, aes(xintercept=median(reads_length)),
  #           linetype="dashed", size=1, colour="red")

median(tab$reads_length)+(median(tab$reads_length)*5/100)
p <- ggplot(data=tab, aes(x=reads_length, y=reads_count)) +
  geom_bar(stat="identity", color="#009999", fill="#009999")+
  theme_minimal()+geom_vline(data=tab, aes(xintercept=median(reads_length)),linetype="dashed", size=1, colour="red")+
  geom_vline(data=tab, aes(xintercept=(median(reads_length)+10)),linetype="dashed", size=1, colour="red")+
  geom_vline(data=tab, aes(xintercept=(median(reads_length)-10)),linetype="dashed", size=1, colour="red")

p

