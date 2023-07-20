library(ggplot2)

data <- read.csv("experiment1.csv", row.names = 1)

nn_freq <- data.frame(table(data$index))
nn_freq <- nn_freq[order(nn_freq$Freq, decreasing = T),]
nn_freq$rel_freq <- round(nn_freq$Freq/sum(nn_freq$Freq)*100, 1)

distances <- data.frame()

for (i in unique(data$n_features)){
  tmp <- c(i, mean(data[data$n_features == i,]$distance), sd(data[data$n_features == i,]$distance))
  distances <- rbind(distances, tmp)
}

colnames(distances) <- c("n_features", "mean_distance", "sd")

ggplot(distances, aes(x=n_features, y=mean_distance)) +
  geom_point(size=2.5, alpha = 0.8) +
  geom_errorbar(aes(ymin=mean_distance-sd, ymax=mean_distance+sd), width=.15) +
  scale_x_continuous(n.breaks=10) +
  ggtitle("Eucledian distance vs. number of features")


freq16 <-data.frame()

for (i in unique(data$n_features)){
  tmp <- c(i, sum(data[data$n_features == i,]$index == 16)/length(data[data$n_features == i,]$index)*100)
  freq16 <- rbind(freq16, tmp)
}

colnames(freq16) <- c("n_features", "percent_NN_16")

ggplot(freq16, aes(n_features, percent_NN_16)) +
  geom_col()

for (i in unique(data$n_features)){
  tmp <- c(i, sum(data[data$n_features == i,]$index == 16)/length(data[data$n_features == i,]$index)*100)
  freq16 <- rbind(freq16, tmp)
}

counts_per_feature <- data.frame()

for (i in unique(data$n_features)){
  for (j in unique(data[data$n_features == i,]$index)){
    tmp <- c(i, j, sum(data$n_features == i & data$index == j))
    counts_per_feature <- rbind(counts_per_feature, tmp)
  }
}

colnames(counts_per_feature) <- c("n_feature", "index", "count")
counts_per_feature$index <- factor(counts_per_feature$index)

ggplot(counts_per_feature, aes(x=n_feature, y=count, fill=index)) + 
  geom_bar(position="fill", stat="identity")

