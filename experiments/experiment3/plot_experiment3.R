library(reticulate)
library(ggplot2)
library(patchwork)
# library(gtools)
# library(qpdf)

py_data_frames <- py_load_object(file = "data_frames.obj")

#data <- py_data_frames[[1000]]

df <- data.frame("n_features"=1:10)
vt <- data.frame()
hz <- df

for (k in seq(1, 1000)) {
  data <- py_data_frames[[k]]
  percent <- c()
  for (i in unique(data$n_features)) {
    percent <- append(percent, sum(data[data$n_features == i,]$index == data$index[nrow(data)])/nrow(data[data$n_features == i,]))
  }
  tmp <- cbind(df, percent)
  vt <- rbind(vt, tmp)
  hz <- cbind(hz, percent)
  colnames(hz)[ncol(hz)] <- paste0("seed", k)
}

m <- c()
stdev <- c()

for (j in 1:10){
  m <- append(m, mean(as.matrix(hz[j,2:ncol(hz)])))
  stdev <- append(stdev, sd(as.matrix(hz[j,2:ncol(hz)])))
}

to_plot <- data.frame("n_features"=1:10, "mean"=m, "sd"=stdev)

ggplot(to_plot, aes(x=n_features, y=mean)) +
  geom_line() +
  geom_point(data=to_plot, aes(x=n_features, y=mean)) +
  theme_classic()


for (k in seq(1, 1000)) {
  data <- py_data_frames[[k]]
  
  nn_freq <- data.frame(table(data$index))
  nn_freq <- nn_freq[order(nn_freq$Freq, decreasing = T),]
  nn_freq$rel_freq <- round(nn_freq$Freq/sum(nn_freq$Freq)*100, 1)
  
  distances <- data.frame()
  
  for (i in unique(data$n_features)){
    tmp <- c(i, mean(data[data$n_features == i,]$distance), sd(data[data$n_features == i,]$distance))
    distances <- rbind(distances, tmp)
  }
  
  colnames(distances) <- c("n_features", "mean_distance", "sd")
  
  plot(ggplot(distances, aes(x=n_features, y=mean_distance)) +
         geom_point(size=2.5, alpha = 0.8) +
         geom_errorbar(aes(ymin=mean_distance-sd, ymax=mean_distance+sd), width=.15) +
         scale_x_continuous(n.breaks=10) +
         ggtitle("Eucledian distance vs. number of features"))
  
  counts_per_feature <- data.frame()
  
  for (i in unique(data$n_features)) {
    for (j in unique(data[data$n_features == i, ]$index)) {
      tmp <- c(i, j, sum(data$n_features == i & data$index == j))
      counts_per_feature <- rbind(counts_per_feature, tmp)
    }
  }
  
  colnames(counts_per_feature) <- c("n_features", "index", "count")
  
  counts_per_feature$index <- ifelse(counts_per_feature$index == counts_per_feature$index[nrow(counts_per_feature)], counts_per_feature$index[nrow(counts_per_feature)], "other")
  
  counts_per_feature$index <- factor(counts_per_feature$index, levels = c("other", counts_per_feature$index[nrow(counts_per_feature)]))
  
  # assign(paste0("p", k), ggplot(counts_per_feature, aes(x = n_features, y = count, fill = index)) + 
  #          geom_bar(position = "fill", stat = "identity") +
  #          scale_x_continuous("", breaks = seq(1, 10), expand = c(0.01, 0.01)) + 
  #          scale_y_continuous("", n.breaks = 10, expand = c(0, 0)) +
  #          scale_fill_manual(values = c(alpha("lightgrey", 0.4), "#005e9e")) +
  #          theme_classic() +
  #          theme(legend.position = "none", text = element_text(size = 12)) +
  #          annotate("text", x = 1, y = 0.95, label = k, size = 5)
  # )
  # 
  # if (k %% 20 == 0) {
  #   pdf(paste0("plots_", k, ".pdf"), width = 11.69, height = 8.27, paper = "a4r")
  #   plots <- get(paste0("p", k - 19)) + get(paste0("p", k - 18)) + get(paste0("p", k - 17)) + get(paste0("p", k - 16)) +
  #     get(paste0("p", k - 15)) + get(paste0("p", k - 14)) + get(paste0("p", k - 13)) + get(paste0("p", k - 12)) +
  #     get(paste0("p", k - 11)) + get(paste0("p", k - 10)) + get(paste0("p", k - 9)) + get(paste0("p", k - 8)) +
  #     get(paste0("p", k - 7)) + get(paste0("p", k - 6)) + get(paste0("p", k - 5)) + get(paste0("p", k - 4)) +
  #     get(paste0("p", k - 3)) + get(paste0("p", k - 2)) + get(paste0("p", k - 1)) + get(paste0("p", k))
  #     plot_layout(nrow = 4)
  #   print(plots)
  #   dev.off()
  # }
}

# files <- mixedsort(list.files(pattern="*.pdf"))
# 
# qpdf::pdf_combine(input = files, output = "output.pdf")
