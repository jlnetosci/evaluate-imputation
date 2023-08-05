library(openxlsx)
library(reticulate)
library(ggplot2)
library(patchwork)
library(gtools)
library(qpdf)
library(showtext)
library(colorspace)

fam <- "Lato"
#fam <- "Atkinson Hyperlegible"
font_add_google(fam, fam)
showtext_auto()

summary <- read.xlsx("experiments_summary.xlsx", sheet=1)
dir <- getwd()

for (exp in c(seq(3, 21), 23, seq(25, 40))){
  setwd(paste0(dir, "/experiment", exp))
  
  py_data_frames <- py_load_object(file = "data_frames.obj")
  print(paste(exp, "loaded"))
  
  longer <- data.frame()
  normalized_longer <- data.frame()
  
  for (k in seq(1, length(py_data_frames))) {
    
    data <- py_data_frames[[k]]
    
    distances <- data.frame()
    normalized_distances <- data.frame()
    
    for (i in unique(data$n_features)){
      tmp <- c(i, k, mean(data[data$n_features == i,]$distance), sd(data[data$n_features == i,]$distance))
      distances <- rbind(distances, tmp)
      tmp <- c(i, k, mean(data[data$n_features == i,]$normalized_distance))
      normalized_distances <- rbind(normalized_distances, tmp)
    }
    
    colnames(distances) <- c("n_features", "query", "mean_distance", "sd")
    colnames(normalized_distances) <- c("n_features", "query", "mean_distance")
    
    longer <- rbind(longer, distances)
    normalized_longer <- rbind(normalized_longer, normalized_distances)
    # plot(ggplot(distances, aes(x=n_features, y=mean_distance)) +
    #   geom_point(size=2.5, alpha = 0.8) +
    #   geom_errorbar(aes(ymin=mean_distance-sd, ymax=mean_distance+sd), width=.15) +
    #   scale_x_continuous(n.breaks=10) +
    #   ggtitle("Eucledian distance vs. number of features"))
  }
  
  # if (exp <= 24){
  #   cor = darken("#005e9e", amount= 0.15, space = "HLS") 
  # } else if (exp <= 32) {
  #   cor = darken("#ff4e4e", amount= 0.3, space = "HLS")
  # } else {
  #   cor = lighten("#025c40", amount= 0.15, space = "HCL")
  # }

  if (exp <= 24){
    cor = "#005e9e"
  } else if (exp <= 32) {
    cor = "#ff4e4e"
  } else {
    cor = "#025c40"
  }
  
  assign(paste0("p", exp), ggplot(longer[longer$query == 1,], aes(x=n_features, y=mean_distance)) +
         #geom_point(size=3, alpha = 1, color = cor) +
         geom_col(alpha = 1, fill = cor) +
         geom_errorbar(aes(ymin=mean_distance-sd, ymax=mean_distance+sd), width=.15, color = lighten(cor, amount = 0.5)) +
         scale_x_continuous(n.breaks = 10) +
         scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
         coord_cartesian(ylim=c(0, 3)) +
         labs(title="Eucledian distance vs. number of features",
              subtitle="mean ± standard deviation per feature. single query.",
              caption =paste0("Experiment ", exp,
                              ":\nSeed: ", summary[which(summary$experiment==exp), which(colnames(summary)=="seed")],
                              " | Features: ", summary[which(summary$experiment==exp), which(colnames(summary)=="n_features")],
                              " | Ref. samples: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="n_samples")]),
                              " | Max. value: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="max_value")]),
                              " | Single query (seed 0)")) +
         xlab("") +
         ylab("") +
         theme_classic() +
         theme(text = element_text(size = 15.5, family=fam),
               plot.title=element_text(face='bold'),
               plot.caption=element_text(hjust = 0, face='bold', family=fam),
               plot.caption.position="panel")
  )
  
  pdf(file=paste0("proof_of_concept_distances_experiment", exp, ".pdf"), paper="a4r")
  plot(get(paste0("p", exp)))
  dev.off()
  print(paste(exp, "plot saved"))
  
  assign(paste0("p", exp), ggplot(longer, aes(x=n_features, y=mean_distance, group = query)) +
    geom_line(linewidth=0.3, alpha=0.1, color=cor) +
    scale_x_continuous(n.breaks = 10, expand = c(0, 0)) +
    scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
    coord_cartesian(xlim = c(1, 10.02), ylim = c(0, 4)) +
    labs(title="Eucledian distance vs. number of features",
         subtitle="mean per feature. each line represents one query.",
         caption =paste0("Experiment ", exp,
                         ":\nSeed: ", summary[which(summary$experiment==exp), which(colnames(summary)=="seed")],
                         " | Features: ", summary[which(summary$experiment==exp), which(colnames(summary)=="n_features")],
                         " | Ref. samples: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="n_samples")]),
                         " | Max. value: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="max_value")]),
                         " | Queries: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="n_queries")]))) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(text = element_text(size = 15.5, family=fam),
          plot.title=element_text(face='bold'),
          plot.caption=element_text(hjust = 0, face='bold', family=fam),
          plot.caption.position="panel")
  )
  
  pdf(file=paste0("distances_vs_features_experiment", exp, ".pdf"), paper="a4r")
  plot(get(paste0("p", exp)))
  dev.off()
  print(paste(exp, "plot saved"))
  
  assign(paste0("p", exp), ggplot(normalized_longer, aes(x=n_features, y=mean_distance, group = query)) +
         geom_line(linewidth=0.3, alpha=0.1, color=cor) +
         scale_x_continuous(n.breaks = 10, expand = c(0, 0)) +
         scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
         coord_cartesian(xlim = c(1, 10.02), ylim = c(0, 0.4)) +
         labs(title="Eucledian distance vs. number of features",
              subtitle='“normalized” mean. each line represents one query.',
              caption =paste0("Experiment ", exp,
                              ":\nSeed: ", summary[which(summary$experiment==exp), which(colnames(summary)=="seed")],
                              " | Features: ", summary[which(summary$experiment==exp), which(colnames(summary)=="n_features")],
                              " | Ref. samples: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="n_samples")]),
                              " | Max. value: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="max_value")]),
                              " | Queries: ", scales::comma(summary[which(summary$experiment==exp), which(colnames(summary)=="n_queries")]))) +
         xlab("") +
         ylab("") +
         theme_classic() +
         theme(text = element_text(size = 15.5, family=fam),
               plot.title=element_text(face='bold'),
               plot.caption=element_text(hjust = 0, face='bold', family=fam),
               plot.caption.position="panel")
  )
  
  pdf(file=paste0("normalized_distances_vs_features_experiment", exp, ".pdf"), paper="a4r")
  plot(get(paste0("p", exp)))
  dev.off()
  print(paste(exp, "plot saved"))
  
}

setwd("../")
# 
# mean_means <- data.frame()
# for (i in unique(longer$n_features)){
#   tmp <- c(i, k, mean(longer[longer$n_features == i,]$mean_distance), sd(longer[longer$n_features == i,]$mean_distance))
#   mean_means <- rbind(mean_means, tmp)
# }
# 
# colnames(mean_means) <- c("n_features", "query", "mean_distance", "sd")
# 
# mean_means$normalized_distance = mean_means$mean_distance/mean_means$n_features
# 
# ggplot(mean_means, aes(x=n_features, y=normalized_distance)) +
#   geom_line(linewidth=1, alpha=1, color=cor) +
#   #geom_line(data=mean_means, aes(x=n_features, y=mean_distance), linewidth=0.8, alpha=0.5, color="black", linetype="dotted") +
#   #geom_ribbon(data=mean_means, aes(x=n_features, ymin=mean_distance-sd, ymax=mean_distance+sd), linewidth=0, alpha=0.2, color="black") +
#   scale_x_continuous(n.breaks = 10, expand = c(0, 0)) +
#   scale_y_continuous(n.breaks = 10) +
#   #coord_cartesian(xlim = c(1, 10.01), ylim = c(0, 4)) +
#   theme_classic() +
#   theme(text = element_text(size = 15.5, family=fam),
#         plot.title=element_text(face='bold'),
#         plot.caption=element_text(hjust = 0, face='bold', family=fam),
#         plot.caption.position="panel")
