library(openxlsx)
library(reticulate)
library(ggplot2)
library(patchwork)
library(gtools)
library(qpdf)
library(showtext)

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
  
  df <- data.frame("n_features"=unique(py_data_frames[[1]]$n_features))
  vt <- data.frame()
  hz <- df
  
  for (k in seq(1, length(py_data_frames))) {
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
  
  for (j in unique(data$n_features)){
    m <- append(m, mean(as.matrix(hz[j,2:ncol(hz)])))
    stdev <- append(stdev, sd(as.matrix(hz[j,2:ncol(hz)])))
  }
  
  to_plot <- data.frame("n_features"=unique(df$n_features), "mean"=m, "sd"=stdev)
  to_plot$sd[which(to_plot$sd == 0)] <- NA
  
  if (exp <= 24){
    cor = "#005e9e" 
  } else if (exp <= 32) {
    cor = "#ff4e4e"
  } else {
    cor = "#025c40"
  }
  
  assign(paste0("p", exp), ggplot() +
           geom_ribbon(data=to_plot, aes(x=n_features, ymin=mean-sd, ymax=mean+sd), fill=cor, alpha=0.25) +
           #geom_point(data=vt, aes(x=n_features, y=percent), position = position_jitter(seed = 42, width=0.05, height=0.005), alpha=0.05, fill="lightgrey", shape=1) +
           geom_point(data=to_plot, aes(x=n_features, y=mean), color=cor, size = 3) +
           geom_line(data=to_plot, aes(x=n_features, y=mean), color=cor, linewidth = 0.6, linetype = "dotted") +
           geom_text(data=to_plot, aes(x=n_features, y=mean, label=round(mean, 2)), nudge_y = 0.05, color=cor, size = 5, fontface='bold', family=fam) +
           scale_x_continuous(n.breaks = 10) +
           scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
           coord_cartesian(ylim = c(0, 1.02)) +
           labs(title="Proportion of nearest neighbor vs. number of features",
                subtitle="(mean ± standard deviation)",
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
  
  pdf(file=paste0("proportion_nn_experiment", exp, ".pdf"), paper="a4r")
  plot(get(paste0("p", exp)))
  dev.off()
  print(paste(exp, "plot saved"))
}

setwd(dir)

# library(patchwork)
# 
# p3 + p4 + p5 +
# p6 + p7 + p8 +
# p9 + p10 + p11 +
# p12 + p13 + p14 +
# p15 + p16 + p17
#   