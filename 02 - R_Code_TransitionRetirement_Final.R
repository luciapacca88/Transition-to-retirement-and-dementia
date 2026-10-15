################################################################################
# TRANSITION-TO-RETIREMENT SEQUENCE ANALYSIS
#
# This script conducts sequence and cluster analysis of employment and
# retirement trajectories from ages 51-75 in the Health and Retirement Study.
#
# Input data are prepared in Stata and contain one employment/retirement state
# for each year of age from 51 through 75.
################################################################################

################################################################################
#Install the required packages if not already installed (uncomment if needed)

#install.packages("cluster")
#install.packages("WeightedCluster")
#install.packages("devtools")
#install.packages("TraMineR")
#install.packages("TraMineRextras")
#install.packages("ggplot2")
#install.packages("NbClust")
#install.packages("ggseqplot")
#install.packages("gridExtra")
#install.packages("Tmisc")


################################################################################
# LOAD REQUIRED PACKAGES
################################################################################

library(cluster)
library(WeightedCluster)
library(TraMineR)
library(TraMineRextras)
library(ggplot2)
library(ggseqplot)
library(gridExtra)
library(haven)

# Define project directories
project_dir <- "/Users/lpacca/Research Projects/02 - Transition to Retirement/Code Review for Jilly"

data_dir <- file.path(project_dir, "data")
figures_dir <- file.path(project_dir, "figures")

################################################################################
# DEFINE SEQUENCE STATES
################################################################################

# Numeric values correspond to the employment/retirement states created in Stata
state_alphabet <- c("1", "2", "3", 
                       "4", "5", "6")   

state_labels <- c(
  "Full-time work",
  "Part-time work",
  "Retired",
  "Disabled",
  "Not working",
  "Unreported"
)

# Colors used consistently across sequence plots
FullTimeWork <- "#0033FF"      
PartTimeWork <- "#99FFFF"  
Retired <- "#00BA38"       
Disabled <- "#FFA500"          
NotWorking <-  "#FF3300"       
Unreported <- "#666666"

################################################################################
# IMPORT EMPLOYMENT/RETIREMENT TRAJECTORIES
################################################################################

States_Wide <- read_dta(
  file.path(data_dir, "final_states_wide.dta")
)

# Extract age from the names of the state variables (state51-state75)
# and retain the participant identifier as the first column
names(States_Wide)[-1] <- stringr::str_extract(
  names(States_Wide)[-1],
  "[0-9]+"
)

View(States_Wide)

################################################################################
# DEFINE SEQUENCES
################################################################################

# Define annual employment/retirement sequences from ages 51-75.
# Missing observations are represented by the "Unreported" state (state 6)
# rather than excluded from the sequence.

States_Wide.seq <- seqdef(
  States_Wide,
  var      = 2:26,
  alphabet = state_alphabet,
  labels   = state_labels,
  start    = 51,
  left     = "6",
  right    = "6",
  gaps     = "6",
  xtstep   = 4,
  cpal     = c(
    FullTimeWork,
    PartTimeWork,
    Retired,
    Disabled,
    NotWorking,
    Unreported
  )
)

View(States_Wide.seq)

################################################################################
# VISUALIZE EMPLOYMENT/RETIREMENT SEQUENCES
################################################################################

# Sequence index plot showing individual trajectories for all participants
seqIplot(
  States_Wide.seq,
  sortv = "from.start",
  with.legend = TRUE,
  cex.legend = 1,
  main = "Sequence Index Plot",
  xlab = "Age (years)"
)

# Display sequence-state legend
seqlegend(States_Wide.seq)

################################################################################
# DEFINE SUBSTITUTION COST MATRIX
################################################################################

# Estimate substitution costs from observed transition rates.
# Costs are constant across age rather than time-varying.
costmatrix <- seqsubm(
  States_Wide.seq,
  method = "TRATE",
  time.varying = FALSE
)

# Display substitution cost matrix
costmatrix

################################################################################
# MAIN ANALYSIS: OPTIMAL MATCHING + HIERARCHICAL CLUSTERING
################################################################################

# Calculate pairwise sequence dissimilarities using Optimal Matching.
# Substitution costs are based on observed transition rates; insertion/deletion
# costs are fixed at 1.
dist_om <- seqdist(
  States_Wide.seq,
  method = "OM",
  indel = 1.0,
  sm = costmatrix
)


################################################################################
# HIERARCHICAL AGGLOMERATIVE CLUSTERING
################################################################################

# Cluster sequences using Ward's hierarchical agglomerative clustering
om_agnes <- hclust(
  as.dist(dist_om),
  method = "ward.D2"
)


################################################################################
# ASSESS CLUSTER SOLUTION QUALITY
################################################################################

# Evaluate alternative cluster solutions using cluster-quality statistics
om_agnes_c <- as.clustrange(
  om_agnes,
  diss = as.dist(dist_om)
)

summary(om_agnes_c, max.rank = 10)

# Plot Average Silhouette Width (ASW), Hubert's C (HC), and
# Calinski-Harabasz (CH) statistics across alternative cluster solutions
plot(
  om_agnes_c,
  stat = c("ASW", "HC", "CH"),
  norm = "zscore",
  lwd = 2,
  cex = 2,
  col = c("#6666ff", "#cc0000", "#008000"),
  legendpos = "topright"
)

# The 10-cluster solution was selected for the main analysis
abline(
  v = 10,
  col = "#666666",
  lty = "longdash",
  lwd = 2
)


################################################################################
# EXTRACT FINAL 10-CLUSTER SOLUTION
################################################################################
# The 10-cluster solution was selected based on cluster-quality statistics
# and substantive interpretability of the resulting retirement trajectories.

cluster10om <- cutree(om_agnes, k = 10)

################################################################################
# EXPORT FINAL 10-CLUSTER SOLUTION
#
# Export participant IDs and cluster assignments for subsequent analyses in
# Stata. Cluster numbers are reordered and descriptively labeled in Stata.
################################################################################

clusters_10_om <- data.frame(
  id = States_Wide$key,
  cluster = cluster10om
)

# Check cluster sizes before export
table(clusters_10_om$cluster)

# Export cluster assignments
write.csv(
  clusters_10_om,
  file.path(data_dir, "clusters_10_om.csv"),
  row.names = FALSE
)

################################################################################
# INSPECT FINAL 10-CLUSTER SOLUTION
#
# Cluster numbers assigned by hierarchical clustering are arbitrary. We first
# inspect the sequence patterns within each cluster before assigning descriptive
# labels and determining the presentation order.
################################################################################

# Index plots using the original cluster numbering
clusters10_index <- ggseqiplot(
  States_Wide.seq,
  group = cluster10om
) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.key.size = grid::unit(0.6, "cm"),
    legend.key = element_rect(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# Extract common legend for final figure
mylegend10 <- ggpubr::get_legend(
  clusters10_index +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical"
    )
)

print(clusters10_index)

# State-distribution plots by cluster
clusters10_chronograms <- ggseqdplot(
  States_Wide.seq,
  group = cluster10om
) +
  theme(
    legend.text = element_text(size = 10),
    legend.key.size = grid::unit(0.5, "cm"),
    legend.key = element_rect(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

print(clusters10_chronograms)

################################################################################
# REORDER AND LABEL 10-CLUSTER SOLUTION FOR PRESENTATION
#
# After inspecting the cluster-specific sequence patterns, clusters are reordered
# and descriptively labeled to facilitate interpretation. Cluster membership is
# unchanged.
################################################################################

# Original cluster numbers in final presentation order
cluster_order <- c(1, 3, 5, 8, 2, 10, 6, 9, 4, 7)

cluster_labels <- c(
  "Retired after full-time work at ages 62-65",
  "Retired before age 60",
  "Retired after full-time work after age 70",
  "Full-time to part-time to retired after age 70",
  "Retired after part-time work at ages 62-65",
  "Continuous part-time work",
  "Mostly not working",
  "Mostly disability",
  "Unreported, then retired at ages 62-65",
  "Mostly unreported"
)


################################################################################
# CREATE INDEX PLOTS IN FINAL PRESENTATION ORDER
################################################################################

cluster_plots <- lapply(seq_along(cluster_order), function(i) {
  
  cluster_seq <- States_Wide.seq[cluster10om == cluster_order[i], ]
  
  ggseqiplot(cluster_seq) +
    labs(
      title = cluster_labels[i],
      x = "Age (years)"
    ) +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 12
      ),
      legend.position = "none"
    )
})


################################################################################
# COMBINE AND SAVE FINAL CLUSTER FIGURE
################################################################################

plots <- gridExtra::arrangeGrob(
  grobs = cluster_plots,
  ncol = 3
)

legend <- gridExtra::arrangeGrob(
  mylegend10,
  heights = grid::unit(1, "npc")
)

clusters_10_om_figure <- gridExtra::grid.arrange(
  plots,
  legend,
  ncol = 2,
  widths = c(4, 1)
)

ggsave(
  filename = file.path(figures_dir, "clusters_10_om.tiff"),
  plot = clusters_10_om_figure,
  width = 14,
  height = 12,
  dpi = 300
)

#########SENSITIVITY: HAMMING ALGORITHM
# Conduct sequence analysis 
#HAMMING DISSIMILARITY MEASURE: prioritizes timing of events
dist_ham <- seqdist(States_Wide.seq,
                    method = "HAM",
                    sm= costmatrix)

# Examine the top left corner of the dissimilarity matrix
corner(dist_ham, n=15)

#Conduct Cluster Analysis
#We will be comparing two clustering agorithms: hierarchical clustering and partitioning around medoids.

#Using HIERARCHICAL, AGGLOMERATIVE CLUSTERING (fastest) 
ham_agnes <- hclust(as.dist(dist_ham), method = "ward.D2")

#Assess cluster quality
ham_agnes_c <- as.clustrange(ham_agnes, diss=as.dist(dist_ham))
summary(ham_agnes_c, max.rank=10)
plot(ham_agnes_c, stat = c("ASW","HC", "CH"), norm="zscore", lwd = 2, cex=2, col = c('#6666ff', '#cc0000', '#008000'), legendpos = "topright")
abline(v=11, col="#666666", lty="longdash", lwd = 2) #7 clusters is our preferred solution

# Cut Tree 11 clusters
cluster11 <- cutree(ham_agnes, k = 11)
seqIplot(States_Wide.seq, group=cluster11, border=NA) #index plots by cluster

# export 11 clusters solution
View(cluster11)
df_cl11 <-cluster11
write.csv(df_cl11, "/Users/lpacca/Downloads/clusters_hc_11.csv")

###############################################################################$
######REORDER AND RENAME THE CLUSTERS

clusters11 <- ggseqiplot(States_Wide.seq,  #index plots by cluster with ggplot
                         group=cluster11) + 
  theme(legend.text = element_text(size=10),
        legend.key.size = unit(0.50, 'cm'),
        legend.key = element_rect(colour="black"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

mylegend11 <- ggpubr::get_legend(
  clusters11 +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical" 
    )
)


# Reorder 11 cluster solution ----------------------------------------------

clust11 <- cluster11

one <- States_Wide.seq[which(clust11 == 1),]
clus1 <-  
  ggseqiplot(one) +
  labs(title = "1)Retired after ft work at ages 62-65",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

two <- States_Wide.seq[which(clust11 == 4),]
clus2 <-  
  ggseqiplot(two) +
  labs(title = "2)Retired before age 60",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust11 == 6),]
clus3 <-  
  ggseqiplot(three) +
  labs(title = "3)Retired after ft work after 70",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust11 == 5),]
clus4 <-  
  ggseqiplot(four) +
  labs(title = "4)Full-time to part-time to retired after age 70",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust11 == 2),]
clus5 <-  
  ggseqiplot(five) +
  labs(title = "5)Retired after pt work at ages 62-65",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust11 == 11),]
clus6 <-  
  ggseqiplot(six) +
  labs(title = "6)Continuous part-time work",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust11 == 3),]
clus7 <-  
  ggseqiplot(seven) +
  labs(title = "7)Mostly out of work",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

eight <- States_Wide.seq[which(clust11 == 9),]
clus8 <-  
  ggseqiplot(eight) +
  labs(title = "8)Mostly disability",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

nine <- States_Wide.seq[which(clust11 == 10),]
clus9 <-  
  ggseqiplot(nine) +
  labs(title = "9)Unreported to ft work, then retired after age 70",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

ten <- States_Wide.seq[which(clust11 == 7),]
clus10 <-  
  ggseqiplot(ten) +
  labs(title = "10)Unreported, then retired at ages 62-65",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

eleven <- States_Wide.seq[which(clust11 == 8),]
clus11 <-  
  ggseqiplot(eleven) +
  labs(title = "11)Mostly unreported",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

# Combine plots
plots <- arrangeGrob(
  clus1, clus2, clus3,
  clus4, clus5, clus6,
  clus7, clus8, clus9,
  clus10, clus11,
  ncol = 3
)

legend <- gridExtra::arrangeGrob(
  mylegend11,
  heights = unit(1, "npc")   # forces full height
)

clusters_11<- gridExtra::grid.arrange(
  plots, legend,
  ncol = 2,
  widths = c(4, 1)
)


ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7.png", plot = clusters_hc_11) 

# Reorder 7 cluster solution - MODAL PLOTS ------------------------------------------

one <- States_Wide.seq[which(clust7 == 9171),]
clus1 <-  
  ggseqmsplot(one) +
  labs(title = "TYPICAL RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7 == 9184),]
clus2 <-  
  ggseqmsplot(two) +
  labs(title = "EARLY RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7 == 9099),]
clus3 <-  
  ggseqmsplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7 == 8970),]
clus4 <-  
  ggseqmsplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7 == 9156),]
clus5 <-  
  ggseqmsplot(five) +
  labs(title = "DISABILITY",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7 == 1190),]
clus6 <-  
  ggseqmsplot(six) +
  labs(title = "TYPICAL RETIREMENT, INITIAL MISSINGNESS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7 == 9040),]
clus7 <-  
  ggseqmsplot(seven) +
  labs(title = "OUT OF WORK GAPS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7_modal)
# Combine plots
clusters_pam_7_modal <- gridExtra::grid.arrange(arrangeGrob(clus1, clus2, clus3,
                                                            clus4, clus5, clus6, clus7,
                                                            ncol = 2),
                                                mylegend7, nrow=2, heights=c(15, 1))

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7_modal.png", plot = clusters_pam_7_modal) 

# Reorder 7 cluster solution - CHRONOGRAMS ------------------------------------------

one <- States_Wide.seq[which(clust7 == 9171),]
clus1 <-  
  ggseqdplot(one) +
  labs(title = "TYPICAL RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7 == 9184),]
clus2 <-  
  ggseqdplot(two) +
  labs(title = "EARLY RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7 == 9099),]
clus3 <-  
  ggseqdplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7 == 8970),]
clus4 <-  
  ggseqdplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7 == 9156),]
clus5 <-  
  ggseqdplot(five) +
  labs(title = "DISABILITY",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7 == 1190),]
clus6 <-  
  ggseqdplot(six) +
  labs(title = "TYPICAL RETIREMENT, INITIAL MISSINGNESS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7 == 9040),]
clus7 <-  
  ggseqdplot(seven) +
  labs(title = "OUT OF WORK GAPS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7_chronograms)
# Combine plots
clusters_pam_7_chrono <- gridExtra::grid.arrange(arrangeGrob(clus1, clus2, clus3,
                                                             clus4, clus5, clus6, clus7,
                                                             ncol = 2),
                                                 mylegend7, nrow=2, heights=c(15, 1))

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7_chrono.png", plot = clusters_pam_7_chrono) 


