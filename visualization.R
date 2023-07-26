###### read in results ######
org_folder <- "E:/clinstat_dev/GMA/RuofanBie Intern 2023/"

est_res <- read.csv(paste0(org_folder, "est_results.csv"))
valid_res <- read.csv(paste0(org_folder, "valid_results.csv"))
CI_res <- read.csv(paste0(org_folder, "CI_results.csv"))

outcome_level <- c("MPSE", "MPLE", "LPSE", "LPLE")

###### bias plot ######
bias1 <- as.numeric(sub(" *\\(.*", "", est_res$est.risk.1))
bias0 <- as.numeric(sub(" *\\(.*", "", est_res$est.risk.0))
biasRD <- as.numeric(sub(" *\\(.*", "", est_res$est.RD))
outcome <- sub("_.*", "", est_res$X)
bias_res <- data.frame(outcome, est_res$method, 
                       risk1 = bias1, 
                       risk0 = bias0, 
                       RD = biasRD)
bias_res$outcome <- factor(bias_res$outcome, level = outcome_level)
 
bias_res <- bias_res[seq(1, 112, 7),]
bias_res$scenario <- rep(c(1:4), each = 4)
library(tidyr)
bias_res <- bias_res %>% pivot_longer(cols = "risk1":"RD",
                                      names_to = "estimand",
                                      values_to = "bias")
library(ggplot2)
bias_res <- bias_res[bias_res$estimand %in% c("risk0", "RD"),]
ggplot(data=bias_res, aes(x=scenario, y=bias, color=outcome)) +
  geom_line() +
  geom_point() +
  facet_wrap(~estimand) +
  scale_color_manual(values=c("#00BFFF", "#00008B", "#7FFF00", "#006400")) +
  scale_x_continuous(breaks = c(1:4),
                     labels=c("S1", "S2", "S3", "S4")) +
  theme_bw() +
  xlab("Balance Scenario") +
  guides(color = guide_legend(title = "Outcome Scenario"))

###### coverage rate plot #####
library(stringr)
cover1 <- as.numeric(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", CI_res[,3], perl=T))
cover0 <- as.numeric(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", CI_res[,4], perl=T))
coverRD <- as.numeric(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", CI_res[,5], perl=T))

len1 <- as.numeric(sub(" *\\(.*", "", CI_res[,3]))
len0 <- as.numeric(sub(" *\\(.*", "", CI_res[,4]))
lenRD <- as.numeric(sub(" *\\(.*", "", CI_res[,5]))

cvrg_res <- data.frame(outcome, method = CI_res$method, 
                       risk1 = cover1, 
                       risk0 = cover0, 
                       RD = coverRD)

cvrg_res$scenario <- rep(c(1:4), each = 28)
cvrg_res <- cvrg_res %>% pivot_longer(cols = "risk1":"RD",
                                      names_to = "estimand",
                                      values_to = "coverage")
methods <- c("Exact F", "Exact SURVEYFREQ", 
             "Asymptotic SURVEYFREQ", 
             "Robust Poisson Model",
             "Cluster Robust Poisson Model")

cvrg_res <- cvrg_res[cvrg_res$method %in% methods, ]
cvrg_res$method <- factor(cvrg_res$method, level = methods)
cvrg_res$outcome <- factor(cvrg_res$outcome, level = outcome_level)

cvrg_res <- cvrg_res %>%
            mutate(method = recode(method, 
                                   "Exact SURVEYFREQ" = "Robust Exact(SURV)",
                                   "Asymptotic SURVEYFREQ" = "Robust Wald(SURV)",
                                   "Cluster Robust Poisson Model" = "Cluster-robust Poisson Model"))
cvrg_res[cvrg_res$method == "Robust Exact(SURV)" & cvrg_res$estimand == "RD", "coverage"] <- NA

cvrg_res_S1 <- cvrg_res[cvrg_res$outcome == "MPSE",]

ggplot(data=cvrg_res, aes(x=scenario, y=coverage, color = method)) +
geom_line() +
geom_point() +
facet_wrap(~estimand + outcome) +
geom_hline(yintercept = 0.95, linetype = 2, color = "red") +
theme_bw() +
scale_color_manual(values=c("#63B8FF", "#0000EE", "#8968CD", "#9BCD9B", "#00CD00")) +
scale_x_continuous(breaks = c(1:4),
                   labels=c("S1", "S2", "S3", "S4")) +
xlab("Balance Scenario")

###### CI length plot ######

len_res <- data.frame(outcome, method = CI_res$method, 
                       risk1 = len1, 
                       risk0 = len0, 
                       RD = lenRD)
len_res$scenario <- rep(c(1:4), each = 28)
len_res$outcome_num <- rep(c(1:4), each = 7)
len_res <- len_res %>% pivot_longer(cols = "risk1":"RD",
                                      names_to = "estimand",
                                      values_to = "length")
for(i in c("risk1", "risk0", "RD")){
  for(j in c("MPSE", "MPLE", "LPSE", "LPLE")){
     if(i == "risk1" & j == "MPSE"){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- "MPSE(0.4)"
     }else if(i == "risk1" & j == "MPLE"){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- "MPLE(0.1)"
     }else if(i == "risk1" & j == "LPSE"){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- "LPSE(0.8)"
     }else if(i == "risk1" & j == "LPLE"){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- "MPLE(0.9)"
     }else if(i == "risk0" & j %in% c("MPSE", "MPLE")){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- paste0(j, "(0.5)")
     }else if(i == "risk0" & j %in% c("LPSE", "LPLE")){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- paste0(j, "(0.9)")
     }else if(i == "RD" & j %in% c("MPSE", "MPLE")){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- paste0(j, "(-0.1)")
     }else if(i == "RD" & j %in% c("LPSE", "LPLE")){
       len_res[len_res$estimand == i & len_res$outcome == j, "outcome"] <- paste0(j, "(-0.4)")
     }
  }
}

len_res <- len_res[len_res$method %in% methods, ]
len_res$method <- factor(len_res$method, level = methods)
len_res$outcome <- factor(len_res$outcome, level = unique(len_res$outcome))

library(dplyr)
len_res <- len_res %>%
           mutate(method = recode(method, 
                                  "Exact SURVEYFREQ" = "Robust Exact(SURV)",
                                  "Asymptotic SURVEYFREQ" = "Robust Wald(SURV)",
                                  "Cluster Robust Poisson Model" = "Cluster-robust Poisson Model"))

len_res[len_res$method == "Robust Exact(SURV)" & len_res$estimand == "RD", "length"] <- NA

len_res_S1 <- len_res[len_res$scenario == 4,]

ggplot(data=len_res_S1, aes(x=outcome, y=length, color = method, group = method)) +
geom_line() +
geom_point() +
facet_wrap(~estimand, scales = "free_x") +
theme_bw() +
scale_color_manual(values=c("#63B8FF", "#0000EE", "#8968CD", "#9BCD9B", "#00CD00")) +
xlab("Outcome Scenario") +
theme(legend.position = "bottom")



ggplot(data=len_res, aes(x=method, y=length, fill = outcome)) +
geom_bar(stat="identity", position=position_dodge())+
theme(axis.text.x=element_text(color = "black", size=11, angle=60, vjust=.8, hjust=0.8))+
scale_fill_manual(values=c("#00BFFF", "#00008B", "#7FFF00", "#006400"))+
facet_wrap(~estimand)

prop_res_list = prop_res_l2[[3]]
prop_res_wald <- prop_res_list[[1]]
prop_res_wald1 <- prop_res_wald[prop_res_wald$trt == 0,]

non_cover <- prop_res_wald1[(prop_res_wald1[,6] > 0.5 | prop_res_wald1[,7] < 0.5),]

ggplot(non_cover, aes(dataset_id, Proportion)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = LowerCL, ymax = UpperCL))



