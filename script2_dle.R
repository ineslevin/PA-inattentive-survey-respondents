# Description: R code to recreate the empirical results, tables, and figures in section 4.2 of the paper

start2_time <- Sys.time()

load("./RecodedData.Rdata")

library(boot)
library(broom)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(stringr)
library(xtable)

# Subset of respondents who received double list experiment (DLE) questions
# Respondents who failed TQ1 were terminated before receiving the double list experiment
RecodedDataDLE <- RecodedData %>% filter(type != 'filter1') 

# 1 respondent did not answer the second list experiment question
sum(is.na(RecodedDataDLE$listA))
sum(is.na(RecodedDataDLE$listB))

# Remove the respondent who did not answer the second list experiment question and keep relevant variables for the analysis
RecodedDataDLE <- RecodedDataDLE %>%
  filter(! is.na(listA) & ! is.na(listB)) %>% 
  select(filter, treat, treatA, treatB, listA, listB, f.gender, f.educ, f.agecat, f.region, ptcp.additive, pol.know.preTQ1, IdeoScale)

#**************************************************************
# Table B8: Number of respondents in each experimental condition
#**************************************************************

TB8 <- new.env()

TB8$TableB8.count <- matrix(table(RecodedDataDLE$treat), nrow = 2, ncol = 2) %>%
  cbind(., margin.table(., margin = 1)) %>%
  rbind(., margin.table(., margin = 2))

TB8$TableB8.prop <- matrix(prop.table(table(RecodedDataDLE$treat)) %>% multiply_by(100) %>% round(1), nrow = 2, ncol = 2) %>%
  cbind(., margin.table(., margin = 1)) %>%
  rbind(., margin.table(., margin = 2))

TableB8 <- mapply(function(count, prop) str_c(count, " (", prop, "%)"), TB8$TableB8.count, TB8$TableB8.prop) %>%
  matrix(nrow = 3, ncol = 3) %>%
  set_rownames(c("Control A - Treatment B", "Treatment A - Control B", "Total")) %>%
  set_colnames(c("Organization X", "Organization Y", "Total")) %>%
  xtable()

print(TableB8, file = "TableB8.tex")

#*******************************************************************************************
# Function sum_listExp_attention
#-------------------------------------------------------------------------------------------
# Description: This function takes data from a list experiment with respondents' attentiveness
#                to produce a summary of the list experiment by attentiveness
# Inputs: (1) data: a dataframe containing the following variables, ordered (i)-(iii):
#             (i)   treatment indicator: a categorical variable indicating treatment status for each respondent
#             (ii)  list experiment response: an integer variable indicating the respondent's answer
#             (iii) filter passage indicator: a categorical variable indicating whether the respondent passed the attention filter
#             [optional] treatment indicator and list experiment response for the second list if cross.list = TRUE
#         (2) var.names: a vector of variable names corresponding to (i)-(iii) in (1)
#               (and optionally variable names corresponding to (i) and (ii) for the second list, if cross.list = TRUE)
#               [default: ("treat", "response", "filter")]
#         (3) treat.groups: a vector of control/treatment group names, in the order of control group and treatment group
#               (and optionally control/treatment group names for the second list, if cross.list = TRUE)
#               [default: (0,1)]
#         (4) filter.levels: a vector of attentive/inattentive group names, in the order of inattentives and attentives
#               [default: (0,1)]
#         (5) cross.list [optional, default = FALSE]: an indicator for whether to compute cross-list differences (for double list experiment only)
# Outputs:(1) 1000 boostrap samples of parameter estimates
#         (2) parameter estimates
#         (3) boostrapped standard errors for parameter estimates
# Parameters include:
# (1)-(3): For attentive respondents, average control response, average treated response, and the DiM estimate
# (4)-(6): For inattentive respondents, average control response, average treated response, and the DiM estimate
# (7)-(9): Attentive-inattentive differences in average control response, average treated response, and the DiM estimate
# [optional] if cross.list = TRUE,
# (10)-(18): Quantities corresponding to (1)-(9) for the second list
# (19): For attentive respondents,cross list differences in the DiM estimate
# (20): For inattentive respondents,cross list differences in the DiM estimate
#*******************************************************************************************

sum_listExp_attention <- function(data, var.names = c("treat", "response", "filter"), treat.groups = c(0, 1), filter.levels = c(0, 1), stratifying.var = "treat", cross.list = FALSE) {
  
  if (cross.list == FALSE) {
    data <- data[, var.names] %>%
      set_names(c("treat", "response", "filter"))
  } else {
    data <- data[, c(var.names, stratifying.var)] %>%
      set_names(c("treat", "response", "filter", "treat.listB", "response.listB", "strata"))
  }
  
  control <- treat.groups[1]
  treated <- treat.groups[2]
  fail <- filter.levels[1]
  pass <- filter.levels[2]
  if (cross.list == FALSE) {
  } else {
    control.listB <- treat.groups[3]
    treated.listB <- treat.groups[4]
  }
  
  func.boot <- function(data, indices) {
    d <- data[indices,]
    
    mean.control.Att <- mean(d$response[d$treat == control  & d$filter == pass]) # Att: Attentives
    mean.treated.Att <- mean(d$response[d$treat == treated & d$filter == pass])
    DiM.Att <- mean.treated.Att - mean.control.Att # DiM: difference-in-means
    
    mean.control.Inatt <- mean(d$response[d$treat == control & d$filter == fail]) # Inatt: Inattentives
    mean.treated.Inatt <- mean(d$response[d$treat == treated & d$filter == fail])
    DiM.Inatt <- mean.treated.Inatt - mean.control.Inatt
    
    AID.control <- mean.control.Att - mean.control.Inatt # AID: Difference b/t attentives and inattentives
    AID.treated <- mean.treated.Att - mean.treated.Inatt
    AID.DiM <- DiM.Att - DiM.Inatt
    
    if (cross.list == FALSE) {
      return(c(mean.control.Att, mean.treated.Att, DiM.Att,
               mean.control.Inatt, mean.treated.Inatt, DiM.Inatt,
               AID.control, AID.treated, AID.DiM))
    } else {
      mean.control.Att.listB <- mean(d$response.listB[d$treat.listB == control.listB & d$filter == pass]) # Att: Attentives
      mean.treated.Att.listB <- mean(d$response.listB[d$treat.listB == treated.listB & d$filter == pass])
      DiM.Att.listB <- mean.treated.Att.listB - mean.control.Att.listB # DiM: difference-in-means
      
      mean.control.Inatt.listB <- mean(d$response.listB[d$treat.listB == control.listB & d$filter == fail]) # Inatt: Inattentives
      mean.treated.Inatt.listB <- mean(d$response.listB[d$treat.listB == treated.listB & d$filter == fail])
      DiM.Inatt.listB <- mean.treated.Inatt.listB - mean.control.Inatt.listB
      
      AID.control.listB <- mean.control.Att.listB - mean.control.Inatt.listB # AID: Difference b/t attentives and inattentives
      AID.treated.listB <- mean.treated.Att.listB - mean.treated.Inatt.listB
      AID.DiM.listB <- DiM.Att.listB - DiM.Inatt.listB
      
      DiM.listdiff.Att <- DiM.Att.listB - DiM.Att
      DiM.listdiff.Inatt <- DiM.Inatt.listB - DiM.Inatt
      
      return(c(mean.control.Att, mean.treated.Att, DiM.Att,
               mean.control.Inatt, mean.treated.Inatt, DiM.Inatt,
               AID.control, AID.treated, AID.DiM,
               mean.control.Att.listB, mean.treated.Att.listB, DiM.Att.listB,
               mean.control.Inatt.listB, mean.treated.Inatt.listB, DiM.Inatt.listB,
               AID.control.listB, AID.treated.listB, AID.DiM.listB,
               DiM.listdiff.Att, DiM.listdiff.Inatt))
    }
  }
  
  if (cross.list == FALSE) {
    boot.params <- boot(data, statistic = func.boot, R = 1000, strata = data$treat, sim = "ordinary", simple = TRUE)
  } else {
    boot.params <- boot(data, statistic = func.boot, R = 1000, strata = data$strata, sim = "ordinary", simple = TRUE)
  }
  
  estimate.params <- boot.params$t0
  se.params <- apply(boot.params$t, 2, sd)
  return(list(boot.params$t, estimate.params, se.params))
}

set.seed(1000)
# Parameter estimates and bootstrapped standard errors for Organization X:
boutput.OrgX <- sum_listExp_attention(data = RecodedDataDLE, var.names = c("treatA", "listA", "filter", "treatB", "listB"), treat.groups = c("C_listA, T_listB", "T_OrgX_listA, C_listB", "T_listA, C_listB", "C_listA, T_OrgX_listB"), filter.levels = c("fail", "pass"), stratifying.var = "treat", cross.list = TRUE)
# Parameter estimates and bootstrapped standard errors for Organization Y:
boutput.OrgY <- sum_listExp_attention(data = RecodedDataDLE, var.names = c("treatA", "listA", "filter", "treatB", "listB"), treat.groups = c("C_listA, T_listB", "T_OrgY_listA, C_listB", "T_listA, C_listB", "C_listA, T_OrgY_listB"), filter.levels = c("fail", "pass"), stratifying.var = "treat", cross.list = TRUE)

#**************************************************************
# Figure 4: Attentiveness and difference-in-means estimates (list A)
#**************************************************************

F4 <- new.env()
F4$DiM.X.Att.estimate <- boutput.OrgX[[2]][3]
F4$DiM.X.Att.boot <- boutput.OrgX[[1]][, 3]
F4$DiM.X.Inatt.estimate <- boutput.OrgX[[2]][6]
F4$DiM.X.Inatt.boot <- boutput.OrgX[[1]][, 6]
F4$AID.DiM.X.estimate <- boutput.OrgX[[2]][9]
F4$AID.DiM.X.se <- boutput.OrgX[[3]][9]

# Figure 4 (upper panel)
Figure4_OrgX <- ggplot() +
  geom_density(mapping=aes(x=F4$DiM.X.Att.boot), linetype="solid") +
  geom_density(mapping=aes(x=F4$DiM.X.Inatt.boot), linetype="dashed") +
  scale_x_continuous(name = "Effect of the inclusion of organization X on the number of selected items", breaks = seq(-6, 14, 2)/10, limits = c(-0.6, 1.4), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", breaks = 0:6, limits = c(0, 6), expand = c(0, 0)) +
  geom_hline(yintercept=0, colour="grey", size=1) +
  geom_vline(xintercept = F4$DiM.X.Att.estimate, linetype="solid") +
  geom_vline(xintercept = F4$DiM.X.Inatt.estimate, linetype="dashed") +
  annotate("text", x = -0.25, y = 2.3, label = paste("Inattentive", sep = ""), size=2.5) +
  annotate("text", x = -0.25, y = 2.0, label = paste("mean = ",round(F4$DiM.X.Inatt.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 0.5, y = 4.8, label = paste("Attentive", sep = ""), size=2.5) +
  annotate("text", x = 0.5, y = 4.5, label = paste("mean = ",round(F4$DiM.X.Att.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.6, label = paste("Difference in effects: ", round(F4$AID.DiM.X.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.3, label = paste("(standard error = ", round(F4$AID.DiM.X.se, 1), ")", sep = ""), size=2.5) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

F4$DiM.Y.Att.estimate <- boutput.OrgY[[2]][3]
F4$DiM.Y.Att.boot <- boutput.OrgY[[1]][, 3]
F4$DiM.Y.Inatt.estimate <- boutput.OrgY[[2]][6]
F4$DiM.Y.Inatt.boot <- boutput.OrgY[[1]][, 6]
F4$AID.DiM.Y.estimate <- boutput.OrgY[[2]][9]
F4$AID.DiM.Y.se <- boutput.OrgY[[3]][9]

# Figure 4 (lower panel)
Figure4_OrgY <- ggplot() +
  geom_density(mapping=aes(x=F4$DiM.Y.Att.boot), linetype="solid") +
  geom_density(mapping=aes(x=F4$DiM.Y.Inatt.boot), linetype="dashed") +
  scale_x_continuous(name = "Effect of the inclusion of organization Y on the number of selected items", breaks = seq(-6, 14, 2)/10, limits = c(-0.6, 1.4), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", breaks = 0:6, limits = c(0, 6), expand = c(0, 0)) +
  geom_hline(yintercept=0, colour="grey", size=1) +
  geom_vline(xintercept = F4$DiM.Y.Att.estimate, linetype="solid") +
  geom_vline(xintercept = F4$DiM.Y.Inatt.estimate, linetype="dashed") +
  annotate("text", x = -0.1, y = 2.2, label = paste("Inattentive", sep = ""), size=2.5) +
  annotate("text", x = -0.1, y = 1.9, label = paste("mean = ",round(F4$DiM.Y.Inatt.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 0.35, y = 4.8, label = paste("Attentive", sep = ""), size=2.5) +
  annotate("text", x = 0.35, y = 4.5, label = paste("mean = ",round(F4$DiM.Y.Att.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.6, label = paste("Difference in effects: ", round(F4$AID.DiM.Y.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.3, label = paste("(standard error = ", round(F4$AID.DiM.Y.se, 2), ")", sep = ""), size=2.5) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

Figure4 <- arrangeGrob(Figure4_OrgX, Figure4_OrgY, ncol = 1)
ggsave("./Figure4.png", Figure4, width = 8, height = 7, units = "in", dpi = 600)

#**************************************************************
# Table B9: Attentiveness and difference-in-means estimates
#**************************************************************

TB9 <- new.env()

# Table B9 (top panel)
TB9$TableB9_ListA <- matrix(NA, nrow = 6, ncol = 5) %>%
  set_rownames(c("Attentive (A)", "Std. error (att. A)", "Inattentive (A)", "Std. error (inatt. A)", "Difference (A)", "Std. error (diff. A)")) %>%
  set_colnames(c("Mean response (control)", "Mean response (Org. X)", "Mean response (Org. Y)", "Diff.-in-means (Org. X)", "Diff.-in-means (Org. Y)")) %>%
  inset(c(1,3,5), c(1,2,4), matrix(boutput.OrgX[[2]][1:9], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), c(1,2,4), matrix(boutput.OrgX[[3]][1:9], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(1,3,5), c(1,3,5), matrix(boutput.OrgY[[2]][1:9], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), c(1,3,5), matrix(boutput.OrgY[[3]][1:9], nrow=3, ncol=3, byrow=TRUE)) %>%
  round(2)

# Table B9 (middle panel)
TB9$TableB9_ListB <- matrix(NA, nrow = 6, ncol = 5) %>%
  set_rownames(c("Attentive (B)", "Std. error (att. B)", "Inattentive (B)", "Std. error (inatt. B)", "Difference (B)", "Std. error (diff. B)")) %>%
  set_colnames(c("Mean response (control)", "Mean response (Org. X)", "Mean response (Org. Y)", "Diff.-in-means (Org. X)", "Diff.-in-means (Org. Y)")) %>%
  inset(c(1,3,5), c(1,2,4), matrix(boutput.OrgX[[2]][10:18], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), c(1,2,4), matrix(boutput.OrgX[[3]][10:18], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(1,3,5), c(1,3,5), matrix(boutput.OrgY[[2]][10:18], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), c(1,3,5), matrix(boutput.OrgY[[3]][10:18], nrow=3, ncol=3, byrow=TRUE)) %>%
  round(2)

# Table B9 (bottom panel)
TB9$TableB9_ListDiff <- matrix(NA, nrow = 4, ncol = 5) %>%
  set_rownames(c("Attentive (B vs. A)", "Std. error (att. B vs. A)", "Inattentive (B vs. A)", "Std. error (inatt. B vs. A)")) %>%
  set_colnames(c("Mean response (control)", "Mean response (Org. X)", "Mean response (Org. Y)", "Diff.-in-means (Org. X)", "Diff.-in-means (Org. Y)")) %>%
  inset(c(1,3), 4, boutput.OrgX[[2]][19:20]) %>%
  inset(c(2,4), 4, boutput.OrgX[[3]][19:20]) %>%
  inset(c(1,3), 5, boutput.OrgY[[2]][19:20]) %>%
  inset(c(2,4), 5, boutput.OrgY[[3]][19:20]) %>%
  round(2)

# Table B9 (all three panels)
TableB9 <- rbind(TB9$TableB9_ListA, TB9$TableB9_ListB, TB9$TableB9_ListDiff) %>% xtable()

print(TableB9, file = "TableB9.tex")

#**************************************************************
# Table B10: Number of selected items in double-list experiment
#**************************************************************

TB10 <- new.env()

TB10$TableB10_ListA <- RecodedDataDLE %>%
  group_by(filter, treatA) %>%
  summarise(
    Response_0 = mean((listA == 0)),
    Response_1 = mean((listA == 1)),
    Response_2 = mean((listA == 2)),
    Response_3 = mean((listA == 3)),
    Response_4 = mean((listA == 4)),
    Response_5 = mean((listA == 5)),
    obs = n(),
    mean = mean(listA),
    sd = sd(listA)
  )

TableB10_ListA <- matrix(NA, nrow = 9, ncol = 6) %>%
  set_rownames(c("0", "1", "2", "3", "4", "5", "Observations", "Mean", "Std. deviation")) %>%
  set_colnames(c("Control", "Org. X", "Org. Y", "Control", "Org. X", "Org. Y")) %>%
  inset(1:9, 1:6, t(TB10$TableB10_ListA[-(1:2)])) %>%
  round(2) %>%
  xtable()

print(TableB10_ListA, file = "TableB10_ListA.tex")
  
TB10$TableB10_ListB <- RecodedDataDLE %>%
  group_by(filter, treatB) %>%
  summarise(
    Response_0 = mean((listB == 0)),
    Response_1 = mean((listB == 1)),
    Response_2 = mean((listB == 2)),
    Response_3 = mean((listB == 3)),
    Response_4 = mean((listB == 4)),
    Response_5 = mean((listB == 5)),
    obs = n(),
    mean = mean(listB),
    sd = sd(listB)
  )

TableB10_ListB <- matrix(NA, nrow = 9, ncol = 6) %>%
  set_rownames(c("0", "1", "2", "3", "4", "5", "Observations", "Mean", "Std. deviation")) %>%
  set_colnames(c("Control", "Org. X", "Org. Y", "Control", "Org. X", "Org. Y")) %>%
  inset(1:9, 1:6, t(TB10$TableB10_ListB[-(1:2)])) %>%
  round(2) %>%
  xtable()

print(TableB10_ListB, file = "TableB10_ListB.tex")

#**************************************************************
# Table B11: Transition matrices between two lists
#**************************************************************

gen_lists_transition <- function(treat.group, filter.level){
  transition_matrix <- 
    table(RecodedDataDLE$listA[RecodedDataDLE$treat == treat.group & RecodedDataDLE$filter == filter.level],
          RecodedDataDLE$listB[RecodedDataDLE$treat == treat.group & RecodedDataDLE$filter == filter.level]) %>%
    prop.table(margin = 1) %>%
    round(2) %>%
    cbind(table(RecodedDataDLE$listA[RecodedDataDLE$treat == treat.group & RecodedDataDLE$filter == filter.level]))
  return(transition_matrix)
}

TableB11 <- list()
TableB11$XA_CB_Attentive <- gen_lists_transition("T_OrgX_listA, C_listB", "pass")
TableB11$XA_CB_Inattentive <- gen_lists_transition("T_OrgX_listA, C_listB", "fail")
TableB11$YA_CB_Attentive <- gen_lists_transition("T_OrgY_listA, C_listB", "pass")
TableB11$YA_CB_Inattentive <- gen_lists_transition("T_OrgY_listA, C_listB", "fail")
TableB11$CA_XB_Attentive <- gen_lists_transition("C_listA, T_OrgX_listB", "pass")
TableB11$CA_XB_Inattentive <- gen_lists_transition("C_listA, T_OrgX_listB", "fail")
TableB11$CA_YB_Attentive <- gen_lists_transition("C_listA, T_OrgY_listB", "pass")
TableB11$CA_YB_Inattentive <- gen_lists_transition("C_listA, T_OrgY_listB", "fail")

TableB11.labels <- c("T_OrgX_listA, C_listB, pass", "T_OrgX_listA, C_listB, fail", "T_OrgY_listA, C_listB, pass", "T_OrgY_listA, C_listB, fail", "C_listA, T_OrgX_listB, pass", "C_listA, T_OrgX_listB, fail", "C_listA, T_OrgY_listB, pass", "C_listA, T_OrgY_listB, fail")

for (i in 1:length(TableB11)) {TableB11[[i]] <- cbind(rep(TableB11.labels[i], nrow(TableB11[[i]])), 0:(nrow(TableB11[[i]]) - 1), TableB11[[i]])}

TableB11 <- rbind.fill.matrix(lapply(TableB11, function(x) {colnames(x) = c("Case type", "List A:", paste("List B:", 0:(ncol(x)-4)), "Cases"); x}))[, c("Case type", "List A:", "List B: 0", "List B: 1", "List B: 2", "List B: 3", "List B: 4", "List B: 5", "Cases")] %>%
  xtable()

print(TableB11, file = "TableB11.tex")

#**************************************************************
# Table B12: Attentiveness and difference-in-means estimates for Eady (2017)
#**************************************************************

Eady17 <- read.csv("./ListGender.csv")

set.seed(1000)
# Parameter estiamtes and bootstrapped standard errors for the sensitive item for Eady (2017):
boutput.Eady17 <- sum_listExp_attention(data = Eady17, var.names = c("listGenderTreatment", "listGender", "attentionCheck"))

TableB12 <- matrix(NA, nrow = 6, ncol = 3) %>%
  set_rownames(c("Pass Screener", "Std. error (pass)", "Fail Screener", "Std. error (fail)", "Difference", "Std. error (diff.)")) %>%
  set_colnames(c("Mean response (control)", "Mean response (treated)", "Diff.-in-means")) %>%
  inset(c(1,3,5), 1:3, matrix(boutput.Eady17[[2]], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), 1:3, matrix(boutput.Eady17[[3]], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(1,3,5), 1:3, matrix(boutput.Eady17[[2]], nrow=3, ncol=3, byrow=TRUE)) %>%
  inset(c(2,4,6), 1:3, matrix(boutput.Eady17[[3]], nrow=3, ncol=3, byrow=TRUE)) %>%
  round(2) %>%
  xtable()

print(TableB12, file = "TableB12.tex")

#**************************************************************
# Figure 5: Attentiveness and difference-in-means estimates (list B)
#**************************************************************

F5 <- new.env()
F5$DiM.X.Att.estimate <- boutput.OrgX[[2]][12]
F5$DiM.X.Att.boot <- boutput.OrgX[[1]][, 12]
F5$DiM.X.Inatt.estimate <- boutput.OrgX[[2]][15]
F5$DiM.X.Inatt.boot <- boutput.OrgX[[1]][, 15]
F5$AID.DiM.X.estimate <- boutput.OrgX[[2]][18]
F5$AID.DiM.X.se <- boutput.OrgX[[3]][18]

# Figure 5 (upper panel)
Figure5_OrgX <- ggplot() +
  geom_density(mapping=aes(x=F5$DiM.X.Att.boot), linetype="solid") +
  geom_density(mapping=aes(x=F5$DiM.X.Inatt.boot), linetype="dashed") +
  scale_x_continuous(name = "Effect of the inclusion of organization X on the number of selected items", breaks = seq(-6, 14, 2)/10, limits = c(-0.6, 1.4), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", breaks = 0:6, limits = c(0, 6), expand = c(0, 0)) +  
  geom_hline(yintercept=0, colour="grey", size=1) +
  geom_vline(xintercept = F5$DiM.X.Att.estimate, linetype="solid") +
  geom_vline(xintercept = F5$DiM.X.Inatt.estimate, linetype="dashed") +
  annotate("text", x = 0.8, y = 2.3, label = paste("Inattentive", sep = ""), size=2.5) +
  annotate("text", x = 0.8, y = 2.0, label = paste("mean = ",round(F5$DiM.X.Inatt.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 0.1, y = 4.8, label = paste("Attentive", sep = ""), size=2.5) +
  annotate("text", x = 0.1, y = 4.5, label = paste("mean = ",round(F5$DiM.X.Att.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.6, label = paste("Difference in effects: ", round(F5$AID.DiM.X.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.3, label = paste("(standard error = ", round(F5$AID.DiM.X.se, 1), ")", sep = ""), size=2.5) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

F5$DiM.Y.Att.estimate <- boutput.OrgY[[2]][12]
F5$DiM.Y.Att.boot <- boutput.OrgY[[1]][, 12]
F5$DiM.Y.Inatt.estimate <- boutput.OrgY[[2]][15]
F5$DiM.Y.Inatt.boot <- boutput.OrgY[[1]][, 15]
F5$AID.DiM.Y.estimate <- boutput.OrgY[[2]][18]
F5$AID.DiM.Y.se <- boutput.OrgY[[3]][18]

# Figure 5 (lower panel)
Figure5_OrgY <- ggplot() +
  geom_density(mapping=aes(x=F5$DiM.Y.Att.boot), linetype="solid") +
  geom_density(mapping=aes(x=F5$DiM.Y.Inatt.boot), linetype="dashed") +
  scale_x_continuous(name = "Effect of the inclusion of organization Y on the number of selected items", breaks = seq(-6, 14, 2)/10, limits = c(-0.6, 1.4), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", breaks = 0:6, limits = c(0, 6), expand = c(0, 0)) + 
  geom_hline(yintercept=0, colour="grey", size=1) +
  geom_vline(xintercept = F5$DiM.Y.Att.estimate, linetype="solid") +
  geom_vline(xintercept = F5$DiM.Y.Inatt.estimate, linetype="dashed") +
  annotate("text", x = 0.8, y = 2.3, label = paste("Inattentive", sep = ""), size=2.5) +
  annotate("text", x = 0.8, y = 2.0, label = paste("mean = ",round(F5$DiM.Y.Inatt.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 0.13, y = 4.8, label = paste("Attentive", sep = ""), size=2.5) +
  annotate("text", x = 0.13, y = 4.5, label = paste("mean = ",round(F5$DiM.Y.Att.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.6, label = paste("Difference in effects: ", round(F5$AID.DiM.Y.estimate, 2), sep = ""), size=2.5) +
  annotate("text", x = 1, y = 4.3, label = paste("(standard error = ", round(F5$AID.DiM.Y.se, 2), ")", sep = ""), size=2.5) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

Figure5 <- arrangeGrob(Figure5_OrgX, Figure5_OrgY, ncol = 1)
ggsave("./Figure5.png", Figure5, width = 8, height = 7, units = "in", dpi = 600)

#**************************************************************
# Table B13: Regression analysis of support for anti-immigrant organizations
#**************************************************************

TB13 <- new.env()

TB13$data_listA <- RecodedDataDLE %>%
  select(-treat) %>%
  mutate(respondent = rownames(.), listindex = "A", list = listA, treat = factor(treatA, labels = c("control", "Org. X", "Org. Y"))) %>%
  select(respondent, list, treat, filter, listindex, f.gender, f.educ, f.agecat, f.region, ptcp.additive, pol.know.preTQ1, IdeoScale) %>%
  na.omit()

TB13$data_listB <- RecodedDataDLE %>%
  select(-treat) %>%
  mutate(respondent = rownames(.), listindex = "B", list = listB, treat = factor(treatB, labels = c("control", "Org. X", "Org. Y"))) %>%
  select(respondent, list, treat, filter, listindex, f.gender, f.educ, f.agecat, f.region, ptcp.additive, pol.know.preTQ1, IdeoScale) %>%
  na.omit()

TB13$data <- rbind(TB13$data_listA, TB13$data_listB) %>%
  mutate(listindex = factor(listindex, labels = c("list A", "list B")))

TB13$Model1 <- lm(list ~ treat + filter + listindex + treat*filter + treat*listindex + listindex*filter + treat*filter*listindex, data=TB13$data) %>%
  summary(cluster=c("respondent"))

TB13$M1_coefficients <- TB13$Model1 %>%
  tidy() %>%
  select(term, estimate, std.error)

TB13$Model2 <- lm(list ~ treat + filter + listindex + treat*filter + treat*listindex + listindex*filter + treat*filter*listindex + f.gender + f.educ + f.agecat + f.region + treat*f.gender + treat*f.educ + treat*f.agecat + treat*f.region, data=TB13$data) %>%
  summary(cluster=c("respondent"))

TB13$M2_coefficients <- TB13$Model2 %>%
  tidy() %>%
  select(term, estimate, std.error)

TB13$Model3 <- lm(list ~ treat + filter + listindex + treat*filter + treat*listindex + listindex*filter + treat*filter*listindex + f.gender + f.educ + f.agecat + f.region + ptcp.additive + pol.know.preTQ1 + IdeoScale + treat*f.gender + treat*f.educ + treat*f.agecat + treat*f.region + treat*ptcp.additive + treat*pol.know.preTQ1 + treat*IdeoScale, data=TB13$data) %>%
  summary(cluster=c("respondent"))

TB13$M3_coefficients <- TB13$Model3 %>%
  tidy() %>%
  select(term, estimate, std.error)

TB13$main_variables <- c("treatOrg. X", "treatOrg. Y", "filterfail", "listindexlist B", "treatOrg. X:filterfail", "treatOrg. Y:filterfail", "treatOrg. X:listindexlist B", "treatOrg. Y:listindexlist B", "filterfail:listindexlist B", "treatOrg. X:filterfail:listindexlist B", "treatOrg. Y:filterfail:listindexlist B", "(Intercept)")

TableB13 <- matrix(NA, nrow = 19, ncol = 6) %>%
  set_rownames(c("Control", "Organization X", "Organization Y", "Pass", "Fail", "List A", "List B", "Org. X x Fail", "Org. Y x Fail", "Org. X x List B", "Org. Y x List B", "Fail x List B", "Org. X x Fail x List B", "Org. Y x Fail x List B", "Intercept", "Demographics", "Additional Controls", "Adj. R-squared", "N")) %>%
  set_colnames(c("Estimate", "SE", "Estimate", "SE", "Estimate", "SE")) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 1, TB13$M1_coefficients$estimate) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 2, TB13$M1_coefficients$std.error) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 3, TB13$M2_coefficients$estimate[(TB13$M2_coefficients$term %in% TB13$main_variables)]) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 4, TB13$M2_coefficients$std.error[(TB13$M2_coefficients$term %in% TB13$main_variables)]) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 5, TB13$M3_coefficients$estimate[(TB13$M3_coefficients$term %in% TB13$main_variables)]) %>%
  inset(c(15, 2:3, 5, 7, 8:14), 6, TB13$M3_coefficients$std.error[(TB13$M3_coefficients$term %in% TB13$main_variables)]) %>%
  inset(c("Adj. R-squared"), c(1,3,5), c(TB13$Model1$adj.r.squared, TB13$Model2$adj.r.squared, TB13$Model3$adj.r.squared)) %>%
  round(2) %>%
  inset(c("Demographics"), c(1,3,5), c("No", "Yes", "Yes")) %>%
  inset(c("Additional Controls"), c(1,3,5), c("No", "No", "Yes")) %>%
  inset(c("N"), c(1,3,5), str_c(c(sum(TB13$Model1$df[1:2])/2, sum(TB13$Model2$df[1:2])/2, sum(TB13$Model3$df[1:2])/2), " x 2")) %>%
  xtable()

print(TableB13, file = "TableB13.tex")

end2_time <- Sys.time()

# Total run time for script2_dle.R

end2_time - start2_time
