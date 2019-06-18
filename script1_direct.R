# Description: R code to recreate the empirical results, tables, and figures up to (and including) section 4.1 of the paper

start1_time <- Sys.time()

load("./RecodedData.Rdata")

library(broom)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(xtable)

#########################################
# How many fail? Who fails?
#########################################

#--------------------- Table B1: How many fail?

# The table shows the number and the percentage of respondents who passed and failed trap questions (TQs).

TB1 <- new.env()
TB1$tab1 <- c(table(RecodedData$type.short)[c(2, 1)], table(RecodedData$type)[2:4])
TB1$ptab1 <- round(c(prop.table(table(RecodedData$type.short))[c(2, 1)], prop.table(table(RecodedData$type))[2:4]), 2) * 100

TableB1 <- t(rbind(TB1$tab1, TB1$ptab1)) %>%
  set_rownames(c("Pass", "Fail", "Fail TQ1", "Fail TQ2", "Fail TQ3")) %>%
  set_colnames(c("N", "%")) %>%
  xtable()

print(TableB1, file = "TableB1.tex")

#--------------------- Behavior in each filter  (Section 4, Paragraph 2)

# TQ 1

# Proportion of respondents who failed TQ1 skipped the question
RecodedData$skipTQ1 <- (RecodedData$TQ1.text == " ")
prop.table(table(RecodedData$failTQ1, RecodedData$skipTQ1), 1)

# TQ 2

# Proportion of respondents who failed TQ2 chose each option or skipped the question
# Available options: "Oppose", "Indifferent", "Support", "I don't know"
prop.table(table(RecodedData$failTQ2, RecodedData$TQ2.choice), 1)

# Does the order of TQ2 in the grid question (randomized) matter for its failure rate?
table(RecodedData$TQ2.order, RecodedData$failTQ2)
chisq.test(table(RecodedData$TQ2.order, RecodedData$failTQ2))

# Proportion of respondents who failed TQ2 chose each option or to skip for other questions in the same grid 
# Available options: "Oppose", "Indifferent", "Support", "I don't know"
prop.table(table(RecodedData$failTQ2, RecodedData$support.Obamacare), 1)
prop.table(table(RecodedData$failTQ2, RecodedData$support.RepealDADT), 1)
prop.table(table(RecodedData$failTQ2, RecodedData$support.PathLegal), 1)
prop.table(table(RecodedData$failTQ2, RecodedData$support.CarbonLimits), 1)
prop.table(table(RecodedData$failTQ2, RecodedData$support.GunControl), 1)
prop.table(table(RecodedData$failTQ2, RecodedData$support.LimitSurveillance), 1)

# TQ 3

# Proportion of respondents who failed TQ3 chose each option or skipped the question
# Available options: "Disagree strongly", "Disagree somewhat", "Neither agree nor disagree", "Agree somewhat", "Agree strongly"
prop.table(table(RecodedData$failTQ3, RecodedData$TQ3.choice), 1)

# Proportion of respondents who failed TQ3 chose each option or to skip for other questions in the same grid
# Available options: "Disagree strongly", "Disagree somewhat", "Neither agree nor disagree", "Agree somewhat", "Agree strongly"
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance1), 1)
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance2), 1)
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance3), 1)
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance4), 1)
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance5), 1)
prop.table(table(RecodedData$failTQ3, RecodedData$tolerance6), 1)

#--------------------- Table B2: Who fails?

# The table shows the percentage of respondents, within each demographic group, who passed or failed trap questions.

# Pass vs. fail

TB2 <- new.env()

TB2$tab2.short <- rbind(table(RecodedData$f.gender, RecodedData$type.short))
TB2$tab3.short <- rbind(table(RecodedData$f.educ, RecodedData$type.short))
TB2$tab4.short <- rbind(table(RecodedData$f.agecat, RecodedData$type.short))
TB2$tab5.short <- rbind(table(RecodedData$f.region, RecodedData$type.short))

chisq.test(TB2$tab2.short)
chisq.test(TB2$tab3.short)
chisq.test(TB2$tab4.short)
chisq.test(TB2$tab5.short)

TB2$ptab2.short <- round(prop.table(TB2$tab2.short, 1), 3) * 100
TB2$ptab3.short <- round(prop.table(TB2$tab3.short, 1), 3) * 100
TB2$ptab4.short <- round(prop.table(TB2$tab4.short, 1), 3) * 100
TB2$ptab5.short <- round(prop.table(TB2$tab5.short, 1), 3) * 100

TB2$ptab.short <- rbind(TB2$ptab2.short, TB2$ptab3.short, TB2$ptab4.short, TB2$ptab5.short)[,c(2, 1)]

# Pass vs. fail, TQ 1

TB2$tab2.TQ1 <- rbind(table(RecodedData$f.gender, RecodedData$failTQ1))
TB2$tab3.TQ1 <- rbind(table(RecodedData$f.educ, RecodedData$failTQ1))
TB2$tab4.TQ1 <- rbind(table(RecodedData$f.agecat, RecodedData$failTQ1))
TB2$tab5.TQ1 <- rbind(table(RecodedData$f.region, RecodedData$failTQ1))

chisq.test(TB2$tab2.TQ1)
chisq.test(TB2$tab3.TQ1)
chisq.test(TB2$tab4.TQ1)
chisq.test(TB2$tab5.TQ1)

TB2$ptab2.TQ1 <- round(prop.table(TB2$tab2.TQ1, 1), 3) * 100
TB2$ptab3.TQ1 <- round(prop.table(TB2$tab3.TQ1, 1), 3) * 100
TB2$ptab4.TQ1 <- round(prop.table(TB2$tab4.TQ1, 1), 3) * 100
TB2$ptab5.TQ1 <- round(prop.table(TB2$tab5.TQ1, 1), 3) * 100

TB2$ptab.TQ1 <- rbind(TB2$ptab2.TQ1, TB2$ptab3.TQ1, TB2$ptab4.TQ1, TB2$ptab5.TQ1)

# Pass vs. fail, TQ 2

TB2$tab2.TQ2 <- rbind(table(RecodedData$f.gender, RecodedData$failTQ2))
TB2$tab3.TQ2 <- rbind(table(RecodedData$f.educ, RecodedData$failTQ2))
TB2$tab4.TQ2 <- rbind(table(RecodedData$f.agecat, RecodedData$failTQ2))
TB2$tab5.TQ2 <- rbind(table(RecodedData$f.region, RecodedData$failTQ2))

chisq.test(TB2$tab2.TQ2)
chisq.test(TB2$tab3.TQ2)
chisq.test(TB2$tab4.TQ2)
chisq.test(TB2$tab5.TQ2)

TB2$ptab2.TQ2 <- round(prop.table(TB2$tab2.TQ2, 1), 3) * 100
TB2$ptab3.TQ2 <- round(prop.table(TB2$tab3.TQ2, 1), 3) * 100
TB2$ptab4.TQ2 <- round(prop.table(TB2$tab4.TQ2, 1), 3) * 100
TB2$ptab5.TQ2 <- round(prop.table(TB2$tab5.TQ2, 1), 3) * 100

TB2$ptab.TQ2 <- rbind(TB2$ptab2.TQ2, TB2$ptab3.TQ2, TB2$ptab4.TQ2, TB2$ptab5.TQ2)

# Pass vs. fail, TQ 3

TB2$tab2.TQ3 <- rbind(table(RecodedData$f.gender, RecodedData$failTQ3))
TB2$tab3.TQ3 <- rbind(table(RecodedData$f.educ, RecodedData$failTQ3))
TB2$tab4.TQ3 <- rbind(table(RecodedData$f.agecat, RecodedData$failTQ3))
TB2$tab5.TQ3 <- rbind(table(RecodedData$f.region, RecodedData$failTQ3))

chisq.test(TB2$tab2.TQ3)
chisq.test(TB2$tab3.TQ3)
chisq.test(TB2$tab4.TQ3)
chisq.test(TB2$tab5.TQ3)

TB2$ptab2.TQ3 <- round(prop.table(TB2$tab2.TQ3, 1), 3) * 100
TB2$ptab3.TQ3 <- round(prop.table(TB2$tab3.TQ3, 1), 3) * 100
TB2$ptab4.TQ3 <- round(prop.table(TB2$tab4.TQ3, 1), 3) * 100
TB2$ptab5.TQ3 <- round(prop.table(TB2$tab5.TQ3, 1), 3) * 100

TB2$ptab.TQ3 <- rbind(TB2$ptab2.TQ3, TB2$ptab3.TQ3, TB2$ptab4.TQ3, TB2$ptab5.TQ3)

# Table B2

TableB2 <- cbind(TB2$ptab.short, TB2$ptab.TQ1, TB2$ptab.TQ2, TB2$ptab.TQ3) %>%
  rbind(c(table(RecodedData$type.short)[c(2,1)], table(RecodedData$failTQ1), table(RecodedData$failTQ2), table(RecodedData$failTQ3))) %>%
  set_rownames(c(rownames(.)[1:nrow(.)-1], "N")) %>%
  set_colnames(c("Pass all", "Fail any", "Pass TQ1", "Fail TQ1", "Pass TQ2", "Fail TQ2", "Pass TQ3", "Fail TQ3")) %>%
  xtable()

print(TableB2, file = "TableB2.tex")

#########################################
# How do failers behave?
#########################################

#--------------------- Table B3: How do failers behave?

# The table shows the average response time, non-attitudes rate, straightlining rate, incomplete preferences rate, intransitive preferences rate among respondents who passed and failed trap questions (all, then each).

TB3 <- new.env()

#--------------------- Speed

# Time between submitting page 2 of knowledge questions, and first click in page 1 of knowledge questions

TB3$speed.short <- aggregate(RecodedData[,c("speed.know")], by = list(RecodedData$type.short), mean, na.rm = T)[, 2]
TB3$speed.TQ1 <- aggregate(RecodedData[,c("speed.know")], by = list(RecodedData$failTQ1), mean, na.rm = T)[, 2]
TB3$speed.TQ2 <- aggregate(RecodedData[,c("speed.know")], by = list(RecodedData$failTQ2), mean, na.rm = T)[, 2]
TB3$speed.TQ3 <- aggregate(RecodedData[,c("speed.know")], by = list(RecodedData$failTQ3), mean, na.rm = T)[, 2]

t.test(RecodedData$speed.know[RecodedData$type.short == 1], RecodedData$speed.know[RecodedData$type.short == 0])
t.test(RecodedData$speed.know[RecodedData$failTQ1 == 1], RecodedData$speed.know[RecodedData$failTQ1 == 0])
t.test(RecodedData$speed.know[RecodedData$failTQ2 == 1], RecodedData$speed.know[RecodedData$failTQ2 == 0])
t.test(RecodedData$speed.know[RecodedData$failTQ3 == 1], RecodedData$speed.know[RecodedData$failTQ3 == 0])

TB3$speed.row <- t(c(TB3$speed.short[c(2, 1)], TB3$speed.TQ1, TB3$speed.TQ2, TB3$speed.TQ3))

#--------------------- Non-attitudes

TB3$nonatt.short <- aggregate(RecodedData[,c("nonattitude_prop")], by = list(RecodedData$type.short), mean, na.rm = T)[, 2]
TB3$nonatt.TQ1 <- aggregate(RecodedData[,c("nonattitude_prop")], by = list(RecodedData$failTQ1), mean, na.rm = T)[, 2]
TB3$nonatt.TQ2 <- aggregate(RecodedData[,c("nonattitude_prop")], by = list(RecodedData$failTQ2), mean, na.rm = T)[, 2]
TB3$nonatt.TQ3 <- aggregate(RecodedData[,c("nonattitude_prop")], by = list(RecodedData$failTQ3), mean, na.rm = T)[, 2]

t.test(RecodedData$nonattitude_prop[RecodedData$type.short == 1], RecodedData$nonattitude_prop[RecodedData$type.short == 0])
t.test(RecodedData$nonattitude_prop[RecodedData$failTQ1 == 1], RecodedData$nonattitude_prop[RecodedData$failTQ1 == 0])
t.test(RecodedData$nonattitude_prop[RecodedData$failTQ2 == 1], RecodedData$nonattitude_prop[RecodedData$failTQ2 == 0])
t.test(RecodedData$nonattitude_prop[RecodedData$failTQ3 == 1], RecodedData$nonattitude_prop[RecodedData$failTQ3 == 0])

TB3$nonatt.row <- t(c(TB3$nonatt.short[c(2, 1)], TB3$nonatt.TQ1, TB3$nonatt.TQ2, TB3$nonatt.TQ3))

#--------------------- Straightlining

TB3$straightlining.TQ2 <- aggregate(RecodedData[,c("straightlining")], by = list(RecodedData$failTQ2), mean, na.rm = T)[, 2]
TB3$straightlining.TQ3 <- aggregate(RecodedData[,c("straightlining")], by = list(RecodedData$failTQ3), mean, na.rm = T)[, 2]

t.test(RecodedData$straightlining[RecodedData$failTQ2 == 1], RecodedData$straightlining[RecodedData$failTQ2 == 0])
t.test(RecodedData$straightlining[RecodedData$failTQ3 == 1], RecodedData$straightlining[RecodedData$failTQ3 == 0])

TB3$straightlining.row <- t(c(c(NA, NA), c(NA, NA), TB3$straightlining.TQ2, TB3$straightlining.TQ3))

#--------------------- Rationality / consistency of strict pairwise orderings

TB3$rationality.short <- t(aggregate(RecodedData[,c("incomplete", "intransitive")], by = list(RecodedData$type.short), mean, na.rm = T)[, c(2, 3)])
TB3$rationality.TQ1 <- t(aggregate(RecodedData[,c("incomplete", "intransitive")], by = list(RecodedData$failTQ1), mean, na.rm = T)[, c(2, 3)])
TB3$rationality.TQ2 <- t(aggregate(RecodedData[,c("incomplete", "intransitive")], by = list(RecodedData$failTQ2), mean, na.rm = T)[, c(2, 3)])
TB3$rationality.TQ3 <- t(aggregate(RecodedData[,c("incomplete", "intransitive")], by = list(RecodedData$failTQ3), mean, na.rm = T)[, c(2, 3)])

t.test(RecodedData$incomplete[RecodedData$type.short == 1], RecodedData$incomplete[RecodedData$type.short == 0])
t.test(RecodedData$incomplete[RecodedData$failTQ1 == 1], RecodedData$incomplete[RecodedData$failTQ1 == 0])
t.test(RecodedData$incomplete[RecodedData$failTQ2 == 1], RecodedData$incomplete[RecodedData$failTQ2 == 0])
t.test(RecodedData$incomplete[RecodedData$failTQ3 == 1], RecodedData$incomplete[RecodedData$failTQ3 == 0])

t.test(RecodedData$intransitive[RecodedData$type.short == 1], RecodedData$intransitive[RecodedData$type.short == 0])
t.test(RecodedData$intransitive[RecodedData$failTQ1 == 1], RecodedData$intransitive[RecodedData$failTQ1 == 0])
t.test(RecodedData$intransitive[RecodedData$failTQ2 == 1], RecodedData$intransitive[RecodedData$failTQ2 == 0])
t.test(RecodedData$intransitive[RecodedData$failTQ3 == 1], RecodedData$intransitive[RecodedData$failTQ3 == 0])

TB3$rationality.rows <- cbind(TB3$rationality.short[, c(2, 1)], TB3$rationality.TQ1, TB3$rationality.TQ2, TB3$rationality.TQ3)

# Table B3

TableB3 <- rbind(TB3$speed.row, TB3$nonatt.row * 100, TB3$straightlining.row*100, TB3$rationality.rows * 100)  %>%
  round(1) %>%
  rbind(c(table(RecodedData$type.short)[c(2,1)], table(RecodedData$failTQ1), table(RecodedData$failTQ2), table(RecodedData$failTQ3))) %>%
  set_rownames(c("Average response speed (s)", "Non-attitudes rate (%)", "Straightlining (%)", "Incomplete preferences (%)", "Intransitive preferences (%)", "N")) %>%
  set_colnames(c("Pass all", "Fail any", "Pass TQ1", "Fail TQ1", "Pass TQ2", "Fail TQ2", "Pass TQ3", "Fail TQ3")) %>%
  xtable()

print(TableB3, file = "TableB3.tex")

##############################################
# Dependent variable: Political Knowledge
##############################################

#--------------------- attentiveness and political knowledge  (Section 4.1, Paragraph 1)

# majority pass ammendments

prop.table(table(RecodedData$type == "complete", RecodedData$majority.amend.choice), 1)

# majority to raise taxes

prop.table(table(RecodedData$type == "complete", RecodedData$majority.taxes.choice), 1)

# party senate

prop.table(table(RecodedData$type == "complete", RecodedData$party.senate.choice), 1)

# party assembly

prop.table(table(RecodedData$type == "complete", RecodedData$party.assembly.choice), 1)

#--------------------- Figure 1

Figure1_density <- ggplot(data = RecodedData[, c("pol.know.preTQ1", "type.TQ2_TQ3_comb")]) +
  stat_density(mapping = aes(pol.know.preTQ1, linetype = type.TQ2_TQ3_comb), geom = "line", position = "identity", adjust = 2) +
scale_x_continuous(name = "Political knowledge scale", breaks = 0:4, limits = c(0, 4)) +
scale_y_continuous(name = "Density", breaks = seq(0, 0.4, 0.1), limits = c(0, 0.4)) +
  labs(title = "Kernel density for political knowledge", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.title=element_blank(), legend.position = c(0.7, 0.88)) +
  theme(text = element_text(size=10), legend.text=element_text(size=8), legend.key.width = unit(1, "cm"), legend.key.height = unit(0.35, "cm"))

Figure1_qqplot <- ggplot(data = as.data.frame(qqplot(jitter(RecodedData$pol.know.preTQ1)[RecodedData$type.TQ2_TQ3_comb == "Attentive"], jitter(RecodedData$pol.know.preTQ1)[RecodedData$type.TQ2_TQ3_comb == "Inattentive (fail TQ 2/3)"], plot.it = FALSE))) +
  geom_point(aes(x=x, y=y)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(name = "Attentive", breaks = 0:4, limits = c(-0.2, 4.2)) +
  scale_y_continuous(name = "Inattentive (fail TQ 2/3)", breaks = 0:4, limits = c(-0.2, 4.2)) +
  labs(title = "Kernel density for political knowledge", linetype = "Respondent Type") +  
  labs(title = "Q-Q plot for political knowledge", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(text = element_text(size=10))

Figure1 <- arrangeGrob(Figure1_density, Figure1_qqplot, nrow = 1)
ggsave("./Figure1.png", Figure1, width = 8, height = 3.75, units = "in", dpi = 600)

#--------------------- Table B4

TB4 <- new.env()

TB4$data.know <- na.omit(subset(RecodedData, select = c("pol.know.preTQ1", "female", "educ", "age", "f.region", "type")))

TB4$know1 <- lm(pol.know.preTQ1 ~ type, data=TB4$data.know)
TB4$know2 <- lm(pol.know.preTQ1 ~ type + educ + age + female + f.region, data=TB4$data.know)
TB4$know3 <- lm(pol.know.preTQ1 ~ educ + age + female + f.region, data=TB4$data.know)

TB4$know1_coefficients <- TB4$know1 %>% tidy() %>% select(term, estimate, std.error)
TB4$know2_coefficients <- TB4$know2 %>% tidy() %>% select(term, estimate, std.error)
TB4$know3_coefficients <- TB4$know3 %>% tidy() %>% select(term, estimate, std.error)

TableB4 <- matrix(NA, nrow = 14, ncol = 6) %>%
  set_rownames(c("Intercept", "Fail TQ 1", "Fail TQ 2", "Fail TQ 3", "Education", "Age", "Female", "SoCal (exc LA)", "SoCal (LA)", "Central/Southern Farm", "North and Mountain", "Central Valley", "Adj. R-squared", "N")) %>%
  set_colnames(c("Estimate", "SE", "Estimate", "SE", "Estimate", "SE")) %>%
  inset(1:4, 1, TB4$know1_coefficients$estimate) %>%
  inset(1:4, 2, TB4$know1_coefficients$std.error) %>%
  inset(1:12, 3, TB4$know2_coefficients$estimate) %>%
  inset(1:12, 4, TB4$know2_coefficients$std.error) %>%
  inset(c(1, 5:12), 5, TB4$know3_coefficients$estimate) %>%
  inset(c(1, 5:12), 6, TB4$know3_coefficients$std.error) %>%
  inset(c("Adj. R-squared"), c(1,3,5), c(summary(TB4$know1)$adj.r.squared, summary(TB4$know2)$adj.r.squared, summary(TB4$know3)$adj.r.squared)) %>%
  round(2) %>%
  inset(c("N"), c(1,3,5), c(nobs(TB4$know1), nobs(TB4$know2), nobs(TB4$know3))) %>%
  xtable()

anova(TB4$know1, TB4$know2)
anova(TB4$know2, TB4$know3)

print(TableB4, file = "TableB4.tex")

#########################################
# Dependent variable: Participation
#########################################

#--------------------- attentiveness and political participation  (Section 4.1, Paragraph 3)

prop.table(table(RecodedData$type, RecodedData$ptcp.additive), 1)

# easy conventional

prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.vote.gral), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.vote.pry), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.online), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.sticker), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.petition), 1)

# more demanding conventional

prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.polmeet), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.wkcampaign), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.donate), 1)

# unconventional

prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.boycott), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.protest), 1)
prop.table(table(RecodedData$type == "complete", RecodedData$ptcp.sitin), 1)

#--------------------- Figure 2

Figure2_density <- ggplot(data = RecodedData[, c("ptcp.additive", "type.TQ2_TQ3_comb")]) +
  stat_density(mapping = aes(ptcp.additive, linetype = type.TQ2_TQ3_comb), geom = "line", position = "identity", adjust = 1.5) +
  scale_x_continuous(name = "Political participation scale", breaks = seq(0, 12, 2), limits = c(0, 12)) +
  scale_y_continuous(name = "Density", breaks = seq(0, 0.25, 0.05), limits = c(0, 0.25)) +
  labs(title = "Kernel density for political participation", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.title=element_blank(), legend.position = c(0.7, 0.88)) +
  theme(text = element_text(size=10), legend.text=element_text(size=8), legend.key.width = unit(1, "cm"), legend.key.height = unit(0.35, "cm"))

Figure2_qqplot <- ggplot(data = as.data.frame(qqplot(jitter(RecodedData$ptcp.additive)[RecodedData$type.TQ2_TQ3_comb == "Attentive"], jitter(RecodedData$ptcp.additive)[RecodedData$type.TQ2_TQ3_comb == "Inattentive (fail TQ 2/3)"], plot.it = FALSE))) +
  geom_point(aes(x=x, y=y)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(name = "Attentive", breaks = seq(0, 12, 2), limits = c(-0.2, 12.2)) +
  scale_y_continuous(name = "Inattentive (fail TQ 2/3)", breaks = seq(0, 12, 2), limits = c(-0.2, 12.2)) +
  labs(title = "Q-Q plot for political participation", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(text = element_text(size=10))

Figure2 <- arrangeGrob(Figure2_density, Figure2_qqplot, nrow = 1)
ggsave("./Figure2.png", Figure2, width = 8, height = 3.75, units = "in", dpi = 600)

#--------------------- Table B5

TB5 <- new.env()

TB5$data.ptcp <- na.omit(subset(RecodedData, select = c("ptcp.additive", "female", "educ", "age", "f.region", "type")))

TB5$ptcp1 <- lm(ptcp.additive ~ type, data=TB5$data.ptcp)
TB5$ptcp2 <- lm(ptcp.additive ~ type + educ + age + female + f.region, data=TB5$data.ptcp)
TB5$ptcp3 <- lm(ptcp.additive ~ educ + age + female + f.region, data=TB5$data.ptcp)

TB5$ptcp1_coefficients <- TB5$ptcp1 %>% tidy() %>% select(term, estimate, std.error)
TB5$ptcp2_coefficients <- TB5$ptcp2 %>% tidy() %>% select(term, estimate, std.error)
TB5$ptcp3_coefficients <- TB5$ptcp3 %>% tidy() %>% select(term, estimate, std.error)

TableB5 <- matrix(NA, nrow = 14, ncol = 6) %>%
  set_rownames(c("Intercept", "Fail TQ 1", "Fail TQ 2", "Fail TQ 3", "Education", "Age", "Female", "SoCal (exc LA)", "SoCal (LA)", "Central/Southern Farm", "North and Mountain", "Central Valley", "Adj. R-squared", "N")) %>%
  set_colnames(c("Estimate", "SE", "Estimate", "SE", "Estimate", "SE")) %>%
  inset(1:4, 1, TB5$ptcp1_coefficients$estimate) %>%
  inset(1:4, 2, TB5$ptcp1_coefficients$std.error) %>%
  inset(1:12, 3, TB5$ptcp2_coefficients$estimate) %>%
  inset(1:12, 4, TB5$ptcp2_coefficients$std.error) %>%
  inset(c(1, 5:12), 5, TB5$ptcp3_coefficients$estimate) %>%
  inset(c(1, 5:12), 6, TB5$ptcp3_coefficients$std.error) %>%
  inset(c("Adj. R-squared"), c(1,3,5), c(summary(TB5$ptcp1)$adj.r.squared, summary(TB5$ptcp2)$adj.r.squared, summary(TB5$ptcp3)$adj.r.squared)) %>%
  round(2) %>%
  inset(c("N"), c(1,3,5), c(nobs(TB5$ptcp1), nobs(TB5$ptcp2), nobs(TB5$ptcp3))) %>%
  xtable()

anova(TB5$ptcp1, TB5$ptcp2)
anova(TB5$ptcp2, TB5$ptcp3)

print(TableB5, file = "TableB5.tex")

########################################################
# Dependent variable: Ideology
########################################################

#--------------------- attentiveness and positions on policy issues  (Section 4.1, Paragraph 4)

# Obamacare
prop.table(table(RecodedData$type == "complete", RecodedData$support.Obamacare), 1) * 100
sum(RecodedData$support.Obamacare[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.Obamacare[RecodedData$type == "complete"] == "Don't know")

# Repeal DADT
prop.table(table(RecodedData$type == "complete", RecodedData$support.RepealDADT), 1) * 100
sum(RecodedData$support.RepealDADT[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.RepealDADT[RecodedData$type == "complete"] == "Don't know")

# Path legal
prop.table(table(RecodedData$type == "complete", RecodedData$support.PathLegal), 1) * 100
sum(RecodedData$support.PathLegal[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.PathLegal[RecodedData$type == "complete"] == "Don't know")

# Carbon Limits
prop.table(table(RecodedData$type == "complete", RecodedData$support.CarbonLimits), 1) * 100
sum(RecodedData$support.CarbonLimits[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.CarbonLimits[RecodedData$type == "complete"] == "Don't know")

# Gun Control
prop.table(table(RecodedData$type == "complete", RecodedData$support.GunControl), 1) * 100
sum(RecodedData$support.GunControl[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.GunControl[RecodedData$type == "complete"] == "Don't know")

# Limit Surveillance
prop.table(table(RecodedData$type == "complete", RecodedData$support.LimitSurveillance), 1) * 100
sum(RecodedData$support.LimitSurveillance[RecodedData$type == "complete"] == "Indifferent")/sum(RecodedData$support.LimitSurveillance[RecodedData$type == "complete"] == "Don't know")

#--------------------- Figure 3

Figure3_density <- ggplot(data = RecodedData[, c("IdeoScale", "type.TQ2_TQ3_comb")]) +
  stat_density(mapping = aes(IdeoScale, linetype = type.TQ2_TQ3_comb), geom = "line", position = "identity", adjust = 1.3) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_x_continuous(name = "Ideology score \n(-6 = support all liberal policies; \n6 = oppose all liberal policies)", breaks = seq(-6, 6, 2), limits = c(-6, 6)) +
  scale_y_continuous(name = "Density", breaks = seq(0, 0.30, 0.1), limits = c(0, 0.30)) +
  labs(title = "Kernel density for ideological leanings", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.title=element_blank(), legend.position = c(0.7, 0.88)) +
  theme(text = element_text(size=10), legend.text=element_text(size=8), legend.key.width = unit(1, "cm"), legend.key.height = unit(0.35, "cm"))

Figure3_qqplot <- ggplot(data = as.data.frame(qqplot(jitter(RecodedData$IdeoScale)[RecodedData$type.TQ2_TQ3_comb == "Attentive"], jitter(RecodedData$IdeoScale)[RecodedData$type.TQ2_TQ3_comb == "Inattentive (fail TQ 2/3)"], plot.it = FALSE))) +
  geom_point(aes(x=x, y=y)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(name = "Attentive \n \n", breaks = seq(-6, 6, 2), limits = c(-6.2, 6.2)) +
  scale_y_continuous(name = "Inattentive (fail TQ 2/3)", breaks = seq(-6, 6, 2), limits = c(-6.2, 6.2)) +
  labs(title = "Q-Q plot for ideological leanings", linetype = "Respondent Type") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(text = element_text(size=10))

Figure3 <- arrangeGrob(Figure3_density, Figure3_qqplot, nrow = 1)
ggsave("./Figure3.png", Figure3, width = 8, height = 4, units = "in", dpi = 600)

#--------------------- Table B6

TB6 <- new.env()

TB6$data.ideo <- na.omit(subset(RecodedData, select = c("IdeoScale.strgth", "female", "educ", "age", "f.region", "type")))

TB6$ideo1 <- lm(IdeoScale.strgth ~ type, data=TB6$data.ideo)
TB6$ideo2 <- lm(IdeoScale.strgth ~ type + educ + age + female + f.region, data=TB6$data.ideo)
TB6$ideo3 <- lm(IdeoScale.strgth ~ educ + age + female + f.region, data=TB6$data.ideo)

TB6$ideo1_coefficients <- TB6$ideo1 %>% tidy() %>% select(term, estimate, std.error)
TB6$ideo2_coefficients <- TB6$ideo2 %>% tidy() %>% select(term, estimate, std.error)
TB6$ideo3_coefficients <- TB6$ideo3 %>% tidy() %>% select(term, estimate, std.error)

TableB6 <- matrix(NA, nrow = 13, ncol = 6) %>%
  set_rownames(c("Intercept", "Fail TQ 2", "Fail TQ 3", "Education", "Age", "Female", "SoCal (exc LA)", "SoCal (LA)", "Central/Southern Farm", "North and Mountain", "Central Valley", "Adj. R-squared", "N")) %>%
  set_colnames(c("Estimate", "SE", "Estimate", "SE", "Estimate", "SE")) %>%
  inset(1:3, 1, TB6$ideo1_coefficients$estimate) %>%
  inset(1:3, 2, TB6$ideo1_coefficients$std.error) %>%
  inset(1:11, 3, TB6$ideo2_coefficients$estimate) %>%
  inset(1:11, 4, TB6$ideo2_coefficients$std.error) %>%
  inset(c(1, 4:11), 5, TB6$ideo3_coefficients$estimate) %>%
  inset(c(1, 4:11), 6, TB6$ideo3_coefficients$std.error) %>%
  inset(c("Adj. R-squared"), c(1,3,5), c(summary(TB6$ideo1)$adj.r.squared, summary(TB6$ideo2)$adj.r.squared, summary(TB6$ideo3)$adj.r.squared)) %>%
  round(2) %>%
  inset(c("N"), c(1,3,5), c(nobs(TB6$ideo1), nobs(TB6$ideo2), nobs(TB6$ideo3))) %>%
  xtable()

anova(TB6$ideo1, TB6$ideo2)
anova(TB6$ideo2, TB6$ideo3)

print(TableB6, file = "TableB6.tex")

end1_time <- Sys.time()

# Total run time for script1_direct.R

end1_time - start1_time
