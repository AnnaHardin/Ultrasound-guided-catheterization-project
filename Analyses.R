setwd("C:/Users/annam/OneDrive - Western University of Health Sciences/Current Projects/Ultrasound project")
library(tidyverse)

usdata <- read.csv("ultrasound_data.csv")

usdata2 <- usdata %>%
  mutate(pre_conf_UTS_clinical_num = 
            ifelse(pre_conf_UTS_clinical == "Strongly agree", 5,
            ifelse(pre_conf_UTS_clinical == "Somewhat agree", 4,
            ifelse(pre_conf_UTS_clinical == "Neither agree nor disagree", 3,
            ifelse(pre_conf_UTS_clinical == "Somewhat disagree", 2,
            ifelse(pre_conf_UTS_clinical == "Strongly disagree", 1, NA))))),
         pre_conf_placement_jug_carot_num = 
            ifelse(pre_conf_placement_jug_carot == "Strongly agree", 5,
            ifelse(pre_conf_placement_jug_carot == "Somewhat agree", 4,
            ifelse(pre_conf_placement_jug_carot == "Neither agree nor disagree", 3,
            ifelse(pre_conf_placement_jug_carot == "Somewhat disagree", 2,
            ifelse(pre_conf_placement_jug_carot == "Strongly disagree", 1, NA))))),
         pre_conf_placement_fem_num = 
            ifelse(pre_conf_placement_fem == "Strongly agree", 5,
            ifelse(pre_conf_placement_fem == "Somewhat agree", 4,
            ifelse(pre_conf_placement_fem == "Neither agree nor disagree", 3,
            ifelse(pre_conf_placement_fem == "Somewhat disagree", 2,
            ifelse(pre_conf_placement_fem == "Strongly disagree", 1, NA))))),
         pre_conf_placement_rad_num =
            ifelse(pre_conf_placement_rad == "Strongly agree", 5,
            ifelse(pre_conf_placement_rad == "Somewhat agree", 4,
            ifelse(pre_conf_placement_rad == "Neither agree nor disagree", 3,
            ifelse(pre_conf_placement_rad == "Somewhat disagree", 2,
            ifelse(pre_conf_placement_rad == "Strongly disagree", 1, NA))))),
         pre_future_UTS_CL_usage_num =
            ifelse(pre_future_UTS_CL_usage == "Strongly agree", 5,
            ifelse(pre_future_UTS_CL_usage == "Somewhat agree", 4,
            ifelse(pre_future_UTS_CL_usage == "Neither agree nor disagree", 3,
            ifelse(pre_future_UTS_CL_usage == "Somewhat disagree", 2,
            ifelse(pre_future_UTS_CL_usage == "Strongly disagree", 1, NA))))),
         pre_anatomy_knowledge_num =
            ifelse(pre_anatomy_knowledge == "Strongly agree", 5,
            ifelse(pre_anatomy_knowledge == "Somewhat agree", 4,
            ifelse(pre_anatomy_knowledge == "Neither agree nor disagree", 3,
            ifelse(pre_anatomy_knowledge == "Somewhat disagree", 2,
            ifelse(pre_anatomy_knowledge == "Strongly disagree", 1, NA))))),
         pre_conf_placement_livingperson_num =
            ifelse(pre_conf_placement_livingperson == "Strongly agree", 5,
            ifelse(pre_conf_placement_livingperson == "Somewhat agree", 4,
            ifelse(pre_conf_placement_livingperson == "Neither agree nor disagree", 3,
            ifelse(pre_conf_placement_livingperson == "Somewhat disagree", 2,
            ifelse(pre_conf_placement_livingperson == "Strongly disagree", 1, NA))))),
         post_conf_UTS_clinical_num = 
           ifelse(post_conf_UTS_clinical == "Strongly agree", 5,
                  ifelse(post_conf_UTS_clinical == "Somewhat agree", 4,
                         ifelse(post_conf_UTS_clinical == "Neither agree nor disagree", 3,
                                ifelse(post_conf_UTS_clinical == "Somewhat disagree", 2,
                                       ifelse(post_conf_UTS_clinical == "Strongly disagree", 1, NA))))),
         post_conf_placement_jug_carot_num = 
           ifelse(post_conf_placement_jug_carot == "Strongly agree", 5,
                  ifelse(post_conf_placement_jug_carot == "Somewhat agree", 4,
                         ifelse(post_conf_placement_jug_carot == "Neither agree nor disagree", 3,
                                ifelse(post_conf_placement_jug_carot == "Somewhat disagree", 2,
                                       ifelse(post_conf_placement_jug_carot == "Strongly disagree", 1, NA))))),
         post_conf_placement_fem_num = 
           ifelse(post_conf_placement_fem == "Strongly agree", 5,
                  ifelse(post_conf_placement_fem == "Somewhat agree", 4,
                         ifelse(post_conf_placement_fem == "Neither agree nor disagree", 3,
                                ifelse(post_conf_placement_fem == "Somewhat disagree", 2,
                                       ifelse(post_conf_placement_fem == "Strongly disagree", 1, NA))))),
         post_conf_placement_rad_num =
           ifelse(post_conf_placement_rad == "Strongly agree", 5,
                  ifelse(post_conf_placement_rad == "Somewhat agree", 4,
                         ifelse(post_conf_placement_rad == "Neither agree nor disagree", 3,
                                ifelse(post_conf_placement_rad == "Somewhat disagree", 2,
                                       ifelse(post_conf_placement_rad == "Strongly disagree", 1, NA))))),
         post_future_UTS_CL_usage_num =
           ifelse(post_future_UTS_CL_usage == "Strongly agree", 5,
                  ifelse(post_future_UTS_CL_usage == "Somewhat agree", 4,
                         ifelse(post_future_UTS_CL_usage == "Neither agree nor disagree", 3,
                                ifelse(post_future_UTS_CL_usage == "Somewhat disagree", 2,
                                       ifelse(post_future_UTS_CL_usage == "Strongly disagree", 1, NA))))),
         post_anatomy_knowledge_num =
           ifelse(post_anatomy_knowledge == "Strongly agree", 5,
                  ifelse(post_anatomy_knowledge == "Somewhat agree", 4,
                         ifelse(post_anatomy_knowledge == "Neither agree nor disagree", 3,
                                ifelse(post_anatomy_knowledge == "Somewhat disagree", 2,
                                       ifelse(post_anatomy_knowledge == "Strongly disagree", 1, NA))))),
         post_conf_placement_livingperson_num =
           ifelse(post_conf_placement_livingperson == "Strongly agree", 5,
                  ifelse(post_conf_placement_livingperson == "Somewhat agree", 4,
                         ifelse(post_conf_placement_livingperson == "Neither agree nor disagree", 3,
                                ifelse(post_conf_placement_livingperson == "Somewhat disagree", 2,
                                       ifelse(post_conf_placement_livingperson == "Strongly disagree", 1, NA))))),
         diff_conf_UTS_clinical_num = post_conf_UTS_clinical_num - pre_conf_UTS_clinical_num,
         diff_conf_placement_jug_carot_num = post_conf_placement_jug_carot_num - pre_conf_placement_jug_carot_num,
         diff_conf_placement_fem_num = post_conf_placement_fem_num - pre_conf_placement_fem_num,
         diff_conf_placement_rad_num = post_conf_placement_rad_num - pre_conf_placement_rad_num,
         diff_future_UTS_CL_usage_num = post_future_UTS_CL_usage_num - pre_future_UTS_CL_usage_num,
         diff_anatomy_knowledge_num = post_anatomy_knowledge_num - pre_anatomy_knowledge_num,
         diff_conf_placement_livingperson_num = post_conf_placement_livingperson_num - pre_conf_placement_livingperson_num)

medians <- usdata2 %>%
  summarise_at(vars(pre_conf_UTS_clinical_num:diff_conf_placement_livingperson_num), median, na.rm = TRUE)

sds <- usdata2 %>%
  summarise_at(vars(pre_conf_UTS_clinical_num:diff_conf_placement_livingperson_num), sd, na.rm = TRUE)

ggplot(data = usdata2) +
  geom_histogram(aes(x = diff_conf_placement_livingperson_num))

before <- c(usdata2$pre_conf_UTS_clinical_num[1:13])
before[14] <- usdata2$pre_conf_UTS_clinical_num[15]
after <- c(usdata2$post_conf_UTS_clinical_num[1:13])
after[14] <- usdata2$post_conf_UTS_clinical_num[15]

wilcox.test(x = before, 
            y = after,
            paired = TRUE)

wilcox.test(x = usdata2$pre_conf_placement_jug_carot_num, 
            y = usdata2$post_conf_placement_jug_carot_num,
            paired = TRUE)

wilcox.test(x = usdata2$pre_conf_placement_fem_num, 
            y = usdata2$post_conf_placement_fem_num,
            paired = TRUE)

wilcox.test(x = usdata2$pre_conf_placement_rad_num, 
            y = usdata2$post_conf_placement_rad_num,
            paired = TRUE)

wilcox.test(x = usdata2$pre_future_UTS_CL_usage_num, 
            y = usdata2$post_future_UTS_CL_usage_num,
            paired = TRUE)

wilcox.test(x = usdata2$pre_anatomy_knowledge_num, 
            y = usdata2$post_anatomy_knowledge_num,
            paired = TRUE)

wilcox.test(x = usdata2$pre_conf_placement_livingperson_num, 
            y = usdata2$post_conf_placement_livingperson_num,
            paired = TRUE)

write.csv(usdata2, "Data2.csv")
usdata3 <- read.csv("Data3.csv") %>%
  mutate(minimum = ifelse(Median - SD >= 1, Median - SD, 1),
         maximum = ifelse(Median + SD <= 5, Median + SD, 5))

usdata3$Statement <- as.character(usdata3$Statement)  
usdata3$Statement <- factor(usdata3$Statement, levels = unique(usdata3$Statement))

ggplot(data = usdata3) +
  geom_point(aes(x = Statement, y = Median, color = Timing)) +
  geom_segment(aes(x = Statement, xend = Statement, y = minimum, yend = maximum, color = Timing)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(limits = c(1, 5),
                     labels = c("Strongly disagree", "Somewhat disagree",
                                "Neither agree nor disagree",
                                "Somewhat agree", "Strongly agree"))

ggsave("Figure1.jpg", dpi = 600)
  