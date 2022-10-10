
##This project is to assess the effect of social intervention on overall PWI 
## `Paired t-test` was used to test the difference.

#Data preparation
###' data stored in spss


### Load library (if required)
# Install - only once
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr") 

library(foreign) # to read spss data
library(tidyverse) # data cleaning 
library(janitor) # column cleaning and table
library("ggpubr") # box-plot
###' read the data
z <- read.spss("C:/Users/ygelaw/OneDrive - UNSW/My drive/file_YG/R and R_Studio/help/zelalem/Analysis_ CSS New Data.sav") %>% 
  as.data.frame()
 
###' Inspect the data before analysis

hist(z$OverallPWI) # to check outlier

##' box plots

# Plot weight by group and color by group

ggboxplot(z, x = "group", y = "OverallPWI", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "OverallPWI", xlab = "Groups")


###' summary statistics
z %>% 
  group_by(AssessmentReason) %>% 
  select(AssessmentReason,OverallPWI) %>% 
  summarise(max_r = max(OverallPWI),
            min_r = min(OverallPWI),
            med_r = median(OverallPWI))

###' to rename Assessment reason into string

z <- z %>% 
  mutate(group = ifelse(AssessmentReason == 1, "before", "after"),
         Indigenous = ifelse(IndigenousStatus =="Aboriginal                                   ","Aboriginal","Non-Aboriginal"))

###' Generate hypothesis to test the change in overall PWI
# Ho: m = 0
# H1: m<= 0
# H1: m>= 0


###' a summary statistics by group and Gender
z %>% 
group_by (group, Gender) %>%
  summarise(
    count = n(),
    mean = mean(OverallPWI, na.rm = TRUE),
    sd = sd(OverallPWI, na.rm = TRUE),
    median = median (OverallPWI, na.rm = TRUE),
    IQR = IQR (OverallPWI, na.rm = TRUE)
  )

## Test hypothesis. 
#apply  `paired t-test` using base R: **t.test(x, y, paired = TRUE, alternative = "two.sided")** `where x and y are before and after measurments`

install.packages("PairedData")


# Subset OverallPWI data before intervention
before <- subset(z,  group == "before", OverallPWI,
                 drop = TRUE)
# subset OverallPWI data after intervention
after <- subset(z,  group == "after", OverallPWI,
                 drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()


# Preliminary test to check paired t-test assumptions
# Assumption 1: Are the two samples paired?
# Assumption 2: Is this a large sample? the sample size >30 
# How to check the normality?

# compute the difference
d <- with(z, 
        OverallPWI[group == "before"] - OverallPWI[group == "after"])

# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value: < 0.001

# Compute paired samples t-test

# Compute t-test
res <- t.test(OverallPWI ~ group, data = z, paired = TRUE)
res

# Test hypothesis - 
# 1: if you want to test whether the average  OverallPWI before intervention is less than the average OverallPWI after intervention, type this:

t.test(OverallPWI ~ group, data = z, paired = TRUE,
        alternative = "less")

# 2: if you want to test whether the average  OverallPWI before intervention is greater than the average OverallPWI after intervention, type this:
t.test(OverallPWI ~ group, data = z, paired = TRUE,
       alternative = "greater")

## Distributions of Overall PWI
p <- z %>% 
ggplot(aes(x = group, y = OverallPWI, fill = group)) + 
  geom_boxplot(size = .75) +  
  xlab ("Intervention") + 
    facet_grid(Gender ~ Indigenous, margins = FALSE) + 
  theme( panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "right") +
   theme_minimal()

## save the plot in `tiff` format
#tiff("ncd2.tiff", width = 2205, height = 1002, res = 300)
# Modify legend titles
p + labs(fill = "Intervention")

#dev.off()
