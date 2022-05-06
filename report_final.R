dataset = read.csv("https://raw.githubusercontent.com/Angallia/final/main/data%20grapes%20wall%20csv.csv")
#This final script was designed to be uploaded to GitHub for grading- originally intended to be a markdown
#file, I couldn't make it work ): this was modified from using local files, hopefully 
#it will work with github and there will be no errors


#I decided to examine this data as two hypotheses: the data has three "treatments", the control
#and 2 parts of the barrier. The idea of the project is to determine if a barrier is effective
# at excluding SLF from accumulating in vineyard borders, and if it is, to further examine if treated netting
#reduces # of SLF more than untreated netting.

# Hypothesis 1 2021
library("dplyr")
library("ggplot2")
dataset.h1 = subset(dataset, Treatment != "No Net")

dataset.h1.factor = dataset.h1 %>%
  mutate_at(vars(Treatment),
            list(factor))

#this is ANOVA among the treatments excluding the control

hyp_1_anov = aov(SLF~Treatment, data = dataset.h1.factor)
summary(hyp_1_anov)

#P = .155


#Hypothesis is true

# Hypothesis 2 2021
dataset.h2 = subset(dataset, Treatment != "Treated Net")

dataset.h2.factor = dataset.h2 %>%
  mutate_at(vars(Treatment),
            list(factor))

#ANOVA for untreated barrier against the control, excluding treated net

hyp_2_anov = aov(SLF~Treatment, data = dataset.h2.factor)
summary(hyp_2_anov)
#             Df Sum Sq Mean Sq F value Pr(>F)  

#P = .0163
#Low P value is strong evidence against null hypothesis


#ATTEMPT TO ANALYZE PREVIOUS DATA FROM 2020

dataset_2020 = read.csv("https://raw.githubusercontent.com/Angallia/final/main/2020%20heather%20data%20csv.csv")
#HYPOTHESIS 1 2020
#nullH = no difference in # of SLF between treated and untreated net
#altH = Treated net has fewer # SLF compared to untreated net

dataset.h1.2020 = subset(dataset_2020, Treatment != "No Net")

dataset.h1.2020.factor = dataset.h1.2020 %>%
  mutate_at(vars(Treatment),
            list(factor))

#this is ANOVA among the treatments excluding the control

hyp_1_anov.2020 = aov(SLF~Treatment, data = dataset.h1.2020.factor)
summary(hyp_1_anov.2020)
#P = .336

#HYPOTHESIS 2 2020
#nullH  = no difference between control and untreated # SLF
#altH = untreated net has fewer # SLF than control (no net)

dataset.h2.2020 = subset(dataset_2020, Treatment != "Treated")

dataset.h2.2020.factor = dataset.h2.2020 %>%
  mutate_at(vars(Treatment),
            list(factor))

#this is ANOVA among the untreated net and control

hyp_2_anov.2020 = aov(SLF~Treatment, data = dataset.h2.2020.factor)
summary(hyp_2_anov.2020)
#P = .00366

#Must check for normality. Will happen below

#Calculate N. Number of SLF counted for each treatment and dataset

treatedsum2020 = sum(dataset_2020[dataset_2020$Treatment=="Treated", "SLF"])
# 2977
no.netsum2020 = sum(dataset_2020[dataset_2020$Treatment=="No Net", "SLF"])
# 4310
untreatedsum2020 = sum(dataset_2020[dataset_2020$Treatment=="Untreated", "SLF"])
# 3262
totalsum2020 = (untreatedsum2020 + no.netsum2020 + treatedsum2020)
#10549

#The difference between untreated and no net was less than I expected... 285


treatedsum2021 = sum(dataset[dataset$Treatment=="Treated Net", "SLF"])
# 3818
no.netsum2021 = sum(dataset[dataset$Treatment=="No Net", "SLF"])
# 2288
untreatedsum2021 = sum(dataset[dataset$Treatment=="Untreated Net", "SLF"])
# 3199
totalsum2021 = (treatedsum2021+no.netsum2021+untreatedsum2021)
# 9305

#check for normality...
dataset.normality = subset(dataset, select = SLF)
dataset.2020.normality = subset(dataset_2020, select = SLF)
barplot(as.numeric(dataset.normality$SLF))
barplot(as.numeric(dataset.2020.normality$SLF))

test = aov(SLF ~ Treatment, data = dataset)
plot(test)

qqplot(test)

test2020 = aov(SLF ~ Treatment, data =dataset_2020)
plot(test2020)

#this shows the data is not normal, so we can't use one way anova. Must use Kruskal Wallis

#2021 data, hyp 1, untreated against treated (removed control)
kruskal.test(dataset.h1$SLF, dataset.h1$Treatment)

#P = .9343
#NullHyp accepted


#2021 data, hyp 2, untreated against control (removed treated)
kruskal.test(dataset.h2$SLF, dataset.h2$Treatment)

#P value .045
#NullHyp rejected

#2020 data, hyp 1, untreated against treated (removed control)
kruskal.test(dataset.h1.2020$SLF, dataset.h1.2020$Treatment)

#P = 0.009654
#NullHyp rejected

#2020 data, hyp 2, untreated against control (removed treated)
kruskal.test(dataset.h2.2020$SLF, dataset.h2.2020$Treatment)

#P = 0.1926
#NullHyp accepted

#time to plot... Data is skewed, so boxplot may be the best choice. 

boxplot(SLF~Treatment, data = dataset, xlab = "Treatment", ylab = "Number SLF", main = "Number of SLF per Treatment 2021", col=c("blue","orange", "gray"))
#looks hideous. Data not normal

boxplot(SLF~Treatment, data = dataset_2020, xlab = "Treatment", ylab = "Number SLF", main = "Number of SLF per Treatment 2020", col=c("blue","orange", "gray"))
#also hideous. Data not normal


#is it normal with only the first 5 rows?
firstrows2021 = read.csv("https://raw.githubusercontent.com/Angallia/final/main/2021%20first%20few%20rows.csv")
test2 = aov(SLF ~ Treatment, data = firstrows2021)
plot(test2)
#still not normal ):
#potential to further evaluate the data with elimination of certain rows, to see if efficacy is different
#immediately adjacent to the wall