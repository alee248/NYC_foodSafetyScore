library(ggplot2)
library(dplyr)
options(stringsAsFactors=F) 
doh <- read.csv("NYC.csv")

doh$SCORE<-as.numeric(doh$SCORE)
doh$year <- substring(c(doh$GRADE.DATE),7,10) # This takes string and only keeps the characters beginning in position 7 to position 10
str(doh)

year2019 <- subset(doh, year=="2019") # Create a subset for just restaurants graded in year 2019
str(year2019)
str(year2019$BORO)

m2019 <- subset(year2019, BORO=="Manhattan") # Create a subset for just restaurants in Manhattan graded in 2019, this will be our main dataset
table(m2019$year)
m2019

m2019$SCORE1 <- m2019$SCORE
m2019$SCORE1[m2019$SCORE<0] <- NA #score1 == cleaned score data of m2019
summary(m2019$SCORE1)

# council district
ggplot(m2019, aes(as.factor(Council.District))) + geom_bar()+
  labs(x="Manhattan Council District", y="Number of observations", 
  title="Figure 2. Number of Food Inspection 
       Grades Recorded by Council District") 

m2019cd <- m2019 %>%
  group_by(Council.District) %>%
  mutate(avg=mean(SCORE1, na.rm=T))

m2019cd

ggplot(m2019cd, aes(x = as.factor(Council.District), y = avg))+
  geom_point()+labs(x="Manhattan Council District", y="Mean Food Safety Score", 
                    title="Figure 3: mean score by coucil districts in Manhattan")


# cuisine type
str(m2019$CUISINE.DESCRIPTION)
table(m2019$CUISINE.DESCRIPTION)

table(m2019$CUISINE.DESCRIPTION)

m2019c <- m2019 %>%
  group_by(m2019$CUISINE.DESCRIPTION) %>%
  mutate(count = n()) %>%
  mutate(avg=mean(SCORE1, na.rm=T)) %>%
  filter(count >= 1000) %>%
  arrange(desc(count))

table(m2019c$CUISINE.DESCRIPTION)
m2019c

ggplot(m2019c, aes(x = CUISINE.DESCRIPTION)) +  
  geom_bar(aes(y = (..count..)))+labs(x = "Cuisine Type", y = "Frequency",  
  title = "Figure 4: Frequency of Different Cuisine Restaurants in Manhattan")



ggplot(m2019c, aes(x = as.factor(CUISINE.DESCRIPTION), y = avg))+
  geom_point()+labs(x="Cuisine Type", y="Mean Food Violation Score", 
                    title="Figure 5: Mean Food Violation Score by Cuisine Type in Manhattan")

#2 Using the two explanatory variables, provide a summary table of the outcome using group_by(), 
# arrange(), and summarize() functions. 
summary_m2019 <- m2019c %>%
  group_by(CUISINE.DESCRIPTION, Council.District) %>%
  arrange(SCORE1) %>%
  summarise(avg = mean(SCORE1, na.rm = T))
summary_m2019

#3 (2 points). Produce an appropriate plot using all three variables. 
#Make sure all of your variables (at least 3) are clearly displayed in the plot. 
ggplot(summary_m2019,aes(x = as.factor(CUISINE.DESCRIPTION), y = avg))+geom_point(aes(color=CUISINE.DESCRIPTION))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(x="Cuisine Type",y="Mean Food Violation Score", title="Figure 6: Mean Score of Cuisine Type by Council District")+
  facet_wrap(~Council.District)

#6 (2 points). Produce another plot reflecting the modifications you made in Question 5. Describe the plot again. 
#Be sure to specifically describe how this plot is different from the previous one.
summary1_m2019_mod <- summary_m2019 %>%
  filter(Council.District < 8)
summary_m2019_mod
ggplot(summary_m2019_mod,aes(x = as.factor(CUISINE.DESCRIPTION), y = avg))+geom_point(aes(color=CUISINE.DESCRIPTION))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(x="Cuisine Type",y="Mean Food Violation Score", title="Figure 7: Mean Score of Cuisine Type by Council Districts 1-7")+
  facet_wrap(~Council.District)

#8. Generate another summary table and plot using all of the variables you chose. 
#Describe how this plot looks different from the previous ones.
table(m2019$VIOLATION.CODE)
m2019vc <- m2019c %>%
  group_by(VIOLATION.CODE) %>%
  mutate(count = n()) %>%
  mutate(avg=mean(SCORE1, na.rm=T)) %>%
  filter(count >= 1000) %>%
  arrange(desc(count))
m2019vc
table(m2019vc$VIOLATION.CODE)

#summary table
summary2_m2019 <- m2019vc %>%
  group_by(CUISINE.DESCRIPTION, VIOLATION.CODE) %>%
  arrange(SCORE1) %>%
  summarise(avg = mean(SCORE1, na.rm = T))
summary2_m2019

#plot of all variables
ggplot(summary2_m2019,aes(x = as.factor(CUISINE.DESCRIPTION), y = avg))+geom_point(aes(color=CUISINE.DESCRIPTION))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(x="Cuisine Type",y="Mean Food Violation Score", title="Figure 8: Mean Score of Cuisine Type by Top5 Violation Code")+
  facet_wrap(~VIOLATION.CODE)



