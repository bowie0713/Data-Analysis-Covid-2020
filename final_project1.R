library(ggplot2)
library(dplyr)
mysd = function(x){sd(x)*sqrt((length(x)-1)/length(x))}

file.choose()
final_data = read.csv("/Users/staceystyce/Downloads/covid.csv")
View(final_data)
# filter out United States, but include district of of Columbia
final_df = filter(final_data, Location != "United States")


#ICU beds vs Deaths in June
ggplot(final_df, aes(x = ICU.Beds, y = June_2020_Deaths, color = Location)) + 
  geom_point(alpha = 0.7, size =1) + 
  geom_smooth(method = "lm", se = F) + 
  geom_vline(xintercept = mean(final_df$ICU.Beds), color = "gray")+
  geom_abline(slope = lm.out$coefficients[2], intercept = lm.out$coefficients[1], color = "green")
  
icu_min = slice(final_df, which.min(ICU.Beds))$Location
ic_max = slice(final_df, which.max(ICU.Beds))$Location

sdx = mysd(final_df$ICU.Beds)
sdy = mysd(final_df$June_2020_Deaths)
avgx = mean(final_df$ICU.Beds)
avgy = mean(final_df$June_2020_Deaths)

r = cor(final_df$ICU.Beds, final_df$June_2020_Deaths)
slope = r*sdy/sdx
b = avgy - avgx*slope
  
lm.out = lm(June_2020_Deaths ~ ICU.Beds, final_df)
lm.out$coefficients
summary(lm.out)
# regression, 
ggplot(final_df, aes(x = June_2020_Cases, y = June_2020_Deaths, color = Status.of.Medicaid.Expansion.Decision)) + 
  geom_point(alpha = 0.7, size =1) + 
  geom_smooth(method = "lm", se = F) + 
  geom_vline(xintercept = mean(final_df$June_2020_Cases), color = "green")


adopt = filter(final_df, Status.of.Medicaid.Expansion.Decision == "Adopted")
n_adopt = filter(final_df, Status.of.Medicaid.Expansion.Decision == "Not Adopted")

ggplot(n_adopt, aes(x = June_2020_Cases, y = June_2020_Deaths, color = Location))+
  geom_point(alpha = 0.7, size =1)+
  geom_smooth(method = "lm", se = F)

ggplot(adopt, aes(x = June_2020_Cases, y = June_2020_Deaths, color = Location))+
  geom_point(alpha = 0.7, size =1)+
  geom_smooth(method = "lm", se = F)
View(n_adopt)
#regression line for those adopted
sdx_adopt = mysd(adopt$June_2020_Cases)
sdy_adopt = mysd(adopt$June_2020_Deaths)
avgx_adopt = mean(adopt$June_2020_Cases)
avgy_adopt = mean(adopt$June_2020_Deaths)

r_adopt = cor(adopt$June_2020_Cases, adopt$June_2020_Deaths)
slope_adopt = r_adopt*sdy_adopt/sdx_adopt
b_adopt = avgy_adopt - avgx_adopt*slope_adopt

lm.out_adopt = lm(June_2020_Deaths ~ June_2020_Cases, adopt)
lm.out_adopt$coefficients
summary(lm.out_adopt)

#regression line for those not adopted
sdx_nadopt = mysd(n_adopt$June_2020_Cases)
sdy_nadopt = mysd(n_adopt$June_2020_Deaths)
avgx_nadopt = mean(n_adopt$June_2020_Cases)
avgy_nadopt = mean(n_adopt$June_2020_Deaths)

r_nadopt = cor(n_adopt$June_2020_Cases, n_adopt$June_2020_Deaths)
slope_nadopt = r_adopt*sdy_adopt/sdx_adopt
b_nadopt = avgy_nadopt - avgx_nadopt*slope_nadopt

lm.out_nadopt = lm(June_2020_Deaths ~ June_2020_Cases, n_adopt)
lm.out_nadopt$coefficients
summary(lm.out_nadopt)

#boxplot for comparison in between different kinds of status of reopening


ggplot(final_df, aes(x = Status.of.Reopening, y = Oct_2020_Cases))+
  geom_boxplot(aes(fill = Status.of.Reopening)) +
  stat_boxplot(geom ='errorbar', width = 0.3)+
  geom_point()+
  coord_flip()+
  scale_fill_manual(values = c("blue", "green", "red", "brown"))+
  labs(title = "Status Of Reopening vs Number of Cases In October", x = "Status of Reopening", y = "Cases in October")

ggplot(final_df, aes(x = Status.of.Reopening, y = Oct_2020_Deaths))+
  geom_boxplot(aes(fill = Status.of.Reopening)) +
  stat_boxplot(geom ='errorbar', width = 0.3)+
  geom_point()+
  coord_flip()+
  scale_fill_manual(values = c("blue", "green", "red", "brown"))+
  labs(title = "Status Of Reopening vs Number of Deaths In October", x = "Status of Reopening", y = "Death Cases in October")

box_df_reopen = data.frame(reopened = filter(final_df, Status.of.Reopening == "Reopened")$Oct_2020_Deaths)
box_df_pwr = data.frame(pwr = filter(final_df, Status.of.Reopening == "Proceeding with Reopening")$Oct_2020_Deaths)
box_df_pause = data.frame(pause = filter(final_df, Status.of.Reopening == "Paused")$Oct_2020_Deaths)
box_df_nri = data.frame(nri = filter(final_df, Status.of.Reopening == "New Restrictions Imposed")$Oct_2020_Deaths)
summary(box_df_reopen)
summary(box_df_pwr)
summary(box_df_pause)
summary(box_df_nri)

#since  

xa = c(0,18,33, 47,61,79)
ta = c(0,1,2,3,4,5)
xb = c(0,12,29,53,85,129)
tb = c(0,1,2,3,4,5)
physicsdf1 = data.frame(xa, ta)
View(physicsdf1)
physicsdf2 = data.frame(xb, tb)
physics = data.frame(xa, xb, tb)
cols = c("distance", "time")
colnames(physicsdf1) = cols
colnames(physicsdf2) = cols

ggplot(physics, aes(tb)) + 
  geom_line(aes(y = xa), color = "green") + 
  geom_line(aes(y = xb), color = "purple") +
  geom_point(aes(y= xa), alpha = 0.7)+
  geom_point(aes(y= xb), alpha = 0.7)+
  xlab('Time (s)')+
  ylab('Distance(m)')

ggplot(physics, aes(tb)) + 
  geom_smooth(aes(y = xa), method = "lm", se = F, color = 'purple') + 
  geom_smooth(aes(y = xb), method = 'lm', se = F, color = 'skyblue') +
  geom_point(aes(y= xa), alpha = 0.7)+
  geom_point(aes(y= xb), alpha = 0.7)+
  xlab('Time(s)')+
  ylab('Position(m)')+
  scale_color_manual(values = c('purple', 'skyblue'))
  

ggplot(physics) + 
  geom_line(aes(x = ta, y = xa), color = 'green') + 
  geom_line(aes(x = tb, y = xb), color = "purple") +
  geom_point(physicsdf1, alpha = 0.7)+
  geom_point(physicsdf2, alpha = 0.7)+
  xlab('Distance (m)')+
  ylab('time(s)')
  

                                                                                      
                                                                                    
  