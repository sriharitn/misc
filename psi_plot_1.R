library(raincloudplots)
library(dplyr)



urlfile <- "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-04-13/HiSCR_dat.csv"

dat <- read.csv(urlfile)
head(dat)

plot(density(dat[dat$TRT=="PBO",1] ))

abscesses_base_act <- dat[dat$TRT=="ACT","abscesses.base"]
abscesses_base_pbo <- dat[dat$TRT=="PBO","abscesses.base"]

abscesses_w16_act <- dat[dat$TRT=="ACT","abscesses.w16"]
abscesses_w16_pbo <- dat[dat$TRT=="PBO","abscesses.w16"]

# 1 x 1 Plot


df_1x1 <- data_1x1(
  array_1 = abscesses_base_act,
  array_2 = abscesses_w16_act,
  jit_distance = .09,
  jit_seed = 400)

figure_1x1_rm <- raincloud_1x1_repmes(df_1x1, align_clouds = T) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("Baseline", "Week 16"), limits=c(0, 3)) +
  xlab("") + 
  ylab("# of Abscesses") + theme_classic()+
  theme(text = element_text(size = 15))

figure_1x1_rm


### 2 x 2 plot

Both_act_pbo_together <- data_2x2(
  array_1 = abscesses_base_act, 
  array_2 = abscesses_w16_act, 
  array_3 = abscesses_base_pbo, 
  array_4 = abscesses_w16_pbo, 
  labels = (c('Base','Week 16')),
  jit_distance = .09,
  jit_seed = 321,
  spread_x_ticks = TRUE) 


Both_act_pbo_together %>%
  group_by(group,x_axis) %>%
  summarise(mean = mean(y_axis), n = n())


figure_2x2_1.0 <- raincloud_2x2_repmes(Both_act_pbo_together, spread_x_ticks = TRUE) +
  
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c("Active Baseline", "Active Week 16", "Placebo Baseline", "Placebo Week 16"),
                     limits=c(0, 5)) +
  xlab("") +
  ylab("# of Abscesses") +
  theme_classic() +theme(text = element_text(size = 15),
                         plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
                         plot.caption.position =  "plot") + labs(title = "# of Abscesses for Active and Placebo Group")

figure_2x2_1.0





