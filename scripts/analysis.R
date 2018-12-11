library(ggplot2)

dat <- read.csv('data_processed/VMP_table.csv')
dat$dt <- as.POSIXct(dat$datetime)

head(dat)

ggplot(data = dat) + geom_point(aes(x = log10(eps1), y = log10(eps2)))

hist(log10(dat$eps1))                                

ggplot(data = dat) + geom_boxplot(aes(x = prof_num, y = log10(eps1), group = prof_num))

ggplot(data = dat) + geom_point(aes(x = log10(eps1), y = pressure), 
                                alpha = 0.2, colour = 'blue') +
  scale_y_reverse()


ggplot(data = dat) + geom_point(aes(x = datetime, y = pressure, c = log10(eps1))) + 
  scale_y_reverse()


#cut off at whiskers from boxplot?
var <- log10(dat$eps1)
topW <- quantile(var, probs=c(0.75)) + 1.5 * IQR(var)
botW <- quantile(var, probs=c(0.25)) - 1.5 * IQR(var)
top <- ifelse(topW>max(var), max(var), topW)
bot <- ifelse(botW<min(var), min(var), botW)
top <- -6.5 #max(pl.data$lgeps.pred)
bot <- -10.5 #min(pl.data$lgeps.pred)

#make factor variable for colours
colf = ifelse(var<bot, bot, ifelse(var>bot & var<=top, var, top))
dat$colf <- colf

pl.dat <- subset(dat, prof_num == 20)

library(shape)
p1 <- ggplot(pl.dat, aes(x=dt, y=pressure)) +
  geom_point(aes(col = colf), size=1, shape=15) +
  scale_colour_gradientn(colours = femmecol(200), na.value = "transparent", 
                         guide = guide_colorbar(title=expression(paste(epsilon, ' (W/kg)'))),
                         limits=c(min(pl.dat$colf),max(pl.dat$colf)), breaks=seq(-10,-6,1),
                         label = c(expression(paste(' ',10^-10)), expression(paste(' ', 10^-9,'  ')),
                                   expression(paste(' ',10^-8,'  ')), expression(paste(' ',10^-7,'  ')),
                                   expression(paste(' ',10^-6,'  ')))) + 
  scale_y_reverse(expand=c(0.01,0)) +
  #scale_x_continuous(breaks = seq(0, 180, by = 20), limits=c(0,187), expand=c(0,0)) +
  #scale_x_datetime(limits =lims) + #, date_breaks="12 hour", date_labels = "%D:%H") +
  labs(x="distance (km)", 
       y="depth (m)"
       ,title= "Epsilon_probe1, default settings"
  ) +
  theme_classic() + 
  theme(legend.box.margin = margin(0,0,0,0,'mm'), plot.margin = margin(3,3,3,3,'mm'),
        axis.title.x = element_blank())

p1

                                  