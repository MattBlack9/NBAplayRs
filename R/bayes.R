library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

curry <- read.csv('curry_t.csv')
kobe <- read.csv('kobe_t.csv')
lebron <- read.csv('lebron_t.csv')
malone <- read.csv('malone_t.csv')
michael <- read.csv('michael_t.csv')
pippin <- read.csv('pippin_t.csv')
stock <- read.csv('stock_t.csv')

curry_reg <- read.csv('curry_reg.csv')
curry_post <- read.csv('curry_post.csv')
kobe_reg <- read.csv('kobe_reg.csv')
kobe_post <- read.csv('kobe_post.csv')
lebron_reg <- read.csv('lebron_reg.csv')
lebron_post <- read.csv('lebron_post.csv')
malone_reg <- read.csv('malone_reg.csv')
malone_post <- read.csv('malone_post.csv')
michael_reg <- read.csv('michael_reg.csv')
michael_post <- read.csv('michael_post.csv')
pippin_reg <- read.csv('pippin_reg.csv')
pippin_post <- read.csv('pippin_post.csv')
stock_reg <- read.csv('stock_reg.csv')
stock_post <- read.csv('stock_post.csv')


### Cleaning the Data ###

curry_reg <- curry_reg$x
curry_post <- curry_post$x
kobe_reg <- kobe_reg$x
kobe_post <- kobe_post$x
lebron_reg <- lebron_reg$x
lebron_post <- lebron_post$x
malone_reg <- malone_reg$x
malone_post <- malone_post$x
michael_reg <- michael_reg$x
michael_post <- michael_post$x
pippin_reg <- pippin_reg$x
pippin_post <- pippin_post$x
stock_reg <- stock_reg$x
stock_post <- stock_post$x


### Setting directory of the file ###

setwd('/Users/matty/Documents/BYU/undergrad_research/stats')


############################
### Creating my analysis ###
############################

t.test(VORP ~ time, curry)
t.test(VORP ~ time, kobe)
t.test(VORP ~ time, lebron)
t.test(VORP ~ time, malone)
t.test(VORP ~ time, michael)
t.test(VORP ~ time, pippin)
t.test(VORP ~ time, stock)

 

dens_reg <- density(michael_reg)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.09))

dens_post <- density(michael_post)
lines(dens_post, col = 'red')
legend('topright', legend = c('reg', 'post'), col = c('black', 'red'), lty = 1)

















