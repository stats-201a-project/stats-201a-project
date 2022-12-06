library(readr)

data = read_csv('data.csv', col_types = cols(experimenter = col_character(),
                                              soil = col_factor(levels = c('1', '2')),
                                              water = col_factor(levels = c('1', '2', '3')),
                                              sunlight= col_factor(levels = c('1', '2', '3'))))
print(data)

# blocking variable
block = data$experimenter

# factors
soil = data$soil
water = data$water
sunlight = data$sunlight

# response variables for day 5
r1_n_sprouts_5 = data$n_sprouts_5
r2_max_nodes_5 = data$max_nodes_5
r3_max_height_5 = data$max_height_5

# response variables for day 7
r1_n_sprouts_7 = data$n_sprouts_7
r2_max_nodes_7 = data$max_nodes_7
r3_max_height_7 = data$max_height_7

# response variables for day 9
r1_n_sprouts_9 = data$n_sprouts_9
r2_max_nodes_9 = data$max_nodes_9
r3_max_height_9 = data$max_height_9


# ANOVA and linear models for day 5
model_n_sprouts_5 = lm(r1_n_sprouts_5~((soil*water*sunlight)+block))
anova(model_n_sprouts_5)
summary(model_n_sprouts_5)
model_max_nodes_5 = lm(r2_max_nodes_5~((soil*water*sunlight)+block))
anova(model_max_nodes_5)
summary(model_max_nodes_5)
model_max_height_5 = lm(r3_max_height_5~((soil*water*sunlight)+block))
anova(model_max_height_5)
summary(model_max_height_5)

# ANOVA and linear models for day 7
model_n_sprouts_7 = lm(r1_n_sprouts_7~((soil*water*sunlight)+block))
anova(model_n_sprouts_7)
summary(model_n_sprouts_7)
model_max_nodes_7 = lm(r2_max_nodes_7~((soil*water*sunlight)+block))
anova(model_max_nodes_7)
summary(model_max_nodes_7)
model_max_height_7 = lm(r3_max_height_7~((soil*water*sunlight)+block))
anova(model_max_height_7)
summary(model_max_height_7)

# ANOVA and linear models for day 9
model_n_sprouts_9 = lm(r1_n_sprouts_9~((soil*water*sunlight)+block))
anova(model_n_sprouts_9)
summary(model_n_sprouts_9)
model_max_nodes_9 = lm(r2_max_nodes_9~((soil*water*sunlight)+block))
anova(model_max_nodes_9)
summary(model_max_nodes_9)
model_max_height_9 = lm(r3_max_height_9~((soil*water*sunlight)+block))
anova(model_max_height_9)
summary(model_max_height_9)


# residual and QQ plots
library(ggplot2)
library(ggpubr)

# day 5
# response: r1_n_sprouts_5
plot(model_n_sprouts_5, 1)
ggqqplot(residuals(model_n_sprouts_5), title='r1_n_sprouts_5')
# response: r2_max_nodes_5
plot(model_max_nodes_5, 1)
ggqqplot(residuals(model_max_nodes_5), title='r2_max_nodes_5')
# response: r3_max_height_5
plot(model_max_height_5, 1)
ggqqplot(residuals(model_max_height_5), title='r3_max_height_5')

# day 7
# response: r1_n_sprouts_7
plot(model_n_sprouts_7, 1)
ggqqplot(residuals(model_n_sprouts_7), title='r1_n_sprouts_7')
# response: r2_max_nodes_7
plot(model_max_nodes_7, 1)
ggqqplot(residuals(model_max_nodes_7), title='r2_max_nodes_7')
# response: r3_max_height_7
plot(model_max_height_7, 1)
ggqqplot(residuals(model_max_height_7), title='r3_max_height_7')

# day 9
# response: r1_n_sprouts_9
plot(model_n_sprouts_9, 1)
ggqqplot(residuals(model_n_sprouts_9), title='r1_n_sprouts_9')
# response: r2_max_nodes_9
plot(model_max_nodes_9, 1)
ggqqplot(residuals(model_max_nodes_9), title='r2_max_nodes_9')
# response: r3_max_height_9
plot(model_max_height_9, 1)
ggqqplot(residuals(model_max_height_9), title='r3_max_height_9')


# interaction plots
soil = as.ordered(soil)
water = as.ordered(water)
sunlight = as.ordered(sunlight)

# day 5
# response: r1_n_sprouts_5
interaction.plot(x.factor=water, trace.factor=soil, response=r1_n_sprouts_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r1_n_sprouts_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r1_n_sprouts_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r2_max_nodes_5
interaction.plot(x.factor=water, trace.factor=soil, response=r2_max_nodes_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r2_max_nodes_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r2_max_nodes_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r3_max_height_5
interaction.plot(x.factor=water, trace.factor=soil, response=r3_max_height_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r3_max_height_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r3_max_height_5, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# day 7
# response: r1_n_sprouts_7
interaction.plot(x.factor=water, trace.factor=soil, response=r1_n_sprouts_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r1_n_sprouts_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r1_n_sprouts_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r2_max_nodes_7
interaction.plot(x.factor=water, trace.factor=soil, response=r2_max_nodes_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r2_max_nodes_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r2_max_nodes_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r3_max_height_7
interaction.plot(x.factor=water, trace.factor=soil, response=r3_max_height_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r3_max_height_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r3_max_height_7, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# day 9
# response: r1_n_sprouts_9
interaction.plot(x.factor=water, trace.factor=soil, response=r1_n_sprouts_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r1_n_sprouts_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r1_n_sprouts_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r2_max_nodes_9
interaction.plot(x.factor=water, trace.factor=soil, response=r2_max_nodes_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r2_max_nodes_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r2_max_nodes_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
# response: r3_max_height_9
interaction.plot(x.factor=water, trace.factor=soil, response=r3_max_height_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=soil, response=r3_max_height_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
interaction.plot(x.factor=sunlight, trace.factor=water, response=r3_max_height_9, fun=mean, type='b', col=c('blue', 'green', 'red'), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')
