library(readr)

germination <- read_csv("germination.csv", col_types = cols(experimenter = col_character(),
                                                            Soil = col_factor(levels = c("1", "2")),
                                                            Water = col_factor(levels = c("50","100", "200")),
                                                            Sunlight= col_factor(levels = c("0","50", "100"))))
germination

# test

#Blocking Variable
block <- germination$experimenter

#Factors
water <- germination$water
sunlight <- germination$sunlight
soil <- germination$soil

#Response Variables for day 5
r1_num_sprouts_5 <- germination$n_sprouts_five
r2_avg_nodes_5 <- germination$avg_nodes_five
r3_avg_height_5 <- germination$avg_height_five
r4_avg_height_to_leaves_5 <- germination$avg_height_to_leaves_five

#Response Variables for day 7
r1_num_sprouts_7 <- germination$n_sprouts_seven
r2_avg_nodes_7 <- germination$avg_nodes_seven
r3_avg_height_7 <- germination$avg_height_seven
r4_avg_height_to_leaves_7 <- germination$avg_height_to_leaves_seven

#Response Variables for day 9
r1_num_sprouts_9 <- germination$n_sprouts_nine
r2_avg_nodes_9 <- germination$avg_nodes_nine
r3_avg_height_9 <- germination$avg_height_nine
r4_avg_height_to_leaves_9 <- germination$avg_height_to_leaves_nine

# ANOVA and Linear Models for Day 5
germination_model_5 <- lm(r1_num_sprouts_5~((water*sunlight*soil)+block))
anova(germination_model_5)
summary(germination_model_5)

germination_model2_5 <- lm(r2_avg_nodes_5~((water*sunlight*soil)+block))
anova(germination_model2_5)
summary(germination_model2_5)

germination_model3_5 <- lm(r3_avg_height_5~((water*sunlight*soil)+block))
anova(germination_model3_5)
summary(germination_model3_5)

germination_model4_5 <- lm(r4_avg_height_to_leaves_5~((water*sunlight*soil)+block))
anova(germination_model4_5)
summary(germination_model4_5)

# ANOVA and Linear Model for Day 7
germination_model_7 <- lm(r1_num_sprouts_7~((water*sunlight*soil)+block))
anova(germination_model_7)
summary(germination_model_7)

germination_model2_7 <- lm(r2_avg_nodes_7~((water*sunlight*soil)+block))
anova(germination_model2_7)
summary(germination_model2_7)

germination_model3_7 <- lm(r3_avg_height_7~((water*sunlight*soil)+block))
anova(germination_model3_7)
summary(germination_model3_7)

germination_model4_7 <- lm(r4_avg_height_to_leaves_7~((water*sunlight*soil)+block))
anova(germination_model4_7)
summary(germination_model4_7)


# ANOVA and Linear Model for Day 9
germination_model_9 <- lm(r1_num_sprouts_9~((water*sunlight*soil)+block))
anova(germination_model_9)
summary(germination_model_9)

germination_model2_9 <- lm(r2_avg_nodes_9~((water*sunlight*soil)+block))
anova(germination_model2_9)
summary(germination_model2_9)

germination_model3_9 <- lm(r3_avg_height_9~((water*sunlight*soil)+block))
anova(germination_model3_9)
summary(germination_model3_9)

germination_model4_9 <- lm(r4_avg_height_to_leaves_9~((water*sunlight*soil)+block))
anova(germination_model4_9)
summary(germination_model4_9)

# Residual plots and QQ Plots
library(ggplot2)
library(ggpubr)

# Day 5
# Response : r1_num_sprouts_5
plot(germination_model_5, 1)
ggqqplot(residuals(germination_model_5))

# Response : r2_avg_nodes_5
plot(germination_model2_5, 1)
ggqqplot(residuals(germination_model2_5))

# Response : r3_avg_height_5
plot(germination_model3_5, 1)
ggqqplot(residuals(germination_model3_5))

# Response : r4_avg_height_to_leaves_5
plot(germination_model4_5, 1)
ggqqplot(residuals(germination_model4_5))


# Day 7
# Response : r1_num_sprouts_7
plot(germination_model_7, 1)
ggqqplot(residuals(germination_model_7))

# Response : r2_avg_nodes_7
plot(germination_model2_7, 1)
ggqqplot(residuals(germination_model2_7))

# Response : r3_avg_height_7
plot(germination_model3_7, 1)
ggqqplot(residuals(germination_model3_7))

# Response : r4_avg_height_to_leaves_7
plot(germination_model4_7, 1)
ggqqplot(residuals(germination_model4_7))


# Day 9
# Response : r1_num_sprouts_9
plot(germination_model_9, 1)
ggqqplot(residuals(germination_model_9))

# Response : r2_avg_nodes_9
plot(germination_model2_9, 1)
ggqqplot(residuals(germination_model2_9))

# Response : r3_avg_height_9
plot(germination_model3_9, 1)
ggqqplot(residuals(germination_model3_9))

# Response : r4_avg_height_to_leaves_9
plot(germination_model4_9, 1)
ggqqplot(residuals(germination_model4_9))


#Interaction Plots
sunlight <- as.ordered(sunlight)
water <- as.ordered(water)
soil <- as.ordered(soil)

# For day 5

# Response : r1_num_sprouts_5
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r1_num_sprouts_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r1_num_sprouts_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r1_num_sprouts_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r2_avg_nodes_5
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r2_avg_nodes_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r2_avg_nodes_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r2_avg_nodes_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r3_avg_height_5
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r3_avg_height_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r3_avg_height_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r3_avg_height_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r4_avg_height_to_leaves_5
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r4_avg_height_to_leaves_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r4_avg_height_to_leaves_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r4_avg_height_to_leaves_5, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')


# For day 7

# Response : r1_num_sprouts_7
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r1_num_sprouts_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r1_num_sprouts_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r1_num_sprouts_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r2_avg_nodes_7
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r2_avg_nodes_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r2_avg_nodes_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r2_avg_nodes_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r3_avg_height_7
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r3_avg_height_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r3_avg_height_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r3_avg_height_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r4_avg_height_to_leaves_7
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r4_avg_height_to_leaves_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r4_avg_height_to_leaves_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r4_avg_height_to_leaves_7, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# For day 9

# Response : r1_num_sprouts_9
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r1_num_sprouts_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r1_num_sprouts_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r1_num_sprouts_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r2_avg_nodes_9
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r2_avg_nodes_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r2_avg_nodes_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r2_avg_nodes_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r3_avg_height_9
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r3_avg_height_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r3_avg_height_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r3_avg_height_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

# Response : r4_avg_height_to_leaves_9
plot.new()
interaction.plot(x.factor = water, trace.factor = sunlight, response = r4_avg_height_to_leaves_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = water, trace.factor = soil, response = r4_avg_height_to_leaves_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')

plot.new()
interaction.plot(x.factor = sunlight, trace.factor = soil, response = r4_avg_height_to_leaves_9, fun = mean, type="b", col=c("black", "red", "green"), pch=c(19, 17, 15), fixed=TRUE, leg.bty='o')


