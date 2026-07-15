## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tseffects)
ADL.finite <- pulse.calculator(x.vrbl = c("mood" = 0, "l1_mood" = 1, "l2_mood" = 2), 
               y.vrbl = NULL,
               limit = 5)

## -----------------------------------------------------------------------------
ADL1.2.pulses <- pulse.calculator(x.vrbl = c("mood" = 0, "l1_mood" = 1, "l2_mood" = 2), 
               y.vrbl = c("l1_policy" = 1),
               limit = 5)

## -----------------------------------------------------------------------------
general.calculator(d.x = 0, d.y = 0, h = -1, limit = 5, pulses = ADL1.2.pulses)

## -----------------------------------------------------------------------------
ADL1.2.d.pulses <- pulse.calculator(x.vrbl = c("d_mood" = 0, "l1_d_mood" = 1, "l2_d_mood" = 2), 
               y.vrbl = c("l1_policy" = 1),
               limit = 5)

## -----------------------------------------------------------------------------
general.calculator(d.x = 1, d.y = 0, h = -1, limit = 5, pulses = ADL1.2.d.pulses)

## -----------------------------------------------------------------------------
general.calculator(d.x = 1, d.y = 0, h = 1, limit = 5, pulses = ADL1.2.d.pulses)

## -----------------------------------------------------------------------------
data(toy.ts.interaction.data)

# Fit an ADL(1, 1)
model.adl <- lm(y ~ l_1_y + x + l_1_x, data = toy.ts.interaction.data)

test.pulse <- GDRF.adl.plot(model = model.adl,
                                   x.vrbl = c("x" = 0, "l_1_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   d.x = 0, 
                                   d.y = 0,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = TRUE)
  

## -----------------------------------------------------------------------------
test.pulse2 <- GDRF.adl.plot(model = model.adl,
                                   x.vrbl = c("x" = 0, "l_1_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   d.x = 0, 
                                   d.y = 0,
                                   shock.history = "step", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = TRUE)
  

## -----------------------------------------------------------------------------
data(toy.ts.interaction.data)

# Fit an ADL(1, 1)
model.adl.diffs <- lm(y ~ l_1_y + d_x + l_1_d_x, data = toy.ts.interaction.data)

## -----------------------------------------------------------------------------
GDRF.adl.plot(model = model.adl.diffs,
                                   x.vrbl = c("d_x" = 0, "l_1_d_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   d.x = 1, 
                                   d.y = 0,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   s.limit = 20, 
                                   return.plot = TRUE)

## -----------------------------------------------------------------------------
GDRF.adl.plot(model = model.adl.diffs,
                                   x.vrbl = c("d_x" = 0, "l_1_d_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   d.x = 1, 
                                   d.y = 0,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "differences",
                                   s.limit = 20, 
                                   return.plot = TRUE)

## -----------------------------------------------------------------------------
# Fit an ADL(1, 1)
model.adl <- lm(y ~ l_1_y + x + l_1_x, data = toy.ts.interaction.data)

test.pulse <- GDRF.adl.plot(model = model.adl,
                                   x.vrbl = c("x" = 0, "l_1_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   d.x = 0, 
                                   d.y = 0,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   effect.type = "fitted",
                                   prediction.values = list("x" = mean(toy.ts.interaction.data$x, na.rm = TRUE), 
                                                            "l_1_x" = mean(toy.ts.interaction.data$x, na.rm = TRUE)),
                                   shock.size = 1,
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = TRUE)

## -----------------------------------------------------------------------------
# Fit a model in differences
model.differences <- lm(d_y ~ l_1_d_y + x + l_1_x, data = toy.ts.interaction.data)

GDRF.adl.plot(model = model.differences,
                                   x.vrbl = c("x" = 0, "l_1_x" = 1), 
                                   y.vrbl = c("l_1_d_y" = 1),
                                   d.x = 0, 
                                   d.y = 1,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   effect.type = "fitted",
                                   baseline.y = 3,
                                   shock.size = 1,
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = FALSE)

## -----------------------------------------------------------------------------
GDRF.adl.plot(model = model.differences,
                                   x.vrbl = c("x" = 0, "l_1_x" = 1), 
                                   y.vrbl = c("l_1_d_y" = 1),
                                   d.x = 0, 
                                   d.y = 1,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   effect.type = "fitted",
                                   baseline.y = 3,
                                   baseline.y.se = sd(toy.ts.interaction.data$y, na.rm = TRUE),
                                   shock.size = 1,
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = FALSE)

## -----------------------------------------------------------------------------
# Fit a GECM(1, 1)
model.gecm <- lm(d_y ~ l_1_y + l_1_d_y + l_1_x + d_x + l_1_d_x, data = toy.ts.interaction.data)

## -----------------------------------------------------------------------------
gecm.pulse <- GDRF.gecm.plot(model = model.gecm,
                                   x.vrbl = c("l_1_x" = 1), 
                                   y.vrbl = c("l_1_y" = 1),
                                   x.vrbl.d.x = 0, 
                                   y.vrbl.d.y = 0,
                                   x.d.vrbl = c("d_x" = 0, "l_1_d_x" = 1),
                                   y.d.vrbl = c("l_1_d_y" = 1),
                                   x.d.vrbl.d.x = 1,
                                   y.d.vrbl.d.y = 1,
                                   shock.history = "pulse", 
                                   inferences.y = "levels", 
                                   inferences.x = "levels",
                                   s.limit = 20, 
                                   return.plot = TRUE, 
                                   return.formulae = TRUE)

## -----------------------------------------------------------------------------
data(toy.ts.interaction.data)

# Fit an ADL(1, 1)
interact.model <- lm(y ~ l_1_y + x + l_1_x + z + l_1_z +
		x_z + z_l_1_x +
		x_l_1_z + l_1_x_l_1_z, data = toy.ts.interaction.data)

## -----------------------------------------------------------------------------
interact.adl.plot(model = interact.model, x.vrbl = c("x" = 0, "l_1_x" = 1), 
					y.vrbl = c("l_1_y" = 1), 
					z.vrbl = c("z" = 0, "l_1_z" = 1),
					x.z.vrbl = c("x_z" = 0, "z_l_1_x" = 1, "x_l_1_z" = 0, "l_1_x_l_1_z" = 1),
					z.vals = -1:1,
					effect.type = "impulse", plot.type = "lines", line.options = "z.lines",
					s.limit = 20)

## -----------------------------------------------------------------------------
interact.adl.plot(model = interact.model, x.vrbl = c("x" = 0, "l_1_x" = 1), 
					y.vrbl = c("l_1_y" = 1), 
					z.vrbl = c("z" = 0, "l_1_z" = 1),
					x.z.vrbl = c("x_z" = 0, "z_l_1_x" = 1, "x_l_1_z" = 0, "l_1_x_l_1_z" = 1),
					z.vals = c(-1, 1),
					s.vals = c(0, 5),
					effect.type = "impulse", plot.type = "lines", line.options = "s.lines",
					s.limit = 20)

## -----------------------------------------------------------------------------
interact.adl.plot(model = interact.model, x.vrbl = c("x" = 0, "l_1_x" = 1), 
					y.vrbl = c("l_1_y" = 1), 
					z.vrbl = c("z" = 0, "l_1_z" = 1),
					x.z.vrbl = c("x_z" = 0, "z_l_1_x" = 1, "x_l_1_z" = 0, "l_1_x_l_1_z" = 1),
					z.vals = c(-1, 1),
					s.vals = c(0, 5),
					effect.type = "step", plot.type = "lines", line.options = "s.lines",
					s.limit = 20)

## -----------------------------------------------------------------------------
interact.adl.plot(model = interact.model, x.vrbl = c("x" = 0, "l_1_x" = 1), 
					y.vrbl = c("l_1_y" = 1), 
					z.vrbl = c("z" = 0, "l_1_z" = 1),
					x.z.vrbl = c("x_z" = 0, "z_l_1_x" = 1, "x_l_1_z" = 0, "l_1_x_l_1_z" = 1),
					effect.type = "pulse", plot.type = "heatmap", heatmap.options = "all",
					s.limit = 20)

## -----------------------------------------------------------------------------
interact.adl.plot(model = interact.model, x.vrbl = c("x" = 0, "l_1_x" = 1), 
					y.vrbl = c("l_1_y" = 1), 
					z.vrbl = c("z" = 0, "l_1_z" = 1),
					x.z.vrbl = c("x_z" = 0, "z_l_1_x" = 1, "x_l_1_z" = 0, "l_1_x_l_1_z" = 1),
					effect.type = "pulse", plot.type = "heatmap", heatmap.options = "significant",
					s.limit = 20)

## -----------------------------------------------------------------------------
library(ARDL)
ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))

uecm_3132 <- uecm(ardl_3132)

mult_uecm <- multipliers(uecm_3132)
mult_uecm

mult_inter12 <- multipliers(uecm_3132, type = 12, se = TRUE)
mult_inter12

mult_uecm_sr <- multipliers(uecm_3132, type = "sr")
mult_uecm_sr

## -----------------------------------------------------------------------------
library(tidyverse)
library(zoo)

denmark_replication <- fortify.zoo(denmark)

denmark_replication$LRM_l1 <- lag(denmark_replication$LRM)
denmark_replication$LRM_d1 <- denmark_replication$LRM - denmark_replication$LRM_l1
denmark_replication$LRM_d1_l1 <- lag(denmark_replication$LRM_d1)
denmark_replication$LRM_d1_l2 <- lag(denmark_replication$LRM_d1, 2)

denmark_replication$IBO_l1 <- lag(denmark_replication$IBO)
denmark_replication$IBO_d1 <- denmark_replication$IBO - denmark_replication$IBO_l1
denmark_replication$IBO_d1_l1 <- lag(denmark_replication$IBO_d1)
denmark_replication$IBO_d1_l2 <- lag(denmark_replication$IBO_d1, 2)

denmark_replication$LRY_l1 <- lag(denmark_replication$LRY)
denmark_replication$LRY_d1 <- denmark_replication$LRY - denmark_replication$LRY_l1

denmark_replication$IDE_l1 <- lag(denmark_replication$IDE)
denmark_replication$IDE_d1 <- denmark_replication$IDE - denmark_replication$IDE_l1
denmark_replication$IDE_d1_l1 <- lag(denmark_replication$IDE_d1)


uecm_3132_replication <- lm(LRM_d1 ~ LRM_l1 + LRM_d1_l1 + LRM_d1_l2 +
                         LRY_l1 + LRY_d1 +
                         IDE_l1 + IDE_d1 + IDE_d1_l1 +
                         IBO_l1 + IBO_d1 + IBO_d1_l1 + IBO_d1_l2,
                       data = denmark_replication)

## -----------------------------------------------------------------------------
mult_inter12_IBO2 <- GDRF.gecm.plot(model = uecm_3132_replication,
                      x.vrbl = c("IBO_l1" = 1), y.vrbl = c("LRM_l1" = 1), 
                      x.d.vrbl = c("IBO_d1" = 0, "IBO_d1_l1" = 1, "IBO_d1_l2" = 2),
                      x.vrbl.d.x = 0, y.vrbl.d.y = 0,
                      y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2), shock.history = "step",
                      x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
                      s.limit = 12, return.data = TRUE, return.plot = FALSE)

all.equal(mult_inter12_IBO2$GDRF[14], mult_uecm$Estimate[3])
all.equal(mult_inter12_IBO2$SE[14], mult_uecm$'Std. Error'[3])
all.equal(mult_inter12_IBO2$GDRF[1:13], mult_inter12$IBO$Interim)

## -----------------------------------------------------------------------------
mult_inter12_IBO <- GDRF.gecm.plot(model = uecm_3132_replication, 
               x.vrbl = c("IBO_l1" = 1), y.vrbl = c("LRM_l1" = 1), 
               x.d.vrbl = c("IBO_d1" = 0, "IBO_d1_l1" = 1, "IBO_d1_l2" = 2),
               x.vrbl.d.x = 0, y.vrbl.d.y = 0,
               y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2), shock.history = "pulse",
               x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
               s.limit = 12, return.data = TRUE, return.plot = FALSE)

all.equal(mult_inter12_IBO$GDRF, mult_inter12$IBO$Delay) 
all.equal(mult_inter12_IBO$SE, mult_inter12$IBO$'Std. Error Delay')

## -----------------------------------------------------------------------------
all.equal(mult_inter12_IBO$SE[1], sqrt(vcov(uecm_3132_replication)[11,11]))

## -----------------------------------------------------------------------------
all.equal(mult_inter12$IBO$'Std. Error Delay'[1], sqrt(vcov(uecm_3132)[9,9]))
all.equal(mult_inter12$IBO$'Std. Error Delay'[1], mult_uecm_sr$'Std. Error'[3])
all.equal(sqrt(vcov(uecm_3132)[9,9]), mult_uecm_sr$'Std. Error'[3])

## -----------------------------------------------------------------------------
all.equal(mult_inter12_IBO$SE[1], mult_uecm_sr$'Std. Error'[3])

## ----eval = FALSE-------------------------------------------------------------
# ### Replication - LRY
# 
# mult_inter12_LRY2 <- GDRF.gecm.plot(model = uecm_3132_replication, x.vrbl = c("LRY_l1" = 1),
#                                     y.vrbl = c("LRM_l1" = 1),
#                                     x.d.vrbl = c("LRY_d1" = 0),
#                                     x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                                     y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2),
#                                     shock.history = "step",
#                                     x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                                     s.limit = 12, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(mult_inter12_LRY2$GDRF[1:13], mult_inter12$LRY$Interim) # Estimates are Identical
# # ARDL does not calculate standard errors for SRF
# all.equal(mult_inter12_LRY2$GDRF[14], mult_uecm$Estimate[2]) # Estimates are Identical
# all.equal(mult_inter12_LRY2$SE[14], mult_uecm$'Std. Error'[2]) # Standard Errors are Identical
# 
# 
# mult_inter12_LRY <- GDRF.gecm.plot(model = uecm_3132_replication, x.vrbl = c("LRY_l1" = 1),
#                                     y.vrbl = c("LRM_l1" = 1),
#                                     x.d.vrbl = c("LRY_d1" = 0),
#                                     x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                                     y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2),
#                                     shock.history = "pulse",
#                                     x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                                     s.limit = 12, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(mult_inter12_LRY$GDRF, mult_inter12$LRY$Delay) # Estimates are Identical
# all.equal(mult_inter12_LRY$SE, mult_inter12$LRY$'Std. Error Delay') # Standard errors are not
# 
# all.equal(mult_inter12_LRY$SE[1], sqrt(vcov(uecm_3132_replication)[6,6])) # IRF(0) SE match Coef SE for tseffects
# 
# all.equal(mult_inter12$LRY$'Std. Error Delay'[1], sqrt(vcov(uecm_3132)[8,8])) # IRF(0) SE do not match Coef SE for ARDL
# all.equal(mult_inter12$LRY$'Std. Error Delay'[1], mult_uecm_sr$'Std. Error'[2]) # IRF(0) SE do not match SRE SE for ARDL
# all.equal(sqrt(vcov(uecm_3132)[8,8]), mult_uecm_sr$'Std. Error'[2]) # SRE SE are Coef SE for ARDL
# 
# 
# 
# ### Replication - IDE
# mult_inter12_IDE2 <- GDRF.gecm.plot(model = uecm_3132_replication,
#                                     x.vrbl = c("IDE_l1" = 1), y.vrbl = c("LRM_l1" = 1),
#                                     x.d.vrbl = c("IDE_d1" = 0, "IDE_d1_l1" = 1),
#                                     x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                                     y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2),
#                                     shock.history = "step",
#                                     x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                                     s.limit = 12, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(mult_inter12_IDE2$GDRF[1:13], mult_inter12$IDE$Interim) # Estimates are Identical
# # ARDL does not calculate standard errors for SRF
# all.equal(mult_inter12_IDE2$GDRF[14], mult_uecm$Estimate[4]) # Estimates are Identical
# all.equal(mult_inter12_IDE2$SE[14], mult_uecm$'Std. Error'[4]) # Standard Errors are Identical
# 
# 
# mult_inter12_IDE <- GDRF.gecm.plot(model = uecm_3132_replication,
#                                     x.vrbl = c("IDE_l1" = 1), y.vrbl = c("LRM_l1" = 1),
#                                     x.d.vrbl = c("IDE_d1" = 0, "IDE_d1_l1" = 1),
#                                     x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                                     y.d.vrbl = c("LRM_d1_l1" = 1, "LRM_d1_l2" = 2),
#                                     shock.history = "pulse",
#                                     x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                                     s.limit = 12, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(mult_inter12_IDE$GDRF, mult_inter12$IDE$Delay) # Estimates are Identical
# all.equal(mult_inter12_IDE$SE, mult_inter12$IDE$'Std. Error Delay') # Standard Errors are not
# 
# all.equal(mult_inter12_IDE$SE[1], sqrt(vcov(uecm_3132_replication)[8,8])) # IRF(0) SE match Coef SE
# 
# all.equal(mult_inter12$IDE$'Std. Error Delay'[1], sqrt(vcov(uecm_3132)[12,12])) # IRF(0) SE do not match Coef SE
# all.equal(mult_inter12$IDE$'Std. Error Delay'[1], mult_uecm_sr$'Std. Error'[4]) # IRF(0) SE do not match SRE SE
# all.equal(sqrt(vcov(uecm_3132)[12,12]), mult_uecm_sr$'Std. Error'[4]) # SRE SE are Coef SE

## ----eval = FALSE-------------------------------------------------------------
# library(kardl)
# 
# kardl_model <- kardl(imf_example_data,
#                      CPI ~ ER + PPI + asy(ER) +
#                        det(covid) + trend,
#                      mode = c(1, 2, 3, 0))
# 
# summary(kardl_model)
# 
# long <- kardl_longrun(kardl_model)
# long_summary <- summary(long)
# 
# boot <- bootstrap(kardl_model, replications=5, seed = 123L)
# 
# data <- imf_example_data
# data$trend <- 1:nrow(data)
# 
# data$L1.CPI <- lag(data$CPI)
# data$L0.d.CPI <- data$CPI - data$L1.CPI
# data$L1.d.CPI <- lag(data$L0.d.CPI)
# 
# data$L1.ER <- lag(data$ER)
# data$L0.d.ER <- data$ER - data$L1.ER
# 
# data$L0.d.ER_POS <- ifelse(data$L0.d.ER>0, data$L0.d.ER, 0)
# data$L1.d.ER_POS <- lag(data$L0.d.ER_POS)
# data$L2.d.ER_POS <- lag(data$L0.d.ER_POS, 2)
# 
# data$L0.d.ER_NEG <- ifelse(data$L0.d.ER>0, 0, data$L0.d.ER)
# data$L1.d.ER_NEG <- lag(data$L0.d.ER_NEG)
# data$L2.d.ER_NEG <- lag(data$L0.d.ER_NEG, 2)
# data$L3.d.ER_NEG <- lag(data$L0.d.ER_NEG, 3)
# 
# data$L1.ER_POS <- lag(cumsum(replace(data$L0.d.ER_POS,
#                                  is.na(data$L0.d.ER_POS), 0)))
# data$L1.ER_NEG <- lag(cumsum(replace(data$L0.d.ER_NEG,
#                                  is.na(data$L0.d.ER_NEG), 0)))
# 
# data$L1.PPI <- lag(data$PPI)
# data$L0.d.PPI <- data$PPI - data$L1.PPI
# 
# model <- lm(L0.d.CPI ~ L1.CPI + L1.ER_POS + L1.ER_NEG + L1.PPI +
#               L1.d.CPI +
#               L0.d.ER_POS + L1.d.ER_POS + L2.d.ER_POS +
#               L0.d.ER_NEG + L1.d.ER_NEG + L2.d.ER_NEG + L3.d.ER_NEG +
#               L0.d.PPI +
#               covid + trend, data = data[6:470,])
# 
# summary(model)
# 
# boot_ppi <- GDRF.gecm.plot(model = model,
#                            x.vrbl = c("L1.PPI" = 1), y.vrbl = c("L1.CPI" = 1),
#                            x.d.vrbl = c("L0.d.PPI" = 0),
#                            x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                            y.d.vrbl = c("L1.d.CPI" = 1), shock.history = "step",
#                            x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                            s.limit = 20, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(boot_ppi$GDRF[1:21], boot$mpsi$PPI_POS[1:21]) # Estimates are Identical
# 
# all.equal(boot_ppi$GDRF[22], as.numeric(long$coefficients[3])) # Estimates are Identical
# all.equal(boot_ppi$SE[22], long_summary$coefficients[3,2]) # Standard Errors are Identical

## ----eval = FALSE-------------------------------------------------------------
# ### Replication - ER_POS
# boot_pos <- GDRF.gecm.plot(model = model,
#                            x.vrbl = c("L1.ER_POS" = 1), y.vrbl = c("L1.CPI" = 1),
#                            x.d.vrbl = c("L0.d.ER_POS" = 0, "L1.d.ER_POS" = 1, "L2.d.ER_POS" = 2),
#                            x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                            y.d.vrbl = c("L1.d.CPI" = 1), shock.history = "step",
#                            x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                            s.limit = 20, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(boot_pos$GDRF[1:21], boot$mpsi$ER_POS[1:21]) # Estimates are Identical
# 
# all.equal(boot_pos$GDRF[22], as.numeric(long$coefficients[1])) # Estimates are Identical
# all.equal(boot_pos$SE[22], long_summary$coefficients[1,2]) # Standard Errors are Identical
# 
# 
# ### Replication - ER_NEG
# boot_neg <- GDRF.gecm.plot(model = model,
#                            x.vrbl = c("L1.ER_NEG" = 1), y.vrbl = c("L1.CPI" = 1),
#                            x.d.vrbl = c("L0.d.ER_NEG" = 0, "L1.d.ER_NEG" = 1,
#                                         "L2.d.ER_NEG" = 2, "L3.d.ER_NEG" = 3),
#                            x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                            y.d.vrbl = c("L1.d.CPI" = 1), shock.history = "step",
#                            x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                            s.limit = 20, return.data = TRUE, return.plot = FALSE)
# 
# all.equal(-boot_neg$GDRF[1:21], boot$mpsi$ER_NEG[1:21]) # Estimates are Identical
# # Must flip sign of tseffect estimates because it's for a one-unit decrease
# 
# all.equal(boot_neg$GDRF[22], as.numeric(long$coefficients[2])) # Estimates are Identical
# all.equal(boot_neg$SE[22], long_summary$coefficients[2,2]) # Standard Errors are Identical

## ----eval = FALSE-------------------------------------------------------------
# library(dynamac)
# 
# ### From helpfile of dynardl.simulation.plot()
# 
# set.seed(1)
# ardl.model <- dynardl(concern ~ incshare10 + urate, data = ineq,
#                       lags = list("concern" = 1, "incshare10" = 1),
#                       diffs = c("incshare10", "urate"),
#                       lagdiffs = list("concern" = 1),
#                       ec = TRUE, simulate = TRUE, range = 30,
#                       shockvar = "incshare10", shockval = 1, fullsims = TRUE)
# 
# summary(ardl.model)
# 
# ### Replication - Model
# inequality <- ineq
# 
# inequality$l1.concern <- lag(inequality$concern, 1)
# inequality$d1.concern <- inequality$concern-inequality$l1.concern
# inequality$l1.d1.concern <- lag(inequality$d1.concern)
# 
# inequality$l1.incshare10 <- lag(inequality$incshare10)
# inequality$d1.incshare10 <- inequality$incshare10 - inequality$l1.incshare10
# 
# inequality$l1.urate <- lag(inequality$urate)
# inequality$d1.urate <- inequality$urate - inequality$l1.urate
# 
# ardl.model.replication <- lm(d1.concern ~ l1.concern + l1.d1.concern +
#                           l1.incshare10 + d1.incshare10 +
#                           d1.urate, data = inequality)
# 
# ### Replication - incshare10
# ardl.model.replication.plot <- GDRF.gecm.plot(model = ardl.model.replication,
#                                          x.vrbl = c("l1.incshare10" = 1), y.vrbl = c("l1.concern" = 1),
#                                          x.d.vrbl = c("d1.incshare10" = 0),
#                                          x.vrbl.d.x = 0, y.vrbl.d.y = 0,
#                                          y.d.vrbl = c("l1.d1.concern" = 1), shock.history = "pulse",
#                                          x.d.vrbl.d.x = 1, y.d.vrbl.d.y = 1,
#                                          s.limit = 20, return.data = TRUE, return.plot = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# # dynamac is fitted values; dshift (difference) is akin to marginal effect (change in fitted)
# all.equal(ardl.model.replication.plot$GDRF[1:21],
#           dshift(ardl.model$simulation$central)[10:30])

## ----eval = FALSE-------------------------------------------------------------
# dynamac.fitted <- dynardl(y ~ x,
#                       lags = list("y" = 1, "x" = 1),
#                       levels = c("x"),
#                       ec = FALSE, simulate = TRUE, range = 35,
#                       shockvar = "x", shockval = 1,
#                       forceset = list("x" = 0.1), fullsims = TRUE,
#                       data = toy.ts.interaction.data)
# 
# model.fitted <- lm(y ~ l_1_y + x + l_1_x, data = toy.ts.interaction.data)
# tseffects.fitted <- GDRF.adl.plot(model = model.fitted,
# 						x.vrbl = c("x" = 0, "l_1_x" = 1),
# 						y.vrbl = c("l_1_y" = 1),
# 						d.y = 0, d.x = 0,
# 						# baseline.y = 0,
# 						effect.type = "fitted", shock.history = "step",
# 						inferences.x = "levels", inferences.y = "levels",
# 						prediction.values = list("x" = 0.1, "l_1_x" = 0.1), return.data = TRUE, return.plot = TRUE)
# 
# # shock at 10, so baseline at 9. s = 0 is 10. thus s = 20 is 30.
# cbind(tseffects.fitted$data$GDRF, dynamac.fitted$simulation$central[9:30]) # estimates are close
# all.equal(tseffects.fitted$data$GDRF, dynamac.fitted$simulation$central[9:30]) # estimates are close
# 
# library(ggplotify)
# library(patchwork)
# 
# combined <- as.ggplot(~dynardl.simulation.plot(dynamac.fitted)) + tseffects.fitted$plot
# combined

