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

