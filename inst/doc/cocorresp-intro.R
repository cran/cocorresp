## ----setup, echo = FALSE, results = "hide", message = FALSE, cache = FALSE----
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library("cocorresp")

## ----load-data----------------------------------------------------------------
data(beetles)
## log transform the beetle data
beetles <- log1p(beetles)
data(plants)

## ----fit-symcoca, message = TRUE----------------------------------------------
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## ----data-process, message = TRUE---------------------------------------------
beetles <- beetles[, colSums(beetles) > 0]
plants <- plants[, colSums(plants) > 0]
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## ----print-symcoca------------------------------------------------------------
bp.sym

## ----screeplot-symcoca--------------------------------------------------------
screeplot(bp.sym)

## ----plot-symcoca, fig.width = 14, fig.height = 7, fig.show = "hold"----------
layout(matrix(1:2, ncol = 2))
biplot(bp.sym, which = "y1", main = "Beetles")
biplot(bp.sym, which = "y2", main = "Plants")
layout(1)

