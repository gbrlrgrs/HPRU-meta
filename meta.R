library(here)
library(magrittr)
library(tidyverse)
library(meta)

tblRota <- read.rm5(here("CD008521StatsDataOnly.rm5")) %>%
  filter(comp.no == 1,
         outcome.no == 1) %>%
  as_tibble() %>%
  mutate(Risk = recode(group.no, "Low", "Medium", "High")) %>%
  arrange(year)

metaP <- metaprop(data          = tblRota,
                  event         = event.c,
                  n             = n.c,
                  # sm            = "PRAW",
                  sm            = "PLOGIT",
                  # sm            = "PFT",
                  method.tau    = "DL",
                  method        = "Inverse",
                  studlab       = studlab,
                  subgroup      = Risk,
                  prediction.subgroup = T,
                  subgroup.name = "Mortality risk")

blnFixed <- F
metaP %>%
  forest(fixed                = blnFixed,
         random               = !blnFixed,
         bottom.lr            = T,
         label.e              = "Vaccination",
         label.c              = "Placebo",
         label.right          = "Favours placebo",
         label.left           = "Favours vaccination",
         bylab                = "",
         print.I2             = T,
         print.tau2           = T,
         print.Q              = T,
         print.pval.Q         = T,
         hetlab               = "  ",
         weight.study         = ifelse(blnFixed, "fixed", "random"),
         weight.subgroup      = "same",
         text.fixed.w         = "  Subtotal",
         text.random.w        = "  Subtotal",
         # text.fixed           = "Overall (FE - Freeman-Tukey)",
         # text.random          = "Overall (RE - Freeman-Tukey)",
         text.fixed           = "Overall (FE - Logit)",
         text.random          = "Overall (RE - Logit)",
         addrow.subgroups     = T,
         spacing              = 1.2,
         squaresize           = 0.75,
         rightcols            = c("effect.ci", ifelse(blnFixed, "w.fixed", "w.random")),
         col.diamond          = "#87AAB9",
         col.square           = "#2F5763",
         col.square.lines     = "#2F5763",
         col.inside           = "black",
         col.by               = "black",
         just                 = "right",
         just.addcols.right   = "right",
         plotwidth            = "8cm",
         fontsize             = 14,
         test.overall.fixed   = blnFixed,
         test.subgroup.fixed  = blnFixed,
         test.overall.random  = !blnFixed,
         test.subgroup.random = !blnFixed,
         # colgap.left          = unit(0.2, "cm"),
         # colgap.studlab       = unit(0, "cm"),
         colgap.forest.left   = unit(3.4, "cm"),
         colgap.forest.right  = unit(1, "cm"),
         big.mark             = ",",
         xlim                 = c(0,0.2),
         # at                   = c(0.01, 0.1, 0.5, 1, 2, 10),
         digits               = 3, 
         digits.pval.Q        = 3, 
         digits.pval          = 3,
         digits.tau2          = 3,
         prediction           = F,
         prediction.subgroup  = F,
         backtransf           = T)

funnel(metaP,
       backtransf = T)

metaP <- metaprop(data          = tblRota,
                  event         = event.c,
                  n             = n.c,
                  sm            = "PLOGIT",
                  method.tau    = "DL",
                  method        = "Inverse",
                  studlab       = studlab)
metabias(metaP,
         k.min=2, method="linreg")

tf1 <- trimfill(metaP)
tf1
funnel(tf1, pch = ifelse(tf1$trimfill, 1, 16), level = 0.9, random = T)

#########

metaR <- metabin(data          = tblRota,
                 event.e       = event.e,
                 n.e           = n.e,
                 event.c       = event.c,
                 n.c           = n.c,
                 sm            = "OR",
                 method.tau    = "DL",
                 method        = "Inverse",
                 studlab       = studlab,
                 subgroup      = Risk,
                 prediction.subgroup = T,
                 subgroup.name = "Mortality risk")
metaR

blnFixed <- F
settings.meta(CIbracket="(", CIseparator=", ")
forest(metaR,
       fixed  = blnFixed,
       random = !blnFixed,
       bottom.lr   = T,
       label.e = "Vaccination",
       label.c = "Placebo",
       label.right = "Favours placebo",
       label.left  = "Favours vaccination",
       bylab="",
       print.I2 = T, print.tau2 = T, print.Q = T, print.pval.Q = T,
       hetlab="  ",
       weight.study = ifelse(blnFixed, "fixed", "random"),
       weight.subgroup = "same",
       text.fixed.w = "  Subtotal",
       text.random.w = "  Subtotal",
       text.fixed = "Overall (FE)",
       text.random = "Overall (RE)",
       addrow.subgroups = T,
       spacing = 1.2,
       squaresize = 0.75,
       rightcols=c("effect.ci", ifelse(blnFixed, "w.fixed", "w.random")),
       col.diamond = "#87AAB9",
       col.square = "#2F5763",
       col.square.lines = "#2F5763",
       col.inside = "black",
       col.by = "black",
       just="right",
       just.addcols.right = "right",
       plotwidth="8cm", fontsize=14,
       test.overall.fixed = blnFixed,
       test.overall.random=!blnFixed,
       test.subgroup.fixed = blnFixed,
       test.subgroup.random=!blnFixed,
       # colgap.left=unit(0.2, "cm"),
       # colgap.studlab=unit(0, "cm"),
       colgap.forest.left=unit(1, "cm"),
       colgap.forest.right=unit(1, "cm"),
       big.mark=",",
       # xlim = c(0.01,10),
       # at = c(0.01, 0.1, 0.5, 1, 2, 10),
       digits = 3, 
       digits.pval.Q = 3, 
       digits.pval = 3,
       digits.tau2 = 3,
       prediction = F,
       prediction.subgroup = F,
       subgroup = T,
       hetstat = T,
       test.subgroup = T,
       backtransf           = F)

metaR <- metabin(data          = tblRota,
                 event.e       = event.e,
                 n.e           = n.e,
                 event.c       = event.c,
                 n.c           = n.c,
                 sm            = "OR",
                 method.tau    = "DL",
                 method        = "Inverse",
                 studlab       = studlab)
metaR %>%
  metabias(k.min  = 2,
           method = "Egger",
           plotit = F)
metaR %>%
  funnel(random      = F,
         fixed       = TRUE,
         xlim        = c(0.02, 5),
         # ylim        = c(1.6, 0),
         level       = 0.95,
         contour     = c(0.9, 0.95, 0.99),
         col.contour = c("grey70", "grey80", "grey90"),
         lwd         = 1,
         cex         = 1.5,
         bg          = "white",
         # pch         = 16,
         pch         = 20 + metaR$data$group.no,
         studlab     = F,
         yaxis       = "se")

legend(x = 0.02,
       y = 0.05,
       legend = c("low", "medium", "high"),
       pch = 21:23,
       cex = 1)

legend(x = 1.5,
       y = 0.05,
       legend = c("p > 0.1", "0.1 > p > 0.05", "0.05 > p > 0.01", "p < 0.01"),
       fill = c("white", "grey70", "grey80", "grey90"))



tf1 <- trimfill(metaR, random=T)
tf1$subG <-
  tibble(studlab = str_remove(tf1$studlab, "Filled: ")) %>%
  left_join(tblRota, by=c("studlab" = "studlab")) %>%
  select(group.no) %>%
  pull()

tf1 %>%
  funnel(random      = F,
         fixed       = TRUE,
         xlim        = c(0.02, 5),
         # ylim        = c(1.6, 0),
         level       = 0.95,
         contour     = c(0.9, 0.95, 0.99),
         col.contour = c("grey70", "grey80", "grey90"),
         lwd         = 1,
         lty.fixed   = 2,
         cex         = 1.5,
         pch         = 20 + tf1$subG,
         bg          = ifelse(tf1$trimfill, "black", "white"),
         studlab     = F,
         yaxis       = "se")


s <- 1
metaRs <- metabin(data          = tblRota %>%
                    filter(group.no == s),
                  event.e       = event.e,
                  n.e           = n.e,
                  event.c       = event.c,
                  n.c           = n.c, 
                  sm            = "OR",
                  method.tau    = "DL",
                  method        = "Inverse",
                  studlab       = studlab)
metaRs %>%
  metabias(k.min  = 2,
           method = "Egger",
           plotit = F)
metaRs %>%
  funnel(random      = F,
         fixed       = TRUE,
         xlim        = c(0.02, 5),
         # ylim        = c(1.6, 0),
         level       = 0.95,
         contour     = c(0.9, 0.95, 0.99),
         col.contour = c("grey70", "grey80", "grey90"),
         lwd         = 1,
         cex         = 1.5,
         bg          = "white",
         # pch         = 16,
         pch         = 20 + metaRs$data$group.no,
         studlab     = F,
         yaxis       = "se")

legend(x = 0.02,
       y = 0.05,
       legend = c("low", "medium", "high"),
       pch = 21:23,
       cex = 1)

legend(x = 1.5,
       y = 0.05,
       legend = c("p > 0.1", "0.1 > p > 0.05", "0.05 > p > 0.01", "p < 0.01"),
       fill = c("white", "grey70", "grey80", "grey90"))


blnFixed <- T
forest(metaRs,
       fixed  = blnFixed,
       random = !blnFixed,
       bottom.lr   = T,
       label.e = "Vaccination",
       label.c = "Placebo",
       label.right = "Favours placebo",
       label.left  = "Favours vaccination",
       bylab="",
       print.I2 = T, print.tau2 = T, print.Q = T, print.pval.Q = T,
       hetlab="  ",
       weight.study = ifelse(blnFixed, "fixed", "random"),
       weight.subgroup = "same",
       text.fixed.w = "  Subtotal",
       text.random.w = "  Subtotal",
       text.fixed = "Overall",
       text.random = "Overall",
       addrow.subgroups = T,
       spacing = 1.2,
       squaresize = 0.75,
       rightcols=c("effect.ci", ifelse(blnFixed, "w.fixed", "w.random")),
       col.diamond = "#87AAB9",
       col.square = "#2F5763",
       col.square.lines = "#2F5763",
       col.inside = "black",
       col.by = "black",
       just="right",
       just.addcols.right = "right",
       plotwidth="8cm", fontsize=14,
       test.overall.fixed = blnFixed,
       test.overall.random=!blnFixed,
       # test.subgroup.fixed = blnFixed,
       # test.subgroup.random=!blnFixed,
       colgap.left=unit(0.4, "cm"),
       # colgap.studlab=unit(0, "cm"),
       colgap.forest.left=unit(1, "cm"),
       colgap.forest.right=unit(1, "cm"),
       big.mark=",",
       xlim = c(0.01,10),
       at = c(0.01, 0.1, 0.5, 1, 2, 10),
       digits = 3, 
       digits.pval.Q = 3, 
       digits.pval = 3,
       digits.tau2 = 3,
       prediction = F,
       prediction.subgroup = F,
       subgroup = F,
       hetstat = T,
       test.subgroup = F)

