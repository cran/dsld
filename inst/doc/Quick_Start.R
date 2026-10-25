## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 5)
library(dsld)
set.seed(2026)

## ----law-school-model---------------------------------------------------------
data(lsa, package = "qeML")
names(lsa)
lsat_model <- dsldLinear(lsa, "lsat", "race1")
summary(lsat_model)

## ----fair-models--------------------------------------------------------------
data(compas1, package = "dsld")
records <- droplevels(subset(compas1, race %in% c("African-American", "Caucasian")))
records <- records[, c("age", "priors_count", "sex", "two_year_recid", "race")]
records <- records[sample.int(nrow(records), 800), ]
train_rows <- sample.int(nrow(records), 600)
training <- records[train_rows, ]
testing <- records[-train_rows, ]
test_x <- testing[, setdiff(names(testing), "two_year_recid")]

baseline <- dsldFgrrm(training, "two_year_recid", "race", unfairness = 1,
                      yesYVal = "Yes")
constrained <- dsldFgrrm(training, "two_year_recid", "race", unfairness = 0.1,
                         yesYVal = "Yes")
baseline_result <- predict(baseline, test_x)
constrained_result <- predict(constrained, test_x)
data.frame(
  model = c("Baseline", "Constrained"),
  test_error = c(
    mean((baseline_result$preds > 0.5) != (testing$two_year_recid == "Yes")),
    mean((constrained_result$preds > 0.5) != (testing$two_year_recid == "Yes"))
  )
)
constrained_result$correlations

## ----conditional-disparity----------------------------------------------------
law_sample <- lsa[sample.int(nrow(lsa), 1500), ]
levels(law_sample$bar)
dsldConditDisparity(law_sample, "bar", "race1", "lsat", minS = 50)

## ----group-frequencies--------------------------------------------------------
data(svcensus, package = "dsld")
names(svcensus)
dsldFrequencyByS(svcensus, cName = "educ", sName = "gender")
dsldFrequencyByS(svcensus, cName = "occ", sName = "gender")
tapply(svcensus$wageinc, svcensus$occ, mean)

## ----parallel-coordinates-----------------------------------------------------
plot_data <- law_sample[, c("fam_inc", "ugpa", "gender", "lsat", "race1")]
dsldFreqPCoord(plot_data, m = 25, sName = "race1", k = 15)

## ----feature-sets-------------------------------------------------------------
wages <- svcensus[sample.int(nrow(svcensus), 800),
                   c("age", "educ", "wkswrkd", "gender", "wageinc")]
dsldTakeALookAround(wages, "wageinc", "gender", maxFeatureSetSize = 2, holdout = 100)

