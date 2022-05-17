## ---- init

library(ggplot2)
library(ggcorrplot)
library(hrbrthemes)
library(GGally)
library(FactoMineR)
library(stats)
library(glmnet)
library(ggfortify)

default_theme <- theme_ipsum(base_family = "") + theme(
  axis.title.x = element_text(hjust = 0.5),
  axis.title.y = element_text(hjust = 0.5),
  plot.margin = margin(
    t = 0.5,
    r = 2, b = 0.5, l = 2, "cm"
  ),
  legend.position = "bottom"
)

theme_set(default_theme)

## ---- read-df

df <- read.table("rain_project.txt", sep = " ", header = T)
numeric_idx <- 3:18

## ---- replace-date

df$date <- months(as.Date(df$date))

## ---- convert-factor

df[c(2, 19)] <- lapply(df[c(2, 19)], as.factor)

## ---- boxplot-df

g_boxplot_df <- stack(
  data.frame(scale(df[numeric_idx], center = T, scale = T))
) %>% ggplot(aes(x = ind, y = values)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 4))


## ---- boxplot-rain-date

g_boxplot_rd <- df %>% ggplot(aes(x = date, y = rain)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

## ---- skewness-df

sk_df <- apply(df[numeric_idx], 2, moments::skewness)

## ---- hist-df

g_hist_df_fun <- function(x) {
  g <- df %>% ggplot(aes_string(x = x)) +
    geom_histogram(
      position = "identity", colour = "grey40", alpha = 0.2
    ) +
    ggtitle(paste("Histogram of", x))
  return(g)
}

g_hist_df <- lapply(colnames(df[numeric_idx]), FUN = g_hist_df_fun)

## ---- corr-df

corr_df <- cor(df[numeric_idx])
g_corr_df <- ggcorrplot(corr_df, type = "lower")

## ---- pairs-df

g_pairs_df <- ggpairs(df[numeric_idx])

## ---- pca

pca <- PCA(
  df[c(19, numeric_idx)],
  scale.unit = T, graph = F, quali.sup = 1, ncp = 7
)

## ---- pca-inertia-bar

g_bar_intertia <- ggplot() +
  geom_col(aes(names(pca$eig[, 2]), unname(pca$eig[, 2]))) +
  scale_x_discrete(
    guide = guide_axis(check.overlap = T),
    limits = names(pca$eig[, 2])
  )

## ---- pca-boxplot-coord

g_boxplot_coord <- stack(
  data.frame(pca$ind$coord)
) %>% ggplot(aes(x = ind, y = values)) +
  geom_boxplot()

## ---- pca-varcor

g_varcor <- plot(
  pca,
  choix = "varcor",
  grah.type = "ggplot",
) + default_theme

## ---- pca-ind-rain

rain_col <- c("blue", "orange", "red")

g_ind_rain <- plot(
  pca,
  choix = "ind",
  unselect = 0,
  grah.type = "ggplot",
  habillage = 1,
  col.hab = rain_col,
  label = "none",
  title = ""
) + default_theme

## ---- kmeans

km_rain <- kmeans(df[numeric_idx], centers = 3)

## ---- pca-ind-kmeans

g_ind_kmeans <- plot(
  pca,
  choix = "ind",
  unselect = 0,
  grah.type = "ggplot",
  habillage = "ind",
  col.hab = sapply(
    unname(km_rain$cluster), function(i) rain_col[i]
  ),
  label = "none",
  title = ""
) + default_theme

## ---- split-index

set.seed(111)
test_ratio <- .2
npop <- nrow(df)
nvar <- ncol(df)
ntest <- ceiling(npop * test_ratio)
test_idx <- sample(1:npop, ntest)
learn_idx <- setdiff(1:npop, test_idx)

## ---- split-df

df_learn_r <- df[learn_idx, -c(1, 19)]
df_test_r <- df[test_idx, -c(1, 19)]
df_learn_rc <- df[learn_idx, -c(1, 18)]
df_test_rc <- df[test_idx, -c(1, 18)]

## ---- plot-res

plot_res_fit <- function(fit, res) {
  g <- ggplot() +
    geom_point(
      aes(fit, res),
      color = "#69b3a2", size = 1, alpha = 0.75
    ) +
    geom_hline(yintercept = 0, linetype = "longdash")
  return(g)
}

plot_res_qq <- function(res) {
  g <- data.frame(res) %>% ggplot(aes(sample = res)) +
    geom_qq(
      color = "#69b3a2", size = 1, alpha = 0.75
    ) +
    geom_qq_line(linetype = "longdash")
  return(g)
}

## ---- lm-no-int

lm_no_int <- aov(rain ~ ., df_learn_r)

## ---- plot-lm-no-int

g_lm_no_int_fit <- plot_res_fit(
  lm_no_int$fitted.values, lm_no_int$residuals
)

g_lm_no_int_qq <- plot_res_qq(
  lm_no_int$residuals
)

## ---- plot-aromne

g_arome_fit <- plot_res_fit(
  df_learn_r$tp_arome, df_learn_r$tp_arome - df_learn_r$rain
)

## ---- lasso-quanti

lasso_quanti <- glmnet(df_learn_r[, -c(1, 17)], df_learn_r$rain)

## ---- model-matrix

model_matrix <- model.matrix(rain ~ . - 1, data = df_learn_r)

## ---- lasso

lasso <- glmnet(model_matrix, df_learn_r$rain)

## ---- plot-lasso-lambda

g_lasso_lambda <- autoplot(lasso, "lambda", label = F)

## ---- lasso-cv

lasso_cv <- cv.glmnet(model_matrix, df_learn_r$rain)

## ---- plot-lasso-cv

g_lasso_cv <- autoplot(
  lasso_cv,
  colour = alpha("grey", 0.75)
)

## ---- plot-lasso-1se

g_lasso_1se <- autoplot(lasso, "lambda", label = F) +
  geom_vline(
    xintercept = log(lasso_cv$lambda.1se),
    colour = "red", linetype = "longdash"
  )

## ---- plot-lasso-min

g_lasso_min <- autoplot(lasso, "lambda", label = F) +
  geom_vline(
    xintercept = log(lasso_cv$lambda.min),
    colour = "red", linetype = "longdash"
  )

## ---- lasso-min-res-fit

fit_lasso_min <- predict(lasso_cv, s = "lambda.min", newx = model_matrix)
res_lasso_min <- df_learn_r$rain - fit_lasso_min

## ---- plot-lasso-min-res-fit

g_lasso_min_res_fit <- plot_res_fit(fit_lasso_min, res_lasso_min)

## ---- lasso-1se-res-fit

fit_lasso_1se <- predict(lasso_cv, s = "lambda.1se", newx = model_matrix)
res_lasso_1se <- df_learn_r$rain - fit_lasso_1se

## ---- plot-lasso-1se-res-fit

g_lasso_1se_res_fit <- plot_res_fit(fit_lasso_1se, res_lasso_1se)