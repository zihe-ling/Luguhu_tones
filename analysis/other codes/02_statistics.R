# 安装所需包（如未安装，取消注释后运行）
# install.packages(c("dplyr", "tidyr", "ggplot2", "purrr", "tibble"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)

# ---------- 1. 在原表中计算年龄 ----------
# 假设 df 是前面“混同列赋值完成”的原表
# 若原表已有“年龄”列，可跳过这一步
df <- df %>%
  mutate(
    年龄 = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年)
  )

# 性别、教育程度转为因子（教育程度如为有序，可自行指定 levels）
df <- df %>%
  mutate(
    性别 = factor(性别),
    教育程度 = factor(教育程度)
  )

# ---------- 2. 定义混同列与对应调类的映射 ----------
mix_info <- tribble(
  ~混同值,        ~相关调类,
  "COP_A1toA2",  "A1",
  "COP_A2toA1",  "A2",
  "COP_B1toB2",  "B1",
  "COP_B2toB1",  "B2",
  "EXT_A1toA2",  "A1",
  "EXT_A2toA1",  "A2",
  "EXT_B1toB2",  "B1",
  "EXT_B2toB1",  "B2"
)

# ---------- 3. 定义单个混同列的检验函数 ----------
test_one <- function(v, tone) {
  sub <- df %>%
    filter(调类 == tone) %>%
    select(姓名, 性别, 年龄, 教育程度, 摩梭话比重, value = all_of(v)) %>%
    filter(!is.na(value))
  
  n <- nrow(sub)
  
  # ---- 性别：t 检验 + Wilcoxon ----
  sex_res <- tryCatch({
    s <- sub %>% filter(!is.na(性别)) %>% mutate(性别 = droplevels(factor(性别)))
    if (nlevels(s$性别) >= 2 && nrow(s) >= 4) {
      tt <- t.test(value ~ 性别, data = s)
      wt <- suppressWarnings(wilcox.test(value ~ 性别, data = s))
      tibble(
        性别_t_p    = tt$p.value,
        性别_wilcox_p = wt$p.value,
        男均值      = mean(s$value[s$性别 == levels(s$性别)[1]], na.rm = TRUE),
        女均值      = mean(s$value[s$性别 == levels(s$性别)[2]], na.rm = TRUE)
      )
    } else tibble(性别_t_p = NA, 性别_wilcox_p = NA, 男均值 = NA, 女均值 = NA)
  }, error = function(e) tibble(性别_t_p = NA, 性别_wilcox_p = NA, 男均值 = NA, 女均值 = NA))
  
  # ---- 年龄：Pearson + Spearman ----
  age_res <- tryCatch({
    s <- sub %>% filter(!is.na(年龄))
    if (nrow(s) >= 3 && sd(s$年龄) > 0 && sd(s$value) > 0) {
      pr <- cor.test(s$value, s$年龄, method = "pearson")
      sr <- suppressWarnings(cor.test(s$value, s$年龄, method = "spearman"))
      tibble(
        年龄_r        = unname(pr$estimate),
        年龄_pearson_p = pr$p.value,
        年龄_rho      = unname(sr$estimate),
        年龄_spearman_p = sr$p.value
      )
    } else tibble(年龄_r = NA, 年龄_pearson_p = NA, 年龄_rho = NA, 年龄_spearman_p = NA)
  }, error = function(e) tibble(年龄_r = NA, 年龄_pearson_p = NA, 年龄_rho = NA, 年龄_spearman_p = NA))
  
  # ---- 教育程度：ANOVA + Kruskal-Wallis ----
  edu_res <- tryCatch({
    s <- sub %>% filter(!is.na(教育程度)) %>% mutate(教育程度 = droplevels(factor(教育程度)))
    if (nlevels(s$教育程度) >= 2 && nrow(s) >= nlevels(s$教育程度) + 1) {
      aov_tab <- summary(aov(value ~ 教育程度, data = s))[[1]]
      kw <- kruskal.test(value ~ 教育程度, data = s)
      tibble(
        教育_F       = aov_tab[["F value"]][1],
        教育_anova_p = aov_tab[["Pr(>F)"]][1],
        教育_kw_p    = kw$p.value
      )
    } else tibble(教育_F = NA, 教育_anova_p = NA, 教育_kw_p = NA)
  }, error = function(e) tibble(教育_F = NA, 教育_anova_p = NA, 教育_kw_p = NA))
  
  # ---- 摩梭话比重：Pearson + Spearman ----
  mo_res <- tryCatch({
    s <- sub %>% filter(!is.na(摩梭话比重))
    if (nrow(s) >= 3 && sd(s$摩梭话比重) > 0 && sd(s$value) > 0) {
      pr <- cor.test(s$value, s$摩梭话比重, method = "pearson")
      sr <- suppressWarnings(cor.test(s$value, s$摩梭话比重, method = "spearman"))
      tibble(
        摩梭话_r        = unname(pr$estimate),
        摩梭话_pearson_p = pr$p.value,
        摩梭话_rho      = unname(sr$estimate),
        摩梭话_spearman_p = sr$p.value
      )
    } else tibble(摩梭话_r = NA, 摩梭话_pearson_p = NA, 摩梭话_rho = NA, 摩梭话_spearman_p = NA)
  }, error = function(e) tibble(摩梭话_r = NA, 摩梭话_pearson_p = NA, 摩梭话_rho = NA, 摩梭话_spearman_p = NA))
  
  bind_cols(
    tibble(混同值 = v, 相关调类 = tone, 样本量 = n),
    sex_res, age_res, edu_res, mo_res
  )
}

# ---------- 4. 批量检验 8 个混同列 ----------
results <- pmap_dfr(
  list(mix_info$混同值, mix_info$相关调类),
  test_one
)

# 转为数据框并打印
results <- as.data.frame(results)
print(results, width = 200, digits = 4)

# ---------- 5. 对 p 值做 FDR 校正（BH 法） ----------
# 便于控制 8 个混同列多次检验带来的假阳性
p_cols <- c("性别_t_p", "性别_wilcox_p", "年龄_pearson_p", "年龄_spearman_p",
            "教育_anova_p", "教育_kw_p", "摩梭话_pearson_p", "摩梭话_spearman_p")

results_fdr <- results
for (col in p_cols) {
  results_fdr[[paste0(col, "_FDR")]] <- p.adjust(results[[col]], method = "BH")
}

cat("\n===== FDR 校正后的 p 值 =====\n")
print(results_fdr[, c("混同值", grep("_FDR$", names(results_fdr), value = TRUE))],
      width = 200, digits = 4)

# ---------- 6. 可视化 ----------
# 6.1 每个混同列 vs 年龄 / 摩梭话比重（散点 + 回归线）
plot_long <- df %>%
  select(姓名, 调类, 性别, 年龄, 教育程度, 摩梭话比重,
         COP_A1toA2, COP_A2toA1, COP_B1toB2, COP_B2toB1,
         EXT_A1toA2, EXT_A2toA1, EXT_B1toB2, EXT_B2toB1) %>%
  pivot_longer(
    cols = c(COP_A1toA2, COP_A2toA1, COP_B1toB2, COP_B2toB1,
             EXT_A1toA2, EXT_A2toA1, EXT_B1toB2, EXT_B2toB1),
    names_to = "混同值", values_to = "取值"
  ) %>%
  left_join(mix_info, by = "混同值") %>%
  filter(调类 == 相关调类)   # 只保留对应调类的记录

# 与连续变量的散点图
plot_cont <- plot_long %>%
  select(混同值, 年龄, 摩梭话比重, 取值) %>%
  pivot_longer(cols = c(年龄, 摩梭话比重), names_to = "变量", values_to = "变量值") %>%
  filter(!is.na(取值), !is.na(变量值))

p_cont <- ggplot(plot_cont, aes(x = 变量值, y = 取值)) +
  geom_point(alpha = 0.5, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  facet_grid(混同值 ~ 变量, scales = "free") +
  theme_minimal(base_size = 10) +
  labs(x = NULL, y = "混同值取值")

print(p_cont)

# 与性别、教育程度的箱线图
plot_cat <- plot_long %>%
  select(混同值, 性别, 教育程度, 取值) %>%
  pivot_longer(cols = c(性别, 教育程度), names_to = "变量", values_to = "变量值") %>%
  filter(!is.na(取值), !is.na(变量值))

p_cat <- ggplot(plot_cat, aes(x = 变量值, y = 取值, fill = 变量值)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +
  facet_grid(混同值 ~ 变量, scales = "free", space = "free_x") +
  theme_minimal(base_size = 10) +
  labs(x = NULL, y = "混同值取值") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

print(p_cat)

# 如需保存：
# ggsave("mix_vs_demographics_cont.png", p_cont, width = 10, height = 14)
# ggsave("mix_vs_demographics_cat.png",  p_cat,  width = 10, height = 14)