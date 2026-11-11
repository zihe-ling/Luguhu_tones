# install.packages(c("dplyr", "tidyr", "broom", "purrr", "tibble"))

library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(tibble)

# ---------- 1. 先准备人口学变量（每人一行） ----------
demo_df <- df %>%
  group_by(姓名) %>%
  summarise(
    性别       = first(na.omit(性别)),
    出生年     = first(na.omit(出生年)),
    教育程度   = first(na.omit(教育程度)),
    摩梭话比重 = first(na.omit(摩梭话比重)),
    .groups = "drop"
  ) %>%
  mutate(
    年龄 = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年),
    性别 = factor(性别),
    # ---- 教育程度：合并大专与初中为"初中及以上"，基线为"无" ----
    教育程度 = case_when(
      教育程度 %in% c("无", "未上学", "没上过学") ~ "无",
      教育程度 %in% c("小学")                     ~ "小学",
      教育程度 %in% c("初中", "中专", "大专")     ~ "初中及以上",
      TRUE                                        ~ NA_character_
    ),
    教育程度 = factor(教育程度,
                  levels = c("无", "小学", "初中及以上"))  # "无" 作基线
  )

# 检查各组人数
cat("\n===== 教育程度分布 =====\n")
print(table(demo_df$教育程度, useNA = "ifany"))

# ---------- 2. 定义 12 个混类值对应的 success 列和调类条件 ----------
mix_specs <- tribble(
  ~混类值,   ~success_col,    ~tone_filter,
  "A1_COP",  "COP_A1toA2",    "A1",
  "A2_COP",  "COP_A2toA1",    "A2",
  "B1_COP",  "COP_B1toB2",    "B1",
  "B2_COP",  "COP_B2toB1",    "B2",
  "A1_EXT",  "EXT_A1toA2",    "A1",
  "A2_EXT",  "EXT_A2toA1",    "A2",
  "B1_EXT",  "EXT_B1toB2",    "B1",
  "B2_EXT",  "EXT_B2toB1",    "B2",
  "AB_COP",  "COP_AtoB",      "A1,A2",
  "BA_COP",  "COP_BtoA",      "B1,B2",
  "AB_EXT",  "EXT_AtoB",      "A1,A2",
  "BA_EXT",  "EXT_BtoA",      "B1,B2"
)

# ---------- 3. 单个逻辑回归模型函数 ----------
fit_glm_one <- function(y_name, success_col, tone_filter) {
  tone_vec <- strsplit(tone_filter, ",")[[1]]
  
  # 按姓名聚合：成功数 = 相关调类记录中该列之和，总数 = 相关调类记录数
  sub <- df %>%
    filter(调类 %in% tone_vec) %>%
    group_by(姓名) %>%
    summarise(
      success = sum(.data[[success_col]], na.rm = TRUE),
      total   = n(),
      .groups = "drop"
    ) %>%
    filter(total > 0) %>%
    left_join(demo_df, by = "姓名") %>%
    filter(!is.na(性别), !is.na(年龄), !is.na(教育程度), !is.na(摩梭话比重))
  
  # 样本太少则跳过
  if (nrow(sub) < 10) {
    return(tibble(
      混类值 = y_name, 样本量 = nrow(sub), AIC = NA_real_,
      term = NA_character_, estimate = NA_real_, std.error = NA_real_,
      z = NA_real_, p = NA_real_,
      OR = NA_real_, OR_low = NA_real_, OR_high = NA_real_
    ))
  }
  
  m <- glm(
    cbind(success, total - success) ~ 性别 + 年龄 + 教育程度 + 摩梭话比重+ (1 | 姓名),
    data   = sub,
    family = binomial
  )
  
  co <- summary(m)$coefficients
  tibble(
    混类值   = y_name,
    样本量   = nrow(sub),
    AIC      = AIC(m),
    term     = rownames(co),
    estimate = co[, "Estimate"],
    std.error = co[, "Std. Error"],
    z        = co[, "z value"],
    p        = co[, "Pr(>|z|)"],
    OR       = exp(co[, "Estimate"]),
    OR_low   = exp(co[, "Estimate"] - 1.96 * co[, "Std. Error"]),
    OR_high  = exp(co[, "Estimate"] + 1.96 * co[, "Std. Error"])
  )
}

# ---------- 4. 批量建模 12 个混类值 ----------
results <- pmap_dfr(
  list(mix_specs$混类值, mix_specs$success_col, mix_specs$tone_filter),
  fit_glm_one
)

# 打印结果
# cat("\n===== 12 个逻辑回归模型结果 =====\n")
# print(as_tibble(results), n = Inf, width = Inf)

# 安装（只需一次）
# install.packages("writexl")

library(writexl)
library(dplyr)
library(tidyr)

# ---------- 1. 主结果表：一行一个系数 ----------
sheet_all <- results %>%
  mutate(
    across(c(estimate, std.error, z, p, OR, OR_low, OR_high),
           ~ round(.x, 4))
  ) %>%
  select(混类值, 样本量, AIC, term, estimate, std.error, z, p,
         OR, OR_low, OR_high)

# ---------- 2. p 值宽表：行 = 混类值，列 = 自变量 ----------
sheet_p <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(p = round(p, 4)) %>%
  select(混类值, term, p) %>%
  pivot_wider(names_from = term, values_from = p)

# ---------- 3. OR 宽表 ----------
sheet_or <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 3)) %>%
  select(混类值, term, OR) %>%
  pivot_wider(names_from = term, values_from = OR)

# ---------- 4. OR 95% CI 宽表（写论文用） ----------
sheet_or_ci <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(`OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR, OR_low, OR_high)) %>%
  select(混类值, term, `OR (95% CI)`) %>%
  pivot_wider(names_from = term, values_from = `OR (95% CI)`)

# ---------- 5. 模型概览 ----------
sheet_model <- results %>%
  distinct(混类值, 样本量, AIC) %>%
  mutate(AIC = round(AIC, 2))

# ---------- 6. 一次性写入一个 Excel，多个 sheet ----------
write_xlsx(
  list(
    全部结果  = sheet_all,
    p值宽表   = sheet_p,
    OR宽表    = sheet_or,
    OR_CI宽表 = sheet_or_ci,
    模型概览  = sheet_model
  ),
  path = "glm_results.xlsx"
)

cat("已保存：", normalizePath("glm_results.xlsx"), "\n")