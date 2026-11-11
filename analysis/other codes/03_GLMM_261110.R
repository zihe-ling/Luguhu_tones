library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(lme4)
library(writexl)

# ---------- 0. 读入数据（变量名用 raw_df，避免和 df() 冲突） ----------
raw_df <- read_excel("Luguhu_tone_variation_261110.xlsx")

# 如果你还没给 raw_df 赋值混类列，这里补上
raw_df <- raw_df %>%
  mutate(
    COP_A1toA2 = 0, COP_A2toA1 = 0,
    COP_B1toB2 = 0, COP_B2toB1 = 0,
    EXT_A1toA2 = 0, EXT_A2toA1 = 0,
    EXT_B1toB2 = 0, EXT_B2toB1 = 0,
    COP_AtoB   = 0, COP_BtoA   = 0,
    EXT_AtoB   = 0, EXT_BtoA   = 0,
    EXT_iso    = 0
  ) %>%
  mutate(
    COP_A1toA2 = ifelse(调类 == "A1" & grepl("HR",  COP, ignore.case = FALSE), 1, COP_A1toA2),
    COP_A2toA1 = ifelse(调类 == "A2" & grepl("HF",  COP, ignore.case = FALSE), 1, COP_A2toA1),
    COP_B1toB2 = ifelse(调类 == "B1" & grepl("LF",  COP, ignore.case = FALSE), 1, COP_B1toB2),
    COP_B2toB1 = ifelse(调类 == "B2" & grepl("LR",  COP, ignore.case = FALSE), 1, COP_B2toB1),
    EXT_A1toA2 = ifelse(调类 == "A1" & grepl("HLR", EXT, ignore.case = FALSE), 1, EXT_A1toA2),
    EXT_A2toA1 = ifelse(调类 == "A2" & grepl("HHF", EXT, ignore.case = FALSE), 1, EXT_A2toA1),
    EXT_B1toB2 = ifelse(调类 == "B1" & grepl("LHF", EXT, ignore.case = FALSE), 1, EXT_B1toB2),
    EXT_B2toB1 = ifelse(调类 == "B2" & grepl("LLH", EXT, ignore.case = FALSE), 1, EXT_B2toB1),
    COP_AtoB   = ifelse(调类 %in% c("A1","A2") & grepl("^L", COP), 1, COP_AtoB),
    COP_BtoA   = ifelse(调类 %in% c("B1","B2") & grepl("^H", COP), 1, COP_BtoA),
    EXT_AtoB   = ifelse(调类 %in% c("A1","A2") & grepl("^L", EXT), 1, EXT_AtoB),
    EXT_BtoA   = ifelse(调类 %in% c("B1","B2") & grepl("^H", EXT), 1, EXT_BtoA),
    EXT_iso    = ifelse(grepl("^R[A-Z]{2}", EXT), 1, EXT_iso)
  )

# ---------- 1. 人口学变量 ----------
demo_df <- raw_df %>%
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
    教育程度 = case_when(
      教育程度 %in% c("无", "未上学", "没上过学") ~ "无",
      教育程度 %in% c("小学", "初中")             ~ "小学及初中",
      教育程度 %in% c("高中", "中专", "大专")     ~ "高中及以上",
      TRUE                                       ~ NA_character_
    ),
    教育程度 = factor(教育程度,
                  levels = c("无", "小学及初中", "高中及以上"))
  )

# ---------- 2. 混类值定义 ----------
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

# ---------- 3. 单模型函数 ----------
fit_glmm_one <- function(y_name, success_col, tone_filter) {
  tone_vec <- strsplit(tone_filter, ",")[[1]]
  
  sub_df <- raw_df %>%
    filter(调类 %in% tone_vec) %>%
    select(姓名, y = all_of(success_col)) %>%          # 只取姓名和 y
    mutate(姓名 = trimws(姓名)) %>%
    left_join(demo_df %>% mutate(姓名 = trimws(姓名)), by = "姓名") %>%
    mutate(
      年龄     = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年),
      性别     = factor(性别),
      教育程度 = factor(教育程度,
                    levels = c("无", "小学及初中", "高中及以上")),
      姓名     = factor(姓名)
    ) %>%
    filter(!is.na(y), !is.na(性别), !is.na(年龄),
           !is.na(教育程度), !is.na(摩梭话比重))
  
  cat("---", y_name, ": 记录数 =", nrow(sub_df),
      "，人数 =", n_distinct(sub_df$姓名), "---\n")
  
  empty_row <- tibble(
    混类值 = y_name, 记录数 = nrow(sub_df), 人数 = n_distinct(sub_df$姓名),
    AIC = NA_real_, term = NA_character_, estimate = NA_real_,
    std.error = NA_real_, z = NA_real_, p = NA_real_,
    OR = NA_real_, OR_low = NA_real_, OR_high = NA_real_
  )
  
  if (nrow(sub_df) < 10 || length(unique(sub_df$y)) < 2) {
    message("跳过 ", y_name, "：响应为常量或记录数不足（事件数 = ",
            sum(sub_df$y == 1), " / ", nrow(sub_df), "）")
    return(empty_row)
  }
  
  rec_per_person <- sub_df %>% count(姓名)
  if (nrow(rec_per_person) < 5 || sum(rec_per_person$n >= 2) < 3) {
    message("跳过 ", y_name, "：随机效应不可识别（人数 = ",
            nrow(rec_per_person), "，每人≥2条的人数 = ",
            sum(rec_per_person$n >= 2), "）")
    return(empty_row)
  }
  
  m <- tryCatch(
    glmer(
      y ~ 性别 + 年龄 + 教育程度 + 摩梭话比重 + (1 | 姓名),
      data    = sub_df,
      family  = binomial,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl = list(maxfun = 2e5))
    ),
    error = function(e) {
      message("模型 ", y_name, " 拟合失败: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(m)) return(empty_row)
  if (isSingular(m)) {
    message("注意：", y_name, " 出现奇异拟合（随机效应方差估计为 0）")
  }
  
  co <- summary(m)$coefficients
  tibble(
    混类值    = y_name,
    记录数    = nrow(sub_df),
    人数      = n_distinct(sub_df$姓名),
    AIC       = AIC(m),
    term      = rownames(co),
    estimate  = co[, "Estimate"],
    std.error = co[, "Std. Error"],
    z         = co[, "z value"],
    p         = co[, "Pr(>|z|)"],
    OR        = exp(co[, "Estimate"]),
    OR_low    = exp(co[, "Estimate"] - 1.96 * co[, "Std. Error"]),
    OR_high   = exp(co[, "Estimate"] + 1.96 * co[, "Std. Error"])
  )
}

# ---------- 4. 批量建模 ----------
results <- pmap_dfr(
  list(mix_specs$混类值, mix_specs$success_col, mix_specs$tone_filter),
  fit_glmm_one
)

print(as_tibble(results), n = Inf, width = Inf)

#--------------write to excel---------------
sheet_all <- results %>%
  mutate(across(c(estimate, std.error, z, p, OR, OR_low, OR_high),
                ~ round(.x, 4))) %>%
  select(混类值, 记录数, 人数, AIC, term, estimate, std.error, z, p,
         OR, OR_low, OR_high)

sheet_p <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(p = round(p, 4)) %>%
  select(混类值, term, p) %>%
  pivot_wider(names_from = term, values_from = p)

sheet_or <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 3)) %>%
  select(混类值, term, OR) %>%
  pivot_wider(names_from = term, values_from = OR)

sheet_or_ci <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(`OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR, OR_low, OR_high)) %>%
  select(混类值, term, `OR (95% CI)`) %>%
  pivot_wider(names_from = term, values_from = `OR (95% CI)`)

sheet_model <- results %>%
  distinct(混类值, 记录数, 人数, AIC) %>%
  mutate(AIC = round(AIC, 2))

write_xlsx(
  list(
    全部结果  = sheet_all,
    p值宽表   = sheet_p,
    OR宽表    = sheet_or,
    OR_CI宽表 = sheet_or_ci,
    模型概览  = sheet_model
  ),
  path = "glmm_results_261110.xlsx"
)

cat("已保存：", normalizePath("glmm_results_261110.xlsx"), "\n")