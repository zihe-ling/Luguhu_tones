
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(logistf)      # install.packages("logistf")
library(writexl)

# ---------- 1. 读入带混类标记的文件 ----------
raw_df <- read_excel("Luguhu_tone_variation_261110_with_flags.xlsx")

names(raw_df) <- names(raw_df) %>%
  gsub("^\ufeff", "", .) %>%
  trimws()

cat("读入成功：", nrow(raw_df), "条记录，", n_distinct(raw_df$姓名), "人\n")

# 确认 13 个混类列都在
flag_cols <- c(
  "COP_A1toA2", "COP_A2toA1", "COP_B1toB2", "COP_B2toB1",
  "EXT_A1toA2", "EXT_A2toA1", "EXT_B1toB2", "EXT_B2toB1",
  "COP_AtoB", "COP_BtoA", "EXT_AtoB", "EXT_BtoA", "EXT_iso"
)
stopifnot(all(flag_cols %in% names(raw_df)))

# ---------- 2. 构造 6 个统一 y 列 ----------
raw_df <- raw_df %>%
  mutate(
    y_A_COP = case_when(
      调类 == "A1" ~ COP_A1toA2,
      调类 == "A2" ~ COP_A2toA1,
      TRUE          ~ NA_real_
    ),
    y_B_COP = case_when(
      调类 == "B1" ~ COP_B1toB2,
      调类 == "B2" ~ COP_B2toB1,
      TRUE          ~ NA_real_
    ),
    y_AB_COP = case_when(
      调类 %in% c("A1","A2") ~ COP_AtoB,
      调类 %in% c("B1","B2") ~ COP_BtoA,
      TRUE                    ~ NA_real_
    ),
    y_A_EXT = case_when(
      调类 == "A1" ~ EXT_A1toA2,
      调类 == "A2" ~ EXT_A2toA1,
      TRUE          ~ NA_real_
    ),
    y_B_EXT = case_when(
      调类 == "B1" ~ EXT_B1toB2,
      调类 == "B2" ~ EXT_B2toB1,
      TRUE          ~ NA_real_
    ),
    y_AB_EXT = case_when(
      调类 %in% c("A1","A2") ~ EXT_AtoB,
      调类 %in% c("B1","B2") ~ EXT_BtoA,
      TRUE                    ~ NA_real_
    )
  )

# ---------- 3. demo_df ----------
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
      TRUE                                       ~ as.character(教育程度)
    ),
    教育程度 = factor(教育程度,
                  levels = c("无", "小学及初中", "高中及以上"))
  )

cat("\n===== demo_df 教育程度分布 =====\n")
print(table(demo_df$教育程度, useNA = "ifany"))
cat("demo_df 人数：", nrow(demo_df), "\n")

# ---------- 4. 6 个混类值定义 ----------
mix_specs_6 <- tibble(
  混类值 = c("A_COP", "B_COP", "AB_COP", "A_EXT", "B_EXT", "AB_EXT"),
  y_col  = c("y_A_COP", "y_B_COP", "y_AB_COP",
             "y_A_EXT", "y_B_EXT", "y_AB_EXT")
)

# ---------- 5. 单个 Firth 模型函数 ----------
fit_firth_one <- function(y_name, y_col) {
  # ---- 1. 按人聚合 ----
  sub_agg <- raw_df %>%
    filter(!is.na(.data[[y_col]])) %>%
    group_by(姓名) %>%
    summarise(
      success = sum(.data[[y_col]], na.rm = TRUE),
      total   = n(),
      .groups = "drop"
    ) %>%
    left_join(demo_df, by = "姓名") %>%
    filter(!is.na(性别), !is.na(年龄),
           !is.na(教育程度), !is.na(摩梭话比重))
  
  n_people  <- nrow(sub_agg)
  n_records <- sum(sub_agg$total)
  n_events  <- sum(sub_agg$success)
  
  cat("---", y_name, ": 人数 =", n_people,
      "，总记录数 =", n_records,
      "，事件数 =", n_events,
      "，事件率 =", round(n_events / n_records, 3), "---\n")
  
  empty_row <- tibble(
    混类值 = y_name, 人数 = n_people, 总记录数 = n_records,
    事件数 = n_events, term = NA_character_,
    estimate = NA_real_, std.error = NA_real_, z = NA_real_, p = NA_real_,
    OR = NA_real_, OR_low = NA_real_, OR_high = NA_real_
  )
  
  if (n_people < 10 || n_events == 0 || n_events == n_records) {
    message("跳过 ", y_name, "：人数不足或响应为常量")
    return(empty_row)
  }
  
  # ---- 2. 展开成长格式 0/1（关键修改）----
  sub_long <- sub_agg %>%
    rowwise() %>%
    do(tibble(
      姓名       = .$姓名,
      性别       = .$性别,
      年龄       = .$年龄,
      教育程度   = .$教育程度,
      摩梭话比重 = .$摩梭话比重,
      y          = c(rep(1, .$success),
                     rep(0, .$total - .$success))
    )) %>%
    ungroup()
  
  cat("    展开后记录数：", nrow(sub_long),
      "，其中 y=1：", sum(sub_long$y), "\n")
  
  # ---- 3. Firth 惩罚逻辑回归（y 是 0/1）----
  m <- tryCatch(
    logistf(y ~ 性别 + 年龄 + 教育程度 + 摩梭话比重,
            data = sub_long),
    error = function(e) {
      message("模型 ", y_name, " 拟合失败: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(m)) return(empty_row)
  
  # ---- 4. 提取结果 ----
  coefs <- coef(m)
  se    <- sqrt(diag(vcov(m)))
  
  tibble(
    混类值    = y_name,
    人数      = n_people,
    总记录数  = n_records,
    事件数    = n_events,
    term      = names(coefs),
    estimate  = unname(coefs),
    std.error = unname(se),
    z         = unname(coefs / se),
    p         = unname(m$prob),
    OR        = unname(exp(coefs)),
    OR_low    = unname(exp(m$ci.lower)),
    OR_high   = unname(exp(m$ci.upper))
  )
}

# ---------- 6. 批量建模 ----------
results_firth <- pmap_dfr(
  list(mix_specs_6$混类值, mix_specs_6$y_col),
  fit_firth_one
)

print(as_tibble(results_firth), n = Inf, width = Inf)

# ---------- 7. 导出 ----------
sheet_all <- results_firth %>%
  mutate(across(c(estimate, std.error, z, p, OR, OR_low, OR_high),
                ~ round(.x, 4))) %>%
  select(混类值, 人数, 总记录数, 事件数,
         term, estimate, std.error, z, p, OR, OR_low, OR_high)

sheet_p <- results_firth %>%
  filter(term != "(Intercept)") %>%
  mutate(p = round(p, 4)) %>%
  select(混类值, term, p) %>%
  pivot_wider(names_from = term, values_from = p)

sheet_or <- results_firth %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 3)) %>%
  select(混类值, term, OR) %>%
  pivot_wider(names_from = term, values_from = OR)

sheet_or_ci <- results_firth %>%
  filter(term != "(Intercept)") %>%
  mutate(`OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR, OR_low, OR_high)) %>%
  select(混类值, term, `OR (95% CI)`) %>%
  pivot_wider(names_from = term, values_from = `OR (95% CI)`)

sheet_overview <- results_firth %>%
  distinct(混类值, 人数, 总记录数, 事件数) %>%
  mutate(事件率 = round(事件数 / 总记录数, 3))

write_xlsx(
  list(
    全部结果  = sheet_all,
    p值宽表   = sheet_p,
    OR宽表    = sheet_or,
    OR_CI宽表 = sheet_or_ci,
    模型概览  = sheet_overview
  ),
  path = "firth_6models_261110.xlsx"
)

cat("\n已保存：", normalizePath("firth_6models_261110.xlsx"), "\n")