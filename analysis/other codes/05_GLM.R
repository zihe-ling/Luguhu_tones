# ============================================================
# 普通逻辑回归 + CR2 聚类稳健标准误
# 4 个混类值：A_COP、B_COP、A_EXT、B_EXT
# 从读入 with_flags.xlsx 开始
# ============================================================

# 安装所需包（如未安装，取消注释后运行）
# install.packages(c("readxl", "dplyr", "tidyr", "purrr",
#                    "tibble", "clubSandwich", "writexl"))

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(clubSandwich)
library(writexl)

# ---------- 1. 读入带混类标记的文件 ----------
raw_df <- read_excel("Luguhu_tone_variation_261110_with_flags.xlsx")

# 清理列名（去 BOM、前后空格）
names(raw_df) <- names(raw_df) %>%
  gsub("^\ufeff", "", .) %>%
  trimws()

cat("读入成功：", nrow(raw_df), "条记录，",
    n_distinct(raw_df$姓名), "人\n")

# ---------- 2. 确认 13 个混类列都在 ----------
flag_cols <- c(
  "COP_A1toA2", "COP_A2toA1", "COP_B1toB2", "COP_B2toB1",
  "EXT_A1toA2", "EXT_A2toA1", "EXT_B1toB2", "EXT_B2toB1",
  "COP_AtoB",   "COP_BtoA",   "EXT_AtoB",   "EXT_BtoA", "EXT_iso"
)
stopifnot(all(flag_cols %in% names(raw_df)))

# ---------- 3. 构造 4 个统一 y 列 ----------
raw_df <- raw_df %>%
  mutate(
    y_A_COP = case_when(
      调类 == "A1" ~ as.numeric(COP_A1toA2),
      调类 == "A2" ~ as.numeric(COP_A2toA1),
      TRUE          ~ NA_real_
    ),
    y_B_COP = case_when(
      调类 == "B1" ~ as.numeric(COP_B1toB2),
      调类 == "B2" ~ as.numeric(COP_B2toB1),
      TRUE          ~ NA_real_
    ),
    y_A_EXT = case_when(
      调类 == "A1" ~ as.numeric(EXT_A1toA2),
      调类 == "A2" ~ as.numeric(EXT_A2toA1),
      TRUE          ~ NA_real_
    ),
    y_B_EXT = case_when(
      调类 == "B1" ~ as.numeric(EXT_B1toB2),
      调类 == "B2" ~ as.numeric(EXT_B2toB1),
      TRUE          ~ NA_real_
    )
  )

# 自检：4 个 y 列的事件数
cat("\n===== 4 个统一 y 列的事件数 =====\n")
for (col in c("y_A_COP", "y_B_COP", "y_A_EXT", "y_B_EXT")) {
  n <- sum(!is.na(raw_df[[col]]))
  e <- sum(raw_df[[col]], na.rm = TRUE)
  cat(col, ": 记录数 =", n,
      "，事件数 =", e,
      "，事件率 =", round(e / n, 3), "\n")
}

# ---------- 4. demo_df：每人一行的人口学变量 ----------
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

demo_df <- demo_df %>%
  mutate(
    年龄_z   = as.numeric(scale(年龄)),
    摩梭话_z = as.numeric(scale(摩梭话比重))
  )

demo_df <- demo_df %>%
  mutate(
    教育程度2 = factor(
      ifelse(教育程度 == "无", "无", "有"),
      levels = c("无", "有")
    )
  )

# 检查
cat("\n===== 教育程度二分分布 =====\n")
print(table(demo_df$教育程度2, useNA = "ifany"))

# ---------- 5. 4 个混类值定义 ----------
mix_specs_4 <- tibble(
  混类值 = c("A_COP", "B_COP", "A_EXT", "B_EXT"),
  y_col  = c("y_A_COP", "y_B_COP", "y_A_EXT", "y_B_EXT")
)

# ---------- 6. 单个模型函数：glm + CR2 ----------
fit_glm_cr2 <- function(y_name, y_col) {
  
  sub_agg <- raw_df %>%
    filter(!is.na(.data[[y_col]])) %>%
    group_by(姓名) %>%
    summarise(
      success = sum(.data[[y_col]], na.rm = TRUE),
      total   = n(),
      .groups = "drop"
    ) %>%
    left_join(demo_df, by = "姓名") %>%
    filter(!is.na(性别), !is.na(年龄_z),
           !is.na(教育程度2), !is.na(摩梭话_z))
  
  n_people  <- nrow(sub_agg)
  n_records <- sum(sub_agg$total)
  n_events  <- sum(sub_agg$success)
  
  cat("\n---", y_name, "---\n")
  cat("  人数 =", n_people,
      "，记录数 =", n_records,
      "，事件数 =", n_events,
      "，EPV =", round(n_events / 4, 1), "\n")  # 4 个参数（教育二分后）
  
  empty_row <- tibble(
    混类值 = y_name, 人数 = n_people, 总记录数 = n_records,
    事件数 = n_events, EPV = round(n_events / 4, 1),
    term = NA_character_,
    estimate = NA_real_, std.error = NA_real_,
    df = NA_real_, t = NA_real_, p = NA_real_,
    OR = NA_real_, OR_low = NA_real_, OR_high = NA_real_
  )
  
  if (n_people < 10 || n_events < 3 || n_events == n_records) {
    message("跳过 ", y_name)
    return(empty_row)
  }
  
  # ★ 关键：do(tibble(...)) 用新列
  sub_long <- sub_agg %>%
    rowwise() %>%
    do(tibble(
      姓名       = .$姓名,
      性别       = .$性别,
      年龄_z     = .$年龄_z,
      教育程度2  = .$教育程度2,
      摩梭话_z   = .$摩梭话_z,
      y          = c(rep(1, .$success),
                     rep(0, .$total - .$success))
    )) %>%
    ungroup() %>%
    mutate(姓名 = factor(姓名))
  
  # ★ 关键：glm 公式用新列，且打印错误
  m <- tryCatch(
    glm(y ~ 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
        data = sub_long, family = binomial),
    error = function(e) {
      message("  ❌ ", y_name, " glm 失败: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(m)) return(empty_row)
  
  # ★ CR2，打印错误
  ct <- tryCatch(
    clubSandwich::coef_test(
      m, vcov = "CR2", cluster = sub_long$姓名,
      test = "Satterthwaite"
    ),
    error = function(e) {
      message("  ❌ ", y_name, " coef_test 失败: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(ct)) return(empty_row)
  
  # ★ 用正确的列名提取
  ci_low  <- ct$beta - qt(0.975, ct$df_Satt) * ct$SE
  ci_high <- ct$beta + qt(0.975, ct$df_Satt) * ct$SE
  
  tibble(
    混类值    = y_name,
    人数      = n_people,
    总记录数  = n_records,
    事件数    = n_events,
    EPV       = round(n_events / 4, 1),
    term      = rownames(ct),
    estimate  = ct$beta,
    std.error = ct$SE,
    df        = ct$df_Satt,
    t         = ct$tstat,
    p         = ct$p_Satt,
    OR        = exp(ct$beta),
    OR_low    = exp(ci_low),
    OR_high   = exp(ci_high)
  )
}

# ---------- 7. 批量建模 4 个混类值 ----------
results_4 <- pmap_dfr(
  list(mix_specs_4$混类值, mix_specs_4$y_col),
  fit_glm_cr2
)

cat("\n===== 4 个模型结果（长格式） =====\n")
print(as_tibble(results_4), n = Inf, width = Inf)

# ---------- 8. 整理并导出 Excel ----------
sheet_all <- results_4 %>%
  mutate(across(c(estimate, std.error, df, t, p, OR, OR_low, OR_high),
                ~ round(.x, 4))) %>%
  select(混类值, 人数, 总记录数, 事件数, EPV,
         term, estimate, std.error, df, t, p, OR, OR_low, OR_high)

sheet_p <- results_4 %>%
  filter(term != "(Intercept)") %>%
  mutate(p = round(p, 4)) %>%
  select(混类值, term, p) %>%
  pivot_wider(names_from = term, values_from = p)

sheet_or <- results_4 %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 3)) %>%
  select(混类值, term, OR) %>%
  pivot_wider(names_from = term, values_from = OR)

sheet_or_ci <- results_4 %>%
  filter(term != "(Intercept)") %>%
  mutate(`OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)",
                                 OR, OR_low, OR_high)) %>%
  select(混类值, term, `OR (95% CI)`) %>%
  pivot_wider(names_from = term, values_from = `OR (95% CI)`)

sheet_overview <- results_4 %>%
  distinct(混类值, 人数, 总记录数, 事件数, EPV) %>%
  mutate(事件率 = round(事件数 / 总记录数, 3))

write_xlsx(
  list(
    全部结果  = sheet_all,
    p值宽表   = sheet_p,
    OR宽表    = sheet_or,
    OR_CI宽表 = sheet_or_ci,
    模型概览  = sheet_overview
  ),
  path = "glm_cr2_4models_261110.xlsx"
)

cat("\n已保存：", normalizePath("glm_cr2_4models_261110.xlsx"), "\n")