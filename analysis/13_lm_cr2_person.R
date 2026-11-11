# ============================================================
# 每人 × 调域 × env 均值（40 个观测）+ lm + CR2
# 独立运行版：从 long_df 构造开始.
# 需要先依次运行value_assignment.R和merge_COP_EXT.R
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(clubSandwich)
library(writexl)

# ---------- 0. 确认 demo_df 有分析所需列 ----------
# 如果 demo_df 还没有 教育程度2 / 年龄_z / 摩梭话_z，先补上
if (!all(c("年龄_z", "摩梭话_z", "教育程度2") %in% names(demo_df))) {
  demo_df <- demo_df %>%
    mutate(
      年龄_z    = as.numeric(scale(年龄)),
      摩梭话_z  = as.numeric(scale(摩梭话比重)),
      教育程度2 = factor(ifelse(教育程度 == "无", "无", "有"),
                     levels = c("无", "有"))
    )
}

# ---------- 1. 构造长格式 long_df ----------
long_df <- raw_df %>%
  filter(调类 %in% c("A1", "A2", "B1", "B2")) %>%
  select(姓名, 调类, y_A_COP, y_A_EXT, y_B_COP, y_B_EXT) %>%
  pivot_longer(
    cols = c(y_A_COP, y_A_EXT, y_B_COP, y_B_EXT),
    names_to = c("调域", "env"),
    names_pattern = "y_([AB])_(COP|EXT)",
    values_to = "y"
  ) %>%
  filter(!is.na(y)) %>%
  mutate(
    env  = factor(env,  levels = c("COP", "EXT")),
    调域 = factor(调域, levels = c("A", "B"))
  ) %>%
  left_join(demo_df, by = "姓名") %>%
  filter(!is.na(性别), !is.na(年龄_z),
         !is.na(教育程度2), !is.na(摩梭话_z))

cat("===== long_df 概览 =====\n")
cat("总记录数：", nrow(long_df), "\n")
print(long_df %>%
        group_by(调域, env) %>%
        summarise(记录数 = n(), 事件数 = sum(y), .groups = "drop"))

# ---------- 2. 每人 × 调域 × env 的混类率 ----------
person_rate_env <- long_df %>%
  group_by(姓名, 调域, env) %>%
  summarise(
    n_records = n(),
    n_events  = sum(y),
    rate      = n_events / n_records,
    .groups   = "drop"
  ) %>%
  left_join(demo_df, by = "姓名") %>%
  filter(!is.na(性别), !is.na(年龄_z),
         !is.na(教育程度2), !is.na(摩梭话_z))

cat("\n===== 模型数据概览（每人×环境） =====\n")
print(person_rate_env %>%
        group_by(调域) %>%
        summarise(观测数 = n(),
                  人数   = n_distinct(姓名),
                  最小率 = round(min(rate), 3),
                  最大率 = round(max(rate), 3),
                  .groups = "drop"))

cat("\n===== 边界值检查 =====\n")
print(person_rate_env %>%
        group_by(调域) %>%
        summarise(n_0 = sum(rate == 0),
                  n_1 = sum(rate == 1),
                  .groups = "drop"))

# ---------- 3. lm + CR2 建模函数 ----------
fit_lm_env <- function(domain) {
  sub <- person_rate_env %>%
    filter(调域 == domain) %>%
    mutate(姓名 = factor(姓名))
  
  cat("\n--- 模型2：", domain, "调域 ---\n")
  cat("  观测数 =", nrow(sub), "，人数 =", n_distinct(sub$姓名), "\n")
  
  empty_row <- tibble(
    调域 = domain, 观测数 = nrow(sub), 人数 = n_distinct(sub$姓名),
    term = NA_character_, estimate = NA_real_, std.error = NA_real_,
    df = NA_real_, t = NA_real_, p = NA_real_,
    CI_low = NA_real_, CI_high = NA_real_
  )
  
  m <- tryCatch(
    lm(rate ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z, data = sub),
    error = function(e) {
      message("  ❌ lm 失败: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(m)) return(empty_row)
  
  ct <- tryCatch(
    clubSandwich::coef_test(
      m, vcov = "CR2", cluster = sub$姓名,
      test = "Satterthwaite"
    ),
    error = function(e) {
      message("  ❌ coef_test 失败: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(ct)) return(empty_row)
  
  ci_low  <- ct$beta - qt(0.975, ct$df_Satt) * ct$SE
  ci_high <- ct$beta + qt(0.975, ct$df_Satt) * ct$SE
  
  tibble(
    调域      = domain,
    观测数    = nrow(sub),
    人数      = n_distinct(sub$姓名),
    term      = rownames(ct),
    estimate  = ct$beta,
    std.error = ct$SE,
    df        = ct$df_Satt,
    t         = ct$tstat,
    p         = ct$p_Satt,
    CI_low    = ci_low,
    CI_high   = ci_high
  )
}

results_comb2 <- map_dfr(c("A", "B"), fit_lm_env)

cat("\n===== 模型 2 结果（每人×环境均值 + lm + CR2） =====\n")
print(as_tibble(results_comb2), n = Inf, width = Inf)

# ---------- 4. 模型 2 概览 ----------
overview_comb2 <- person_rate_env %>%
  group_by(调域) %>%
  summarise(
    观测数   = n(),
    人数     = n_distinct(姓名),
    最小率   = round(min(rate), 3),
    最大率   = round(max(rate), 3),
    平均率   = round(mean(rate), 3),
    .groups  = "drop"
  )

# ---------- 5. 写入 Excel ----------
sheet_comb2_all <- results_comb2 %>%
  mutate(across(c(estimate, std.error, df, t, p, CI_low, CI_high),
                ~ round(.x, 4))) %>%
  select(调域, 观测数, 人数, term,
         estimate, std.error, df, t, p, CI_low, CI_high)

sheet_comb2_sig <- results_comb2 %>%
  filter(term != "(Intercept)") %>%
  mutate(across(c(estimate, p, CI_low, CI_high), ~ round(.x, 4))) %>%
  select(调域, term, estimate, p, CI_low, CI_high)

write_xlsx(
  list(
    "模型2_全部结果" = sheet_comb2_all,
    "模型2_主效应"   = sheet_comb2_sig,
    "模型2_概览" = overview_comb2
  ),
  path = "敏感性分析_模型2_261111.xlsx"
)

cat("\n已保存：", normalizePath("敏感性分析_模型2_261111.xlsx"), "\n")