library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(clubSandwich)

# ============================================================
# 1. 构造长格式 long_df：每个 (姓名, 调类) 产生 COP + EXT 两行
# ============================================================
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

# 检查
cat("长格式记录数：", nrow(long_df), "\n")
cat("按调域 × 环境的事件数：\n")
print(long_df %>%
        group_by(调域, env) %>%
        summarise(记录数 = n(), 事件数 = sum(y), .groups = "drop"))
cat("按调域的事件数：\n")
print(long_df %>%
        group_by(调域) %>%
        summarise(记录数 = n(), 事件数 = sum(y), .groups = "drop"))

# ============================================================
# 2. 单个合并模型函数：env 作为固定效应
# ============================================================
fit_merged <- function(domain) {
  
  sub_df <- long_df %>%
    filter(调域 == domain) %>%
    mutate(姓名 = factor(姓名))
  
  n_people  <- n_distinct(sub_df$姓名)
  n_records <- nrow(sub_df)
  n_events  <- sum(sub_df$y)
  epv       <- round(n_events / 5, 1)   # 5 个参数：env + 性别 + 年龄 + 教育 + 摩梭话
  
  cat("\n--- 调域", domain, "---\n")
  cat("  人数 =", n_people,
      "，记录数 =", n_records,
      "，事件数 =", n_events,
      "，事件率 =", round(n_events / n_records, 3),
      "，EPV =", epv, "\n")
  
  empty_row <- tibble(
    调域 = domain, 人数 = n_people, 记录数 = n_records,
    事件数 = n_events, EPV = epv, term = NA_character_,
    estimate = NA_real_, std.error = NA_real_,
    df = NA_real_, t = NA_real_, p = NA_real_,
    OR = NA_real_, OR_low = NA_real_, OR_high = NA_real_
  )
  
  # ---- 普通 glm ----
  m <- tryCatch(
    glm(y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
        data = sub_df, family = binomial),
    error = function(e) {
      message("  ❌ glm 失败: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(m)) return(empty_row)
  
  # ---- 分离检测 ----
  if (any(abs(coef(m)) > 15, na.rm = TRUE)) {
    message("  ⚠️ 可能分离（|coef| > 15），考虑改用 Firth")
  }
  
  # ---- CR2 聚类稳健 SE ----
  ct <- tryCatch(
    clubSandwich::coef_test(
      m, vcov = "CR2", cluster = sub_df$姓名,
      test = "Satterthwaite"
    ),
    error = function(e) {
      message("  ❌ coef_test 失败: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(ct)) return(empty_row)
  
  # ---- t 分布 CI ----
  ci_low  <- ct$beta - qt(0.975, ct$df_Satt) * ct$SE
  ci_high <- ct$beta + qt(0.975, ct$df_Satt) * ct$SE
  
  tibble(
    调域      = domain,
    人数      = n_people,
    记录数    = n_records,
    事件数    = n_events,
    EPV       = epv,
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

# ============================================================
# 3. 跑 2 个合并模型
# ============================================================
results_merged <- map_dfr(c("A", "B"), fit_merged)

cat("\n===== 合并模型结果 =====\n")
print(as_tibble(results_merged), n = Inf, width = Inf)