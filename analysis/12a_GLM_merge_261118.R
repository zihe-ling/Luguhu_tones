# ============================================================
# A、B 调域分别建模（均 glm + CR2）
# A: y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z
# B: y ~ env * 调类 + 性别 + 年龄_z + 教育程度2 + 摩梭话_z
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(clubSandwich)
library(writexl)

# ---------- 1. 构造 long_df（含 调类 列） ----------
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
    调域 = factor(调域, levels = c("A", "B")),
    调类 = factor(调类, levels = c("A1", "A2", "B1", "B2"))
  ) %>%
  left_join(demo_df, by = "姓名") %>%
  filter(!is.na(性别), !is.na(年龄_z),
         !is.na(教育程度2), !is.na(摩梭话_z))

cat("===== long_df 概览 =====\n")
print(long_df %>%
        group_by(调域, env, 调类) %>%
        summarise(记录数 = n(), 事件数 = sum(y), .groups = "drop"))

# ---------- 2. 通用提取函数：glm + CR2 + t 分布 CI ----------
fit_and_extract <- function(m, sub, domain) {
  ct <- tryCatch(
    clubSandwich::coef_test(
      m, vcov = "CR2", cluster = sub$姓名,
      test = "Satterthwaite"
    ),
    error = function(e) {
      message("  ❌ coef_test 失败: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(ct)) return(NULL)
  
  ci_low  <- ct$beta - qt(0.975, ct$df_Satt) * ct$SE
  ci_high <- ct$beta + qt(0.975, ct$df_Satt) * ct$SE
  
  tibble(
    调域      = domain,
    记录数    = nrow(sub),
    人数      = n_distinct(sub$姓名),
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

# ---------- 3. A 调域：原公式 ----------
sub_A <- long_df %>%
  filter(调域 == "A") %>%
  mutate(姓名 = factor(姓名))

m_A <- glm(
  y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = sub_A, family = binomial
)

cat("\n===== A 调域模型 =====\n")
cat("  公式：y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z\n")
cat("  记录数 =", nrow(sub_A),
    "，人数 =", n_distinct(sub_A$姓名),
    "，事件数 =", sum(sub_A$y), "\n")

res_A <- fit_and_extract(m_A, sub_A, "A")

# ---------- 4. B 调域：env * 调类 ----------
sub_B <- long_df %>%
  filter(调域 == "B") %>%
  mutate(
    姓名 = factor(姓名),
    调类 = droplevels(factor(调类, levels = c("B1", "B2")))  # 只保留 B1/B2
  )

m_B <- glm(
  y ~ env * 调类 + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = sub_B, family = binomial
)

cat("\n===== B 调域模型 =====\n")
cat("  公式：y ~ env * 调类 + 性别 + 年龄_z + 教育程度2 + 摩梭话_z\n")
cat("  记录数 =", nrow(sub_B),
    "，人数 =", n_distinct(sub_B$姓名),
    "，事件数 =", sum(sub_B$y), "\n")
cat("  调类水平：", paste(levels(sub_B$调类), collapse = " / "), "\n")

res_B <- fit_and_extract(m_B, sub_B, "B")

# ---------- 5. 合并结果 ----------
results_AB <- bind_rows(res_A, res_B)

cat("\n===== A、B 调域结果 =====\n")
print(as_tibble(results_AB), n = Inf, width = Inf)

# ---------- 6. B 调域交互项的单独解读 ----------
cat("\n===== B 调域：env × 调类 交互项 =====\n")
print(results_AB %>%
        filter(调域 == "B", term == "envEXT:调类B2") %>%
        mutate(across(c(estimate, std.error, df, t, p, OR, OR_low, OR_high),
                      ~ round(.x, 4))),
      n = Inf, width = Inf)

# 用 LRT 整体检验 env × 调类 交互
m_B_no_int <- glm(
  y ~ env + 调类 + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = sub_B, family = binomial
)
cat("\n===== B 调域：env × 调类 LRT =====\n")
print(anova(m_B_no_int, m_B, test = "Chisq"))

# ---------- 7. 模型概览 ----------
overview <- results_AB %>%
  group_by(调域) %>%
  summarise(
    记录数    = first(记录数),
    人数      = first(人数),
    事件数    = sum(y_event = NA),   # 占位
    df最小    = round(min(df, na.rm = TRUE), 2),
    df最大    = round(max(df, na.rm = TRUE), 2),
    .groups   = "drop"
  ) %>%
  select(-事件数)

# 事件数单独算
event_summary <- long_df %>%
  filter(调域 %in% c("A", "B")) %>%
  group_by(调域) %>%
  summarise(事件数 = sum(y), 事件率 = round(mean(y), 3), .groups = "drop")

overview <- overview %>% left_join(event_summary, by = "调域")

cat("\n===== 模型概览 =====\n")
print(overview)

# ---------- 8. 写入 Excel ----------
round_df <- function(df, cols, digits = 4) {
  df %>% mutate(across(all_of(cols), ~ round(.x, digits)))
}

sheet_all <- results_AB %>%
  round_df(c("estimate", "std.error", "df", "t", "p",
             "OR", "OR_low", "OR_high")) %>%
  select(调域, 记录数, 人数, term,
         estimate, std.error, df, t, p, OR, OR_low, OR_high)

sheet_A <- results_AB %>%
  filter(调域 == "A") %>%
  round_df(c("estimate", "std.error", "df", "t", "p",
             "OR", "OR_low", "OR_high")) %>%
  select(调域, term, estimate, std.error, df, t, p, OR, OR_low, OR_high)

sheet_B <- results_AB %>%
  filter(调域 == "B") %>%
  round_df(c("estimate", "std.error", "df", "t", "p",
             "OR", "OR_low", "OR_high")) %>%
  select(调域, term, estimate, std.error, df, t, p, OR, OR_low, OR_high)

sheet_p <- results_AB %>%
  filter(term != "(Intercept)") %>%
  mutate(p = round(p, 4)) %>%
  select(调域, term, p) %>%
  pivot_wider(names_from = term, values_from = p)

sheet_or <- results_AB %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 3)) %>%
  select(调域, term, OR) %>%
  pivot_wider(names_from = term, values_from = OR)

write_xlsx(
  list(
    "全部结果"   = sheet_all,
    "A调域结果"  = sheet_A,
    "B调域结果"  = sheet_B,
    "p值宽表"    = sheet_p,
    "OR宽表"     = sheet_or,
    "模型概览"   = overview
  ),
  path = "C:/Luguhu_tones/data/AB分域模型_results.xlsx"
)

cat("\n已保存：", normalizePath("C:/Luguhu_tones/data/AB分域模型_results.xlsx"), "\n")