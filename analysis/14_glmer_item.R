# ============================================================
# 记录层面 + glmer + 随机截距（报告方差 = 0）
# 独立运行版：从 long_df 构造开始
# 需要先依次运行value_assignment.R和merge_COP_EXT.R
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(lme4)
library(writexl)

# ---------- 0. 确认 demo_df 有分析所需列 ----------
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

# ---------- 2. glmer 建模函数 ----------
fit_glmer_one <- function(domain) {
  sub <- long_df %>%
    filter(调域 == domain) %>%
    mutate(姓名 = factor(姓名))
  
  cat("\n--- 模型 3：", domain, "调域 ---\n")
  cat("  记录数 =", nrow(sub),
      "，人数 =", n_distinct(sub$姓名), "\n")
  
  m <- tryCatch(
    glmer(
      y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z + (1 | 姓名),
      data    = sub,
      family  = binomial,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl = list(maxfun = 2e5))
    ),
    error = function(e) {
      message("  ❌ glmer 失败: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(m)) return(NULL)
  
  # ---- 随机效应方差 ----
  vc_df    <- as.data.frame(VarCorr(m))
  var_name <- vc_df$vcov[vc_df$grp == "姓名"]
  if (length(var_name) == 0) var_name <- vc_df$vcov[1]   # 兜底：取第一个
  singular <- isSingular(m)
  
  cat("  随机效应方差（姓名）= ", round(var_name, 6),
      "，isSingular =", singular, "\n")
  
  # ---- 固定效应 ----
  co <- summary(m)$coefficients
  tibble(
    调域         = domain,
    记录数       = nrow(sub),
    人数         = n_distinct(sub$姓名),
    随机效应方差 = var_name,
    isSingular   = singular,
    term         = rownames(co),
    estimate     = co[, "Estimate"],
    std.error    = co[, "Std. Error"],
    z            = co[, "z value"],
    p            = co[, "Pr(>|z|)"],
    OR           = exp(co[, "Estimate"]),
    OR_low       = exp(co[, "Estimate"] - 1.96 * co[, "Std. Error"]),
    OR_high      = exp(co[, "Estimate"] + 1.96 * co[, "Std. Error"])
  )
}

results_comb3 <- map_dfr(c("A", "B"), fit_glmer_one)

cat("\n===== 模型 3 结果（记录 + glmer） =====\n")
print(as_tibble(results_comb3), n = Inf, width = Inf)

# ---------- 3. 模型 3 的随机效应概览 ----------
overview_comb3 <- results_comb3 %>%
  distinct(调域, 记录数, 人数, 随机效应方差, isSingular)

# ---------- 4. 写入 Excel ----------
sheet_comb3_all <- results_comb3 %>%
  mutate(across(c(estimate, std.error, z, p, OR, OR_low, OR_high),
                ~ round(.x, 4))) %>%
  select(调域, 记录数, 人数, 随机效应方差, isSingular,
         term, estimate, std.error, z, p, OR, OR_low, OR_high)

sheet_comb3_random <- overview_comb3 %>%
  mutate(随机效应方差 = round(随机效应方差, 6))

write_xlsx(
  list(
    "模型3_全部结果" = sheet_comb3_all,
    "模型3_随机效应" = sheet_comb3_random
  ),
  path = "敏感性分析_模型3_261111.xlsx"
)

cat("\n已保存：", normalizePath("敏感性分析_模型3_261111.xlsx"), "\n")