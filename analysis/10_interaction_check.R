# ============================================================
# 交互检验汇总 + 写入 Excel
# 需要先依次运行value_assignment.R
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(clubSandwich)
library(writexl)

# ---------- 1. 构造 long_df ----------
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

all_vars <- c("性别", "年龄_z", "教育程度2", "摩梭话_z")

# ---------- 2. 主效应模型（含 CR2 结果） ----------
fit_main <- function(domain) {
  sub <- long_df %>% filter(调域 == domain) %>% mutate(姓名 = factor(姓名))
  
  m <- glm(y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
           data = sub, family = binomial)
  
  ct <- clubSandwich::coef_test(
    m, vcov = "CR2", cluster = sub$姓名,
    test = "Satterthwaite"
  )
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

results_main <- map_dfr(c("A", "B"), fit_main)

# ---------- 3. 整体交互检验（env × 全部4变量，同时给 LRT 和 CR2 Wald） ----------
test_overall <- function(domain) {
  sub <- long_df %>% filter(调域 == domain) %>% mutate(姓名 = factor(姓名))
  
  m_main <- glm(y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
                data = sub, family = binomial)
  m_int  <- glm(y ~ env * (性别 + 年龄_z + 教育程度2 + 摩梭话_z),
                data = sub, family = binomial)
  
  lrt <- anova(m_main, m_int, test = "Chisq")
  int_terms <- grep(":", names(coef(m_int)), value = TRUE)
  
  wald_res <- tryCatch({
    wt <- clubSandwich::Wald_test(
      m_int,
      constraints = clubSandwich::constrain_zero(int_terms),
      vcov        = "CR2",
      cluster     = sub$姓名,
      test        = "HTZ"
    )
    tibble(
      CR2_Wald_F    = wt$Fstat,
      CR2_Wald_df1  = wt$df_num,
      CR2_Wald_df2  = wt$df_denom,
      CR2_Wald_p    = wt$p_val
    )
  }, error = function(e) {
    tibble(CR2_Wald_F = NA, CR2_Wald_df1 = NA,
           CR2_Wald_df2 = NA, CR2_Wald_p = NA)
  })
  
  bind_cols(
    tibble(
      调域       = domain,
      交互项数   = length(int_terms),
      Df变化     = lrt$Df[2],
      Deviance变化 = lrt$Deviance[2],
      LRT_p      = lrt$`Pr(>Chi)`[2]
    ),
    wald_res
  )
}

results_overall <- map_dfr(c("A", "B"), test_overall)

# ---------- 4. 单独交互检验（env × 每个变量，LRT + CR2 Wald 单参数） ----------
test_single <- function(domain, var) {
  sub <- long_df %>% filter(调域 == domain) %>% mutate(姓名 = factor(姓名))
  other_vars <- setdiff(all_vars, var)
  
  f_main <- y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z
  f_int  <- as.formula(paste(
    "y ~ env +", var, "+ env:", var, "+",
    paste(other_vars, collapse = " + ")
  ))
  
  m_main <- glm(f_main, data = sub, family = binomial)
  m_int  <- glm(f_int,  data = sub, family = binomial)
  
  lrt <- anova(m_main, m_int, test = "Chisq")
  
  ct <- clubSandwich::coef_test(
    m_int, vcov = "CR2", cluster = sub$姓名,
    test = "Satterthwaite"
  )
  int_term <- grep(":", rownames(ct), value = TRUE)
  idx <- rownames(ct) == int_term
  
  tibble(
    调域             = domain,
    交互项           = paste0("env × ", var),
    Df变化           = lrt$Df[2],
    Deviance变化     = lrt$Deviance[2],
    LRT_p            = lrt$`Pr(>Chi)`[2],
    交互项_estimate  = ct$beta[idx],
    交互项_SE        = ct$SE[idx],
    交互项_OR        = exp(ct$beta[idx]),
    交互项_t         = ct$tstat[idx],
    交互项_df        = ct$df_Satt[idx],
    交互项_CR2_p     = ct$p_Satt[idx]
  )
}

grid <- expand.grid(调域 = c("A", "B"), var = all_vars,
                    stringsAsFactors = FALSE)
results_single <- pmap_dfr(list(grid$调域, grid$var), test_single)

# ---------- 5. 控制台摘要 ----------
cat("\n===== 整体交互检验 =====\n")
print(results_overall, digits = 4)

cat("\n===== 单独交互检验 =====\n")
print(results_single %>%
        select(调域, 交互项, Df变化, LRT_p, 交互项_CR2_p) %>%
        mutate(across(c(LRT_p, 交互项_CR2_p), ~ round(.x, 4))),
      n = Inf)

# ---------- 6. 写入 Excel ----------
round_df <- function(df, cols, digits = 4) {
  df %>% mutate(across(all_of(cols), ~ round(.x, digits)))
}

write_xlsx(
  list(
    "整体交互检验" = results_overall %>%
      round_df(c("Df变化", "Deviance变化", "LRT_p",
                 "CR2_Wald_F", "CR2_Wald_df1", "CR2_Wald_df2", "CR2_Wald_p")),
    
    "单独交互检验" = results_single %>%
      round_df(c("Df变化", "Deviance变化", "LRT_p",
                 "交互项_estimate", "交互项_SE", "交互项_OR",
                 "交互项_t", "交互项_df", "交互项_CR2_p")),
    
    "主效应模型" = results_main %>%
      round_df(c("estimate", "std.error", "df", "t", "p",
                 "OR", "OR_low", "OR_high")),
    
    "判断汇总" = results_single %>%
      select(调域, 交互项, LRT_p, 交互项_CR2_p) %>%
      mutate(
        LRT显著    = ifelse(LRT_p < 0.05, "是", "否"),
        CR2显著    = ifelse(交互项_CR2_p < 0.05, "是", "否"),
        是否纳入   = ifelse(LRT_p < 0.05 | 交互项_CR2_p < 0.05,
                        "纳入（需说明）", "不纳入")
      )
  ),
  path = "交互检验结果.xlsx"
)

cat("\n已保存：", normalizePath("交互检验结果.xlsx"), "\n")