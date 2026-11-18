# ============================================================
# 独立运行：A/B 调域分别向前选择 + 导出 Excel
# ============================================================

# 1. 加载所需包
library(dplyr)
library(tidyr)
library(lme4)
library(writexl)

# ============================================================
# 2. 准备长格式数据 (如果你确认当前的 long_df 已经清洗好，这步会自动覆盖)
# ============================================================
# 假设你的 raw_df 和 demo_df 已经在环境中存在
long_df <- raw_df %>%
  filter(调类 %in% c("A1","A2","B1","B2")) %>%
  select(姓名, 调类, 意义, y_A_COP, y_A_EXT, y_B_COP, y_B_EXT) %>%
  pivot_longer(
    cols = c(y_A_COP, y_A_EXT, y_B_COP, y_B_EXT),
    names_to = c("调域", "env"),
    names_pattern = "y_([AB])_(COP|EXT)",
    values_to = "y"
  ) %>%
  filter(!is.na(y)) %>%
  mutate(
    env   = factor(env, levels = c("COP", "EXT")),
    调域  = factor(调域, levels = c("A", "B"))
  ) %>%
  left_join(demo_df, by = "姓名") %>%
  filter(!is.na(性别), !is.na(年龄_z),
         !is.na(教育程度3), !is.na(摩梭话_z))

# 拆分 A 和 B 数据，并清理因子水平
df_A <- long_df %>% 
  filter(调域 == "A") %>% 
  mutate(调类 = droplevels(factor(调类)), 意义 = factor(意义), 姓名 = factor(姓名))

df_B <- long_df %>% 
  filter(调域 == "B") %>% 
  mutate(调类 = droplevels(factor(调类)), 意义 = factor(意义), 姓名 = factor(姓名))

# ============================================================
# 3. 自定义向前选择函数（带边际性原则保护）
# ============================================================
forward_select_glmer <- function(base_formula, candidates, data) {
  # 初始模型
  current_formula <- base_formula
  current_model <- glmer(as.formula(current_formula), data = data, family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  current_aic <- AIC(current_model)
  
  history <- data.frame(Step = 0, Added = "None", AIC = current_aic, Formula = current_formula)
  step <- 1
  
  while (length(candidates) > 0) {
    aic_list <- list()
    model_list <- list()
    added_terms_list <- list()
    
    for (cand in candidates) {
      # ★ 核心修复：检查边际性原则，交互项必须连同主效应一起加入
      terms_to_add <- cand
      if (grepl(":", cand)) {
        parts <- strsplit(cand, ":")[[1]]
        missing_parts <- parts[!sapply(parts, function(p) grepl(p, current_formula, fixed = TRUE))]
        if (length(missing_parts) > 0) {
          terms_to_add <- paste(c(missing_parts, cand), collapse = " + ")
        }
      }
      
      test_formula <- paste(current_formula, "+", terms_to_add)
      
      tryCatch({
        m <- glmer(as.formula(test_formula), data = data, family = binomial,
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
        aic_list[[cand]] <- AIC(m)
        model_list[[cand]] <- m
        added_terms_list[[cand]] <- terms_to_add
      }, error = function(e) {
        aic_list[[cand]] <- Inf 
      }, warning = function(w) {
        aic_list[[cand]] <- Inf 
      })
    }
    
    if (length(aic_list) == 0) break
    
    best_cand <- names(which.min(unlist(aic_list)))
    best_aic <- min(unlist(aic_list))
    
    if (is.infinite(best_aic) || best_aic >= current_aic - 2) {
      break
    }
    
    actual_added <- added_terms_list[[best_cand]]
    current_formula <- paste(current_formula, "+", actual_added)
    current_aic <- best_aic
    current_model <- model_list[[best_cand]]
    
    history <- rbind(history, data.frame(Step = step, Added = actual_added, AIC = current_aic, Formula = current_formula))
    
    candidates <- setdiff(candidates, best_cand)
    if (grepl(":", best_cand)) {
      parts <- strsplit(best_cand, ":")[[1]]
      candidates <- setdiff(candidates, parts)
    }
    step <- step + 1
  }
  return(list(final_model = current_model, history = history, final_formula = current_formula))
}

# 设置候选变量
base_formula <- "y ~ (1 | 姓名) + (1 | 意义)"
main_effects <- c("env", "调类", "性别", "年龄_z", "教育程度3", "摩梭话_z")
interactions <- combn(main_effects, 2, FUN = function(x) paste(x, collapse = ":"), simplify = TRUE)
candidates <- c(main_effects, interactions)

# ============================================================
# 4. 跑 A 和 B 的模型
# ============================================================
cat("\n========== 正在计算 A 类模型 ==========\n")
result_A <- forward_select_glmer(base_formula, candidates, df_A)

cat("\n========== 正在计算 B 类模型 ==========\n")
result_B <- forward_select_glmer(base_formula, candidates, df_B)

# ============================================================
# 5. 提取结果并导出 Excel
# ============================================================
# 定义一个提取系数并计算 OR 的辅助函数
extract_coefs <- function(model_obj) {
  if (is.null(model_obj)) return(data.frame())
  
  # summary 获取的是矩阵，矩阵的列名包含空格（如 "Std. Error"）
  co <- summary(model_obj)$coefficients
  
  # 直接通过矩阵的列名提取数值，绝对不会有列名丢失或解析错误
  est <- co[, "Estimate"]
  se  <- co[, "Std. Error"]
  z   <- co[, "z value"]
  p   <- co[, "Pr(>|z|)"]
  
  # 直接用 data.frame 组合并重命名
  df <- data.frame(
    "变量 (Term)"      = rownames(co),
    "系数 (Log-odds)"  = est,
    "标准误 (SE)"      = se,
    "z值 (z-value)"    = z,
    "p值 (p-value)"    = p,
    "几率比 (OR)"      = exp(est),
    "OR_95%_下限"      = exp(est - 1.96 * se),
    "OR_95%_上限"      = exp(est + 1.96 * se),
    check.names = FALSE   # 防止 R 自动把中文或特殊符号变成点
  )
  
  return(df)
}

# 提取各自的结果
df_A_coefs <- extract_coefs(result_A$final_model)
df_B_coefs <- extract_coefs(result_B$final_model)

# 提取选择历史
df_A_history <- result_A$history
df_B_history <- result_B$history

# 写入 Excel
out_path <- "C:/Luguhu_tones/data/AB_domain_glmer_results_edu3.xlsx"

write_xlsx(
  list(
    "A模型_系数与OR" = df_A_coefs,
    "A模型_选择历史" = df_A_history,
    "B模型_系数与OR" = df_B_coefs,
    "B模型_选择历史" = df_B_history
  ),
  path = out_path
)

cat("\n✅ 建模完成！结果已保存至：", normalizePath(out_path), "\n")