library(lme4)
library(dplyr)
library(emmeans)    
library(writexl)

# ============================================================
# 1. 准备建模数据
# ============================================================
# 确保没有缺失值，并转为因子
df_model <- long_df %>%
  filter(!is.na(意义), !is.na(y)) %>%
  mutate(
    意义 = factor(意义),
    姓名 = factor(姓名),
    调类 = factor(调类)
  )

# 基础模型：只含两个随机效应
base_formula <- "y ~ (1 | 姓名) + (1 | 意义)"

# 候选主效应
main_effects <- c("env", "调类", "性别", "年龄_z", "教育程度2", "摩梭话_z")

# 生成所有二阶交互项 (如 "env:调类")
interactions <- combn(main_effects, 2, FUN = function(x) paste(x, collapse = ":"), simplify = TRUE)
candidates <- c(main_effects, interactions)

# ============================================================
# 2. 自定义向前选择函数 (基于 AIC)
# ============================================================
forward_select_glmer <- function(base_formula, candidates, data) {
  # 初始模型
  current_formula <- base_formula
  current_model <- glmer(as.formula(current_formula), data = data, family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  current_aic <- AIC(current_model)
  
  cat("【初始模型】", current_formula, " AIC =", round(current_aic, 2), "\n")
  
  history <- data.frame(Step = 0, Added = "None", AIC = current_aic, Formula = current_formula)
  step <- 1
  
  while (length(candidates) > 0) {
    aic_list <- list()
    model_list <- list()
    added_terms_list <- list() # 记录为了维持层次原则，实际加入了哪些项
    
    # 遍历剩下的所有候选变量
    for (cand in candidates) {
      
      # ★ 核心修复：检查边际性原则
      terms_to_add <- cand
      if (grepl(":", cand)) {
        # 如果是交互项，拆分出主效应
        parts <- strsplit(cand, ":")[[1]]
        # 找出当前公式里缺失的主效应
        missing_parts <- parts[!sapply(parts, function(p) grepl(p, current_formula, fixed = TRUE))]
        if (length(missing_parts) > 0) {
          # 如果主效应缺失，必须把缺失的主效应和交互项一起加入
          terms_to_add <- paste(c(missing_parts, cand), collapse = " + ")
        }
      }
      
      test_formula <- paste(current_formula, "+", terms_to_add)
      
      # 使用 tryCatch 防止模型不收敛导致程序中断
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
    
    # 停止条件：如果 AIC 下降不到 2，则停止纳入
    if (is.infinite(best_aic) || best_aic >= current_aic - 2) {
      cat("【停止】加入", best_cand, "后 AIC 未显著下降。\n")
      break
    }
    
    # 更新模型
    actual_added <- added_terms_list[[best_cand]]
    current_formula <- paste(current_formula, "+", actual_added)
    current_aic <- best_aic
    current_model <- model_list[[best_cand]]
    
    if (actual_added != best_cand) {
      cat("【Step", step, "】加入交互项:", best_cand, " (同时强制补齐主效应:", actual_added, ") | 当前 AIC =", round(current_aic, 2), "\n")
    } else {
      cat("【Step", step, "】加入变量:", best_cand, " | 当前 AIC =", round(current_aic, 2), "\n")
    }
    
    history <- rbind(history, data.frame(Step = step, Added = actual_added, AIC = current_aic, Formula = current_formula))
    
    # 从候选列表中移除：既移除刚加入的候选变量本身，也移除可能被强制补齐的主效应
    candidates <- setdiff(candidates, best_cand)
    if (grepl(":", best_cand)) {
      parts <- strsplit(best_cand, ":")[[1]]
      candidates <- setdiff(candidates, parts)
    }
    
    step <- step + 1
  }
  
  return(list(final_model = current_model, history = history, final_formula = current_formula))
}

# ============================================================
# 3. 运行向前选择
# ============================================================
cat("\n========== 开始向前选择 ==========\n")
result <- forward_select_glmer(base_formula, candidates, df_model)

cat("\n========== 最终结果 ==========\n")
print(result$history, row.names = FALSE)
cat("\n最终模型公式：", result$final_formula, "\n")

# 查看最终模型摘要
# if (!is.null(result$final_model)) {
#  print(summary(result$final_model))
#}

# ============================================================
# 4. 事后对比 (emmeans) & 导出 Excel
# ============================================================

# 先确认最终模型里到底有没有"调类"这个变量（万一向前选择把它踢了）
if ("调类" %in% attr(terms(result$final_model), "term.labels")) {
  
  # ---- 4.1 计算边际均值 (EMMs) ----
  # 这里用 | env，意思是分别在 COP 和 EXT 环境下计算各类的估计值
  # 如果你的最终模型没有 env 或者你不在乎环境，可以把 | env 去掉
  emm <- emmeans(result$final_model, ~ 调类 | env)
  
  cat("\n========== 估计的边际均值 (EMMs) ==========\n")
  print(as.data.frame(emm))
  
  # ---- 4.2 提取指定的事后对比 (Contrasts) ----
  # 定义我们要比较的组别
  # 注意：这里的顺序必须和 emm 里 levels(调类) 的顺序一致，即 A1, A2, B1, B2
  contrast_list <- list(
    "A2 vs A1" = c(-1,  1,  0,  0),
    "B2 vs B1" = c( 0,  0, -1,  1)
  )
  
  # 计算对比（默认在对数几率 logit 尺度上）
  cont_res <- contrast(emm, method = contrast_list)
  
  cat("\n========== 事后对比 (Log-Odds) ==========\n")
  print(as.data.frame(cont_res))
  
  # ---- 4.3 转换为几率比 (Odds Ratio, OR) ----
  # 使用 summary(..., type = "response") 可以将 logit 尺度转换为 OR
  cont_or <- summary(cont_res, type = "response", infer = c(TRUE, TRUE))
  
  cat("\n========== 事后对比 (Odds Ratio) ==========\n")
  print(as.data.frame(cont_or))
  
  # ---- 4.4 导出为 Excel ----
  # 整理为干净的数据框
  df_emm <- as.data.frame(emm)
  df_contrast <- as.data.frame(cont_or)
  
  # 写入 Excel
  write_xlsx(
    list(
      "向前选择历史" = result$history,
      "EMMs_均值"    = df_emm,
      "对比_OR值"    = df_contrast
    ),
    path = "C:/Luguhu_tones/data/emmeans_results.xlsx"
  )
  
  cat("\n已保存对比结果到：C:/Luguhu_tones/data/emmeans_results.xlsx\n")
  
} else {
  message("⚠️ 警告：最终模型中不包含'调类'，无法进行事后对比。")
}