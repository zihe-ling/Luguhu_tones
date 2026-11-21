library(lme4)
library(dplyr)
library(purrr)

# 1. 统一的固定效应底座
fixed_formula <- "y ~ env * 调类 + 性别 + 年龄_z + 教育程度3 + 摩梭话_z"

# 2. 列出所有需要测试的随机效应组合 (被试验、词项)
re_combinations <- list(
  list(name = "Max_Cov",      form = "(1 + env * 调类 | 姓名) + (1 + env * 调类 | 意义)"),
  list(name = "Max_NoCov",    form = "(1 + env * 调类 || 姓名) + (1 + env * 调类 || 意义)"),
  list(name = "Subj_Max_Itm_Env", form = "(1 + env * 调类 || 姓名) + (1 + env || 意义)"),
  list(name = "Subj_Env_Itm_Max", form = "(1 + env || 姓名) + (1 + env * 调类 || 意义)"),
  list(name = "Subj_Tone_Itm_Env",form = "(1 + 调类 || 姓名) + (1 + env || 意义)"),
  list(name = "Subj_Env_Itm_Tone",form = "(1 + env || 姓名) + (1 + 调类 || 意义)"),
  list(name = "Subj_Both_Itm_Int",form = "(1 + env + 调类 || 姓名) + (1 | 意义)"),
  list(name = "Final_Current",form = "(1 + env || 姓名) + (1 | 意义)"),
  list(name = "Simple_RI",    form = "(1 | 姓名) + (1 | 意义)")
)

# 3. 定义一个安全的拟合函数（防止报错中断程序）
fit_safe <- possibly(function(form_str, data) {
  f <- as.formula(paste(fixed_formula, "+", form_str))
  mod <- glmer(f, data = data, family = binomial, 
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  
  # 提取指标
  tibble(
    随机效应结构 = form_str,
    是否收敛 = ifelse(is.null(mod@optinfo$conv$lme4$messages), "是", "否(警告)"),
    是否奇异拟合 = isSingular(mod),
    AIC = AIC(mod),
    BIC = BIC(mod)
  )
}, otherwise = NULL)

# 4. 循环测试所有的组合
cat("正在穷尽测试随机效应组合，请稍候...\n\n")
results_list <- map(re_combinations, ~ fit_safe(.x$form, df_A))
results_df <- bind_rows(results_list)

# 5. 输出终极对比表
print(as.data.frame(results_df))