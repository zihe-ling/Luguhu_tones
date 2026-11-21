#先确定了随机效应加哪些
#然后测试在包含所有主效应的模型的基础上，是否需要加交互效应

# 1. 纯截距健康模型底座
healthy_base <- glmer(
  y ~ env * 调类 + 性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 | 姓名) + (1 | 意义), 
  data = df_B, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# 2. 测试社会变量与调类的交互
m_test_interact_B <- glmer(
  y ~ env * 调类 + 
    调类 * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 | 姓名) + (1 | 意义),
  data = df_B, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# 3. 严谨性全模型（加入 env × 社会变量）
m_full_strict_B <- glmer(
  y ~ env * 调类 + 
    调类 * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    env * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 | 姓名) + (1 | 意义),
  data = df_B, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# 4. 运行似然比检验（LRT）
anova(healthy_base, m_test_interact_B, test = "Chisq")
anova(healthy_base, m_full_strict_B, test = "Chisq")
anova(m_test_interact_B, m_full_strict_B, test = "Chisq")