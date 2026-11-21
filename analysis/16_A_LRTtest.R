#先测试了A类调模型的随机效应，确定(1 + env || 姓名) + (1 | 意义)两个随机项
#然后测试在包含所有主效应的模型的基础上，是否需要加交互效应

library(lme4)

# 你的 A 调域当前基础模型（只含主效应）
m_A_base <- glmer(
  y ~ env + 调类 + 性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 + env || 姓名) + (1 | 意义),
  data = df_A, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# ---- 测试 1：核心语音机制交互 (env * 调类) ----
m_A_env_tone <- glmer(
  y ~ env * 调类 + 性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 + env || 姓名) + (1 | 意义),
  data = df_A, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
cat("\n--- env * 调类 LRT 检验 ---\n")
print(anova(m_A_base, m_A_env_tone, test = "Chisq"))


# ---- 测试 2：环境与社会变量的交互 (env * 社会变量) ----
m_A_env_social <- glmer(
  y ~ env + 调类 + env * 性别 + env * 年龄_z + env * 教育程度3 + env * 摩梭话_z + 
    (1 + env || 姓名) + (1 | 意义),
  data = df_A, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
cat("\n--- env * 社会变量 LRT 检验 ---\n")
print(anova(m_A_base, m_A_env_social, test = "Chisq"))


# ---- 测试 3：调类与社会变量的交互 (调类 * 社会变量) ----
m_A_tone_social <- glmer(
  y ~ env + 调类 + 调类 * 性别 + 调类 * 年龄_z + 调类 * 教育程度3 + 调类 * 摩梭话_z + 
    (1 + env || 姓名) + (1 | 意义),
  data = df_A, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
cat("\n--- 调类 * 社会变量 LRT 检验 ---\n")
print(anova(m_A_base, m_A_tone_social, test = "Chisq"))