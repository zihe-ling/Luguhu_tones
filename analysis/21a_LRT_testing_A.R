#先确定了随机效应加哪些
#然后测试在包含所有主效应的模型的基础上，是否需要加交互效应

# 定义优化器控制，防报错
ctrl_A <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# 1. 新的基础模型（带最大随机效应，包含 env*调类 固定效应，以及社会主效应）
m_A_healthy <- glmer(
  y ~ env * 调类 + 性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 + env * 调类 | 姓名) + (1 + env * 调类 | 意义),
  data = df_A, family = binomial,
  control = ctrl_A
)

# 2. 测试：是否保留“调类 × 社会变量”交互？
m_A_tone_social <- glmer(
  y ~ env * 调类 + 调类 * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    (1 + env * 调类 | 姓名) + (1 + env * 调类 | 意义),
  data = df_A, family = binomial,
  control = ctrl_A
)

# 3. 测试：是否保留“环境 × 社会变量”交互？
m_A_env_social <- glmer(
  y ~ env * 调类 + env * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    (1 + env * 调类 | 姓名) + (1 + env * 调类 | 意义),
  data = df_A, family = binomial,
  control = ctrl_A
)

# 4. 全模型，调类 × 社会变量 + 环境 × 社会变量
m_A_full <- glmer(
  y ~ env * 调类 + 
    调类 * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    env * (性别 + 年龄_z + 教育程度3 + 摩梭话_z) + 
    性别 + 年龄_z + 教育程度3 + 摩梭话_z + 
    (1 + env * 调类 | 姓名) + (1 + env * 调类 | 意义),
  data = df_A, family = binomial,
  control = ctrl_A
)

# 5. LRT 检验
anova(m_A_healthy, m_A_tone_social, test = "Chisq")
anova(m_A_healthy, m_A_env_social, test = "Chisq")
anova(m_A_healthy, m_A_full, test = "Chisq")
anova(m_A_tone_social, m_A_env_social, test = "Chisq")
anova(m_A_tone_social, m_A_full, test = "Chisq")
anova(m_A_env_social, m_A_full, test = "Chisq")