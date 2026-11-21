#确定了随机效应、固定效应后，拆解交互效应
library(emmeans)

# 1. 看性别在 B1 和 B2 中分别是什么情况
emm_sex <- emmeans(m_test_interact_B, ~ 性别 | 调类)
contrast(emm_sex, method = "revpairwise", adjust = "bonferroni")

# 2. 看摩梭话和年龄的趋势在 B1 和 B2 中分别是什么情况
emtrends(m_test_interact_B, ~ 调类, var = "摩梭话_z")
emtrends(m_test_interact_B, ~ 调类, var = "年龄_z")

# 3. 看教育程度在 B1 和 B2 中的差异（跟“无”比）
emm_edu <- emmeans(m_test_interact_B, ~ 教育程度3 | 调类)
contrast(emm_edu, method = "trt.vs.ctrl", ref = 1) 