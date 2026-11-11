library(dplyr)

# 从原表 df 中提取每个人的性别、出生年、教育程度、摩梭话比重
demo_df <- df %>%
  group_by(姓名) %>%
  summarise(
    性别       = first(na.omit(性别)),
    出生年     = first(na.omit(出生年)),
    教育程度   = first(na.omit(教育程度)),
    摩梭话比重 = first(na.omit(摩梭话比重)),
    .groups = "drop"
  ) %>%
  mutate(
    年龄 = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年),
    # M/F 转成因子，顺序无所谓，因子化即可
    性别     = factor(性别, levels = c("M", "F")),
    教育程度 = factor(教育程度)
  )

# 合并到 summary_df
summary_df <- summary_df %>%
  left_join(demo_df, by = "姓名")

# 检查一下列名
names(summary_df)
str(summary_df)

library(car)

m_check <- lm(`A调类COP` ~ 性别 + 年龄 + 教育程度 + 摩梭话比重,
              data = summary_df)
vif(m_check)