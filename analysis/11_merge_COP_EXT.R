# ---------- 构造长格式数据 ----------
# 每个 (姓名, 调类) 产生 2 行：COP 和 EXT
long_df <- raw_df %>%
  filter(调类 %in% c("A1","A2","B1","B2")) %>%
  select(姓名, 调类, y_A_COP, y_A_EXT, y_B_COP, y_B_EXT) %>%
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
  )

# 合并人口学变量
long_df <- long_df %>%
  left_join(demo_df, by = "姓名") %>%
  filter(!is.na(性别), !is.na(年龄_z),
         !is.na(教育程度2), !is.na(摩梭话_z))

# 检查
cat("长格式记录数：", nrow(long_df), "\n")
cat("按调域 × 环境的事件数：\n")
print(long_df %>% group_by(调域, env) %>%
        summarise(记录数 = n(), 事件数 = sum(y), .groups = "drop"))

m_A <- glm(
  y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = long_df %>% filter(调域 == "A"),
  family = binomial
)

# CR2 聚类
ct_A <- clubSandwich::coef_test(
  m_A, vcov = "CR2",
  cluster = long_df %>% filter(调域 == "A") %>% pull(姓名),
  test = "Satterthwaite"
)

m_B <- glm(
  y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = long_df %>% filter(调域 == "B"),
  family = binomial
)

m_A_interact <- glm(
  y ~ env * (性别 + 年龄_z + 教育程度2 + 摩梭话_z),
  data = long_df %>% filter(调域 == "A"),
  family = binomial
)

# 用 LRT 检验交互项整体是否显著
m_A_no_interact <- glm(
  y ~ env + 性别 + 年龄_z + 教育程度2 + 摩梭话_z,
  data = long_df %>% filter(调域 == "A"),
  family = binomial
)

anova(m_A_no_interact, m_A_interact, test = "Chisq")