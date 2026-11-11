# 安装所需包（如未安装，取消注释后运行）
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "car"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(purrr)
library(tibble)

# ---------- 1. 读入 Excel 文件 ----------
file_path <- "Luguhu_tone_variation_261110.xlsx"
df <- read_excel(file_path)

# 如列名不是“姓名、调类、COP、EXT、性别、出生年、教育程度、摩梭话比重”，请重命名：
# df <- df %>% rename(姓名 = name, 调类 = tone, COP = cop, EXT = ext)

# ---------- 2. 初始化混类列 ----------
df <- df %>%
  mutate(
    COP_A1toA2 = 0, COP_A2toA1 = 0,
    COP_B1toB2 = 0, COP_B2toB1 = 0,
    EXT_A1toA2 = 0, EXT_A2toA1 = 0,
    EXT_B1toB2 = 0, EXT_B2toB1 = 0,
    COP_AtoB   = 0, COP_BtoA   = 0,
    EXT_AtoB   = 0, EXT_BtoA   = 0,
    EXT_iso    = 0
  )

# ---------- 3. 逐条检查并赋值 ----------
df <- df %>%
  mutate(
    # ---- COP 相邻调类之间的混类 ----
    COP_A1toA2 = ifelse(调类 == "A1" & !is.na(COP) & grepl("HR", COP), 1, COP_A1toA2),
    COP_A2toA1 = ifelse(调类 == "A2" & !is.na(COP) & grepl("HF", COP), 1, COP_A2toA1),
    COP_B1toB2 = ifelse(调类 == "B1" & !is.na(COP) & grepl("LF", COP), 1, COP_B1toB2),
    COP_B2toB1 = ifelse(调类 == "B2" & !is.na(COP) & grepl("LR", COP), 1, COP_B2toB1),
    
    # ---- EXT 相邻调类之间的混类 ----
    EXT_A1toA2 = ifelse(调类 == "A1" & !is.na(EXT) & grepl("HLR", EXT), 1, EXT_A1toA2),
    EXT_A2toA1 = ifelse(调类 == "A2" & !is.na(EXT) & grepl("HHF", EXT), 1, EXT_A2toA1),
    EXT_B1toB2 = ifelse(调类 == "B1" & !is.na(EXT) & grepl("LHF", EXT), 1, EXT_B1toB2),
    EXT_B2toB1 = ifelse(调类 == "B2" & !is.na(EXT) & grepl("LLH", EXT), 1, EXT_B2toB1),
    
    # ---- A 调类 ↔ B 调类之间的混类（跨调域）----
    # A 调类（A1/A2）中出现以 L 开头的 COP → 向 B 漂移
    COP_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(COP) & grepl("^L", COP), 1, COP_AtoB),
    # B 调类（B1/B2）中出现以 H 开头的 COP → 向 A 漂移
    COP_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(COP) & grepl("^H", COP), 1, COP_BtoA),
    # A 调类中出现以 L 开头的 EXT → 向 B 漂移
    EXT_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(EXT) & grepl("^L", EXT), 1, EXT_AtoB),
    # B 调类中出现以 H 开头的 EXT → 向 A 漂移
    EXT_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(EXT) & grepl("^H", EXT), 1, EXT_BtoA),
    
    # ---- EXT 起首 R + 两个大写字母（如 RHL、RLL）----
    EXT_iso = ifelse(!is.na(EXT) & grepl("^R[A-Z]{2}", EXT), 1, EXT_iso)
  )

# ---------- 4. 按姓名分组计算 12 个混类值 ----------
# 辅助函数：当某个子集为空时返回 NA，避免 mean(numeric(0)) 得到 NaN
safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

summary_df <- df %>%
  group_by(姓名) %>%
  summarise(
    # ---- 4 个 COP 相邻调类混类值 ----
    A1_COP = safe_mean(COP_A1toA2[调类 == "A1"]),
    A2_COP = safe_mean(COP_A2toA1[调类 == "A2"]),
    B1_COP = safe_mean(COP_B1toB2[调类 == "B1"]),
    B2_COP = safe_mean(COP_B2toB1[调类 == "B2"]),
    # ---- 4 个 EXT 相邻调类混类值 ----
    A1_EXT = safe_mean(EXT_A1toA2[调类 == "A1"]),
    A2_EXT = safe_mean(EXT_A2toA1[调类 == "A2"]),
    B1_EXT = safe_mean(EXT_B1toB2[调类 == "B1"]),
    B2_EXT = safe_mean(EXT_B2toB1[调类 == "B2"]),
    # ---- 2 个 COP 跨调域混类值 ----
    AB_COP = safe_mean(COP_AtoB[调类 %in% c("A1","A2")]),
    BA_COP = safe_mean(COP_BtoA[调类 %in% c("B1","B2")]),
    # ---- 2 个 EXT 跨调域混类值 ----
    AB_EXT = safe_mean(EXT_AtoB[调类 %in% c("A1","A2")]),
    BA_EXT = safe_mean(EXT_BtoA[调类 %in% c("B1","B2")]),
    .groups = "drop"
  )

# ---------- 5. 合并人口学变量 ----------
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
    年龄     = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年),
    性别     = factor(性别),
    教育程度 = factor(教育程度)
  )

summary_df <- summary_df %>%
  mutate(姓名 = trimws(姓名)) %>%
  left_join(demo_df %>% mutate(姓名 = trimws(姓名)), by = "姓名")

print(summary_df)

# ——————————————————————混类统计——————————————————————
# 8 个“调类-环境”组合的规则
# 本调类：默认值；相邻调类：该环境下的相邻调类混类；其他调：跨调域或 R 开头
stat_specs <- tribble(
  ~调类, ~环境, ~本调类, ~相邻调类, ~判断列,      ~跨调列,
  "A1",  "COP", "A1",   "A2",     "COP_A1toA2", "COP_AtoB",
  "A2",  "COP", "A2",   "A1",     "COP_A2toA1", "COP_AtoB",
  "B1",  "COP", "B1",   "B2",     "COP_B1toB2", "COP_BtoA",
  "B2",  "COP", "B2",   "B1",     "COP_B2toB1", "COP_BtoA",
  "A1",  "EXT", "A1",   "A2",     "EXT_A1toA2", "EXT_AtoB",
  "A2",  "EXT", "A2",   "A1",     "EXT_A2toA1", "EXT_AtoB",
  "B1",  "EXT", "B1",   "B2",     "EXT_B1toB2", "EXT_BtoA",
  "B2",  "EXT", "B2",   "B1",     "EXT_B2toB1", "EXT_BtoA"
)
calc_stat <- function(data, tone, env, 本调类, 相邻调类, 判断列, 跨调列) {
  raw_col <- if (env == "COP") "COP" else "EXT"
  
  # 只取该调类、且原始 COP/EXT 非 NA 的记录
  sub <- data %>%
    filter(调类 == tone, !is.na(.data[[raw_col]]))
  
  n_total <- nrow(sub)
  if (n_total == 0) {
    return(tibble(
      组合 = paste0(tone, "-", env),
      本调类 = 本调类, 相邻调类 = 相邻调类,
      总记录数 = 0L,
      本调类比例 = NA_real_, 相邻调类比例 = NA_real_, 其他调比例 = NA_real_
    ))
  }
  
  # 判断每条记录的结果
  sub <- sub %>%
    mutate(
      结果 = case_when(
        .data[[跨调列]] == 1 ~ "其他调",
        .data[[判断列]] == 1 ~ 相邻调类,
        TRUE                 ~ 本调类
      )
    )
  
  # EXT 环境下，EXT_iso（R 开头三个大写字母）归为“其他调”
  if (env == "EXT" && "EXT_iso" %in% names(sub)) {
    sub <- sub %>%
      mutate(结果 = ifelse(EXT_iso == 1, "其他调", 结果))
  }
  
  tibble(
    组合         = paste0(tone, "-", env),
    本调类       = 本调类,
    相邻调类     = 相邻调类,
    总记录数     = n_total,
    本调类比例   = round(mean(sub$结果 == 本调类),   3),
    相邻调类比例 = round(mean(sub$结果 == 相邻调类), 3),
    其他调比例   = round(mean(sub$结果 == "其他调"), 3)
  )
}
# 所有人
stat_overall <- pmap_dfr(
  list(stat_specs$调类, stat_specs$环境, stat_specs$本调类,
       stat_specs$相邻调类, stat_specs$判断列, stat_specs$跨调列),
  function(tone, env, ben, lin, judge, cross) {
    calc_stat(df, tone, env, ben, lin, judge, cross)
  }
)

cat("\n===== 总体统计（所有人） =====\n")
print(as.data.frame(stat_overall), row.names = FALSE)

# 画图，宽表转长表，便于堆叠
plot_overall <- stat_overall %>%
  select(组合, 本调类比例, 相邻调类比例, 其他调比例) %>%
  pivot_longer(
    cols = c(本调类比例, 相邻调类比例, 其他调比例),
    names_to = "结果类型",
    values_to = "比例"
  ) %>%
  mutate(
    结果类型 = factor(结果类型,
                  levels = c("本调类比例", "相邻调类比例", "其他调比例"),
                  labels = c("本调类", "相邻调类", "其他调")),
    组合 = factor(组合, levels = stat_overall$组合)
  )

p_bar_overall <- ggplot(plot_overall, aes(x = 组合, y = 比例, fill = 结果类型)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  geom_text(
    aes(label = ifelse(比例 >= 0.05, sprintf("%.2f", 比例), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_fill_manual(values = c("本调类" = "#4C72B0",
                               "相邻调类" = "#DD8452",
                               "其他调" = "#55A868")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = "比例", fill = "读音类型",
       title = "各调类-环境组合的读音分布（总体）") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_bar_overall)

#分性别
stat_by_gender <- df %>%
  group_by(性别) %>%
  group_modify(~ {
    d <- .x
    pmap_dfr(
      list(stat_specs$调类, stat_specs$环境, stat_specs$本调类,
           stat_specs$相邻调类, stat_specs$判断列, stat_specs$跨调列),
      function(tone, env, ben, lin, judge, cross) {
        calc_stat(d, tone, env, ben, lin, judge, cross)
      }
    )
  }) %>%
  ungroup() %>%
  relocate(性别, .before = 1)

cat("\n===== 分性别统计 =====\n")
print(as.data.frame(stat_by_gender), row.names = FALSE)
# 画图
plot_gender <- stat_by_gender %>%
  select(性别, 组合, 本调类比例, 相邻调类比例, 其他调比例) %>%
  pivot_longer(
    cols = c(本调类比例, 相邻调类比例, 其他调比例),
    names_to = "结果类型",
    values_to = "比例"
  ) %>%
  mutate(
    结果类型 = factor(结果类型,
                  levels = c("本调类比例", "相邻调类比例", "其他调比例"),
                  labels = c("本调类", "相邻调类", "其他调")),
    组合 = factor(组合, levels = stat_overall$组合),
    # 性别标签，按实际编码改（M/F 或 男/女）
    性别 = factor(性别, levels = c("F", "M"),
                labels = c("女性", "男性"))
  )

p_bar_gender <- ggplot(plot_gender, aes(x = 组合, y = 比例, fill = 结果类型)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  geom_text(
    aes(label = ifelse(比例 >= 0.05, sprintf("%.2f", 比例), "")),
    position = position_stack(vjust = 0.5),
    size = 2.5, color = "black"
  ) +
  scale_fill_manual(values = c("本调类" = "#4C72B0",
                               "相邻调类" = "#DD8452",
                               "其他调" = "#55A868")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_wrap(~ 性别, ncol = 2) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "比例", fill = "读音类型",
       title = "各调类-环境组合的读音分布（分性别）") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
        strip.text = element_text(size = 11, face = "bold"))

print(p_bar_gender)

# ---------- 6. 画热力图 ----------
# 12 个混类值的顺序
mix_levels <- c(
  "A1_COP","A2_COP","B1_COP","B2_COP",
  "A1_EXT","A2_EXT","B1_EXT","B2_EXT",
  "AB_COP","BA_COP","AB_EXT","BA_EXT"
)

# 排序：女性在前、男性在后，再按姓名
summary_df <- summary_df %>%
  mutate(性别 = factor(性别, levels = c("F", "M"))) %>%
  arrange(性别, 姓名)

# 宽表转长表
plot_df <- summary_df %>%
  select(姓名, 性别, all_of(mix_levels)) %>%
  pivot_longer(cols = all_of(mix_levels),
               names_to = "混类类型", values_to = "混类值") %>%
  mutate(
    混类类型 = factor(混类类型, levels = mix_levels),
    # y 轴姓名按 summary_df 的顺序（女性在前），用 factor 固定
    姓名 = factor(姓名, levels = rev(unique(summary_df$姓名)))
  )

# 为 y 轴姓名标签准备性别颜色向量（顺序需与 levels(姓名) 一致）
name_levels <- levels(plot_df$姓名)
name_color_vec <- setNames(
  ifelse(summary_df$性别 == "F", "red", "blue"),
  summary_df$姓名
)
y_label_colors <- name_color_vec[name_levels]

# 绘制热力图
p <- ggplot(plot_df, aes(x = 混类类型, y = 姓名, fill = 混类值)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(混类值), "", sprintf("%.2f", 混类值))),
    color = "black", size = 2.5
  ) +
  scale_fill_gradient(
    low = "white", high = "darkred",
    name = "混类值", na.value = "grey90"
  ) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8, color = y_label_colors)
  )

print(p)

# 如需保存图片：
# ggsave("tone_confusion_heatmap_12cols.png", p,
#        width = 10, height = max(5, nrow(summary_df) * 0.3))

# ---------- 7. 自变量共线性检验（VIF） ----------
# ---------- 确认 demo_df 干净 ----------
cat("demo_df 人数：", nrow(demo_df), "\n")
cat("教育程度 NA 数：", sum(is.na(demo_df$教育程度)), "\n")
cat("性别 NA 数：",     sum(is.na(demo_df$性别)), "\n")
cat("年龄 NA 数：",     sum(is.na(demo_df$年龄)), "\n")
cat("摩梭话比重 NA 数：", sum(is.na(demo_df$摩梭话比重)), "\n")

# ---------- 用任意一个"占位"因变量建 lm ----------
# VIF 与因变量无关，这里用 1:20 构造一个假 y 也可以
demo_vif <- demo_df %>%
  filter(!is.na(性别), !is.na(年龄),
         !is.na(教育程度), !is.na(摩梭话比重)) %>%
  mutate(y_placeholder = rnorm(n()))   # 假因变量，只为让 lm 能跑

m_vif <- lm(y_placeholder ~ 性别 + 年龄 + 教育程度 + 摩梭话比重,
            data = demo_vif)

cat("\n===== VIF（含 GVIF） =====\n")
print(vif(m_vif))

cat("\n===== 判读：看最后一列 GVIF^(1/(2*Df)) =====\n")
cat("  < 1.41 : 无共线\n")
cat("  1.41~2.24 : 轻中度，可接受\n")
cat("  2.24~3.16 : 中度，需注意\n")
cat("  > 3.16 : 严重，必须处理\n")