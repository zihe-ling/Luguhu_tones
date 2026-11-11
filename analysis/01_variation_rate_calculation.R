# 安装所需包（如未安装，取消注释后运行）
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "car"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

# ---------- 1. 读入 Excel 文件 ----------
file_path <- "Luguhu_tone_variation_261030.xlsx"
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
    EXT_A2toA1 = ifelse(调类 == "A2" & !is.na(EXT) & grepl("HLL", EXT), 1, EXT_A2toA1),
    EXT_B1toB2 = ifelse(调类 == "B1" & !is.na(EXT) & grepl("LHL", EXT), 1, EXT_B1toB2),
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
# 用聚合到个人的数据，VIF 与因变量无关，任选一个混类值即可
m_vif <- lm(A1_COP ~ 性别 + 年龄 + 教育程度 + 摩梭话比重,
            data = summary_df)

cat("\n===== VIF 结果 =====\n")
print(vif(m_vif))

# 若想看到更直观的“折算后 VIF”，可看最后一列 GVIF^(1/(2*Df))
# 判读标准：最后一列 > 3.16 才说明严重共线