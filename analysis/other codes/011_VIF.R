library(dplyr)
library(car)

# ---------- 1. 先看 summary_df 里教育程度的原始取值 ----------
cat("===== summary_df 中教育程度原始分布 =====\n")
print(table(summary_df$教育程度, useNA = "ifany"))
print(levels(summary_df$教育程度))

# ---------- 2. 新建 VIF 专用表，重新编码教育程度 ----------
vif_df <- summary_df %>%
  mutate(
    # 先把可能存在的因子转回字符，避免 factor 赋值出错
    教育程度 = as.character(教育程度),
    
    # 重新编码：合并为 3 类
    教育程度 = case_when(
      教育程度 %in% c("无", "未上学", "没上过学")           ~ "无",
      教育程度 == "小学"                                   ~ "小学",
      教育程度 %in% c("初中", "中专", "大专")              ~ "初中及以上",
      TRUE                                                 ~ NA_character_
    ),
    
    # 设为因子，"无" 作基线
    教育程度 = factor(教育程度,
                  levels = c("无", "小学", "初中及以上")),
    
    # 性别也顺便统一成因子
    性别 = factor(性别)
  )

# ---------- 3. 检查新表 ----------
cat("\n===== vif_df 中教育程度分布 =====\n")
print(table(vif_df$教育程度, useNA = "ifany"))
cat("教育程度水平数:", nlevels(vif_df$教育程度), "\n")

cat("\n===== vif_df 中性别分布 =====\n")
print(table(vif_df$性别, useNA = "ifany"))

# 若教育程度有 NA，看看是哪些原始值没匹配上
if (any(is.na(vif_df$教育程度))) {
  cat("\n===== 未匹配到分类的原始教育程度取值 =====\n")
  print(table(summary_df$教育程度[is.na(vif_df$教育程度)], useNA = "ifany"))
}

# ---------- 4. 跑 VIF ----------
m_vif <- lm(A1_COP ~ 性别 + 年龄 + 教育程度 + 摩梭话比重,
            data = vif_df)

cat("\n===== VIF 结果 =====\n")
print(vif(m_vif))