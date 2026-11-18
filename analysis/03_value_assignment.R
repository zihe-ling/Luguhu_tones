library(readxl)
library(dplyr)
library(writexl)

# ---------- 1. 读入原始文件 ----------
raw_df <- read_excel("C:/Luguhu_tones/analysis_manual/Luguhu_tone_variation_261110.xlsx")

names(raw_df) <- names(raw_df) %>%
  gsub("^\ufeff", "", .) %>%
  trimws()

need_cols <- c("姓名", "调类", "COP", "EXT",
               "性别", "出生年", "教育程度", "摩梭话比重")
missing_cols <- setdiff(need_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop("缺少列：", paste(missing_cols, collapse = ", "))
}
cat("读入成功：", nrow(raw_df), "条记录，", n_distinct(raw_df$姓名), "人\n")

# ---------- 2. 初始化 12 个混类列 ----------
raw_df <- raw_df %>%
  mutate(
    COP_A1toA2 = 0, COP_A2toA1 = 0,
    COP_B1toB2 = 0, COP_B2toB1 = 0,
    EXT_A1toA2 = 0, EXT_A2toA1 = 0,
    EXT_B1toB2 = 0, EXT_B2toB1 = 0,
    COP_AtoB   = 0, COP_BtoA   = 0,
    EXT_AtoB   = 0, EXT_BtoA   = 0,
    EXT_iso    = 0
  )

# ---------- 3. 逐条赋值 ----------
raw_df <- raw_df %>%
  mutate(
    COP_A1toA2 = ifelse(调类 == "A1" & !is.na(COP) & grepl("HR",  COP), 1, COP_A1toA2),
    COP_A2toA1 = ifelse(调类 == "A2" & !is.na(COP) & grepl("HF",  COP), 1, COP_A2toA1),
    COP_B1toB2 = ifelse(调类 == "B1" & !is.na(COP) & grepl("LF",  COP), 1, COP_B1toB2),
    COP_B2toB1 = ifelse(调类 == "B2" & !is.na(COP) & grepl("LR",  COP), 1, COP_B2toB1),
    EXT_A1toA2 = ifelse(调类 == "A1" & !is.na(EXT) & grepl("HLR", EXT), 1, EXT_A1toA2),
    EXT_A2toA1 = ifelse(调类 == "A2" & !is.na(EXT) & grepl("HHF", EXT), 1, EXT_A2toA1),
    EXT_B1toB2 = ifelse(调类 == "B1" & !is.na(EXT) & grepl("LHF", EXT), 1, EXT_B1toB2),
    EXT_B2toB1 = ifelse(调类 == "B2" & !is.na(EXT) & grepl("LLH", EXT), 1, EXT_B2toB1),
    COP_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(COP) & grepl("^L", COP), 1, COP_AtoB),
    COP_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(COP) & grepl("^H", COP), 1, COP_BtoA),
    EXT_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(EXT) & grepl("^L", EXT), 1, EXT_AtoB),
    EXT_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(EXT) & grepl("^H", EXT), 1, EXT_BtoA),
    EXT_iso  = ifelse(!is.na(EXT) & grepl("^R[A-Z]{2}", EXT), 1, EXT_iso)
  )

# ---------- 3.5 构造 4 个统一 y 列 ----------
raw_df <- raw_df %>%
  mutate(
    y_A_COP = case_when(
      调类 == "A1" ~ as.numeric(COP_A1toA2),
      调类 == "A2" ~ as.numeric(COP_A2toA1),
      TRUE          ~ NA_real_
    ),
    y_A_EXT = case_when(
      调类 == "A1" ~ as.numeric(EXT_A1toA2),
      调类 == "A2" ~ as.numeric(EXT_A2toA1),
      TRUE          ~ NA_real_
    ),
    y_B_COP = case_when(
      调类 == "B1" ~ as.numeric(COP_B1toB2),
      调类 == "B2" ~ as.numeric(COP_B2toB1),
      TRUE          ~ NA_real_
    ),
    y_B_EXT = case_when(
      调类 == "B1" ~ as.numeric(EXT_B1toB2),
      调类 == "B2" ~ as.numeric(EXT_B2toB1),
      TRUE          ~ NA_real_
    )
  )

# ---------- 3.6 定义 demo_df（含派生列） ----------
demo_df <- raw_df %>%
  group_by(姓名) %>%
  summarise(
    性别       = first(na.omit(性别)),
    出生年     = first(na.omit(出生年)),
    教育程度   = first(na.omit(教育程度)),
    摩梭话比重 = first(na.omit(摩梭话比重)),
    .groups    = "drop"
  ) %>%
  mutate(
    年龄       = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(出生年),
    性别       = factor(性别),
    教育程度   = factor(教育程度),
    教育程度2  = factor(
      ifelse(教育程度 %in% c("无", "未上学", "没上过学"), "无", "有"),
      levels = c("无", "有")
    ),
    年龄_z     = as.numeric(scale(年龄)),
    摩梭话_z   = as.numeric(scale(摩梭话比重))
  )

cat("\n===== demo_df 概览 =====\n")
cat("人数：", nrow(demo_df), "\n")
print(table(demo_df$教育程度2, useNA = "ifany"))

# ---------- 4. 保存前自检 ----------
flag_cols <- c(
  "COP_A1toA2", "COP_A2toA1", "COP_B1toB2", "COP_B2toB1",
  "EXT_A1toA2", "EXT_A2toA1", "EXT_B1toB2", "EXT_B2toB1",
  "COP_AtoB", "COP_BtoA", "EXT_AtoB", "EXT_BtoA", "EXT_iso",
  "y_A_COP", "y_A_EXT", "y_B_COP", "y_B_EXT"
)
cat("\n===== 各混类/y列事件数（全部记录） =====\n")
event_summary <- data.frame(
  列名 = flag_cols,
  事件数 = sapply(flag_cols, function(c) sum(raw_df[[c]], na.rm = TRUE))
)
print(event_summary, row.names = FALSE)

#-------4.1 新增：画图-----------
# 8 个“调类-环境”组合的规则
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
  
  sub <- sub %>%
    mutate(
      结果 = case_when(
        .data[[跨调列]] == 1 ~ "其他调",
        .data[[判断列]] == 1 ~ 相邻调类,
        TRUE                 ~ 本调类
      )
    )
  
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

# ---------- 4.1 所有人 ----------
stat_overall <- pmap_dfr(
  list(stat_specs$调类, stat_specs$环境, stat_specs$本调类,
       stat_specs$相邻调类, stat_specs$判断列, stat_specs$跨调列),
  function(tone, env, ben, lin, judge, cross) {
    calc_stat(raw_df, tone, env, ben, lin, judge, cross)
  }
)

cat("\n===== 总体统计（所有人） =====\n")
print(as.data.frame(stat_overall), row.names = FALSE)

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

# ---------- 4.2 分性别 ----------
stat_by_gender <- raw_df %>%
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

# ---------- 4.3 分教育程度（二分类） ----------
# 把 教育程度3 拼到逐条记录级别
raw_df_edu <- raw_df %>%
  left_join(demo_df %>% select(姓名, 教育程度2), by = "姓名")

stat_by_education <- raw_df_edu %>%
  group_by(教育程度2) %>%
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
  relocate(教育程度2, .before = 1)

cat("\n===== 分教育程度统计 =====\n")
print(as.data.frame(stat_by_education), row.names = FALSE)

plot_education <- stat_by_education %>%
  select(教育程度2, 组合, 本调类比例, 相邻调类比例, 其他调比例) %>%
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
    教育程度3 = factor(教育程度3,
                   levels = c("无", "有"))
  )

p_bar_education <- ggplot(plot_education,
                          aes(x = 组合, y = 比例, fill = 结果类型)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  geom_text(
    aes(label = ifelse(比例 >= 0.05, sprintf("%.2f", 比例), "")),
    position = position_stack(vjust = 0.5),
    size = 2.2, color = "black"
  ) +
  scale_fill_manual(values = c("本调类" = "#4C72B0",
                               "相邻调类" = "#DD8452",
                               "其他调" = "#55A868")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  facet_wrap(~ 教育程度2, ncol = 2) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "比例", fill = "读音类型",
       title = "各调类-环境组合的读音分布（分教育程度）") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text = element_text(size = 11, face = "bold"))

print(p_bar_education)

# ---------- 5. 另存为新 Excel ----------
out_file <- "C:/Luguhu_tones/data/Luguhu_tone_variation_261110_with_flags.xlsx"
write_xlsx(raw_df, out_file)
cat("\n已保存：", normalizePath(out_file), "\n")