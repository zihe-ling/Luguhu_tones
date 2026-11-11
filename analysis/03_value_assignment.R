
library(readxl)
library(dplyr)
library(writexl)

# ---------- 1. 读入原始文件 ----------
raw_df <- read_excel("Luguhu_tone_variation_261110.xlsx")

# 清理列名（去 BOM、前后空格）
names(raw_df) <- names(raw_df) %>%
  gsub("^\ufeff", "", .) %>%
  trimws()

# 确认必要列都存在
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
    # ---- COP 相邻调类混类 ----
    COP_A1toA2 = ifelse(调类 == "A1" & !is.na(COP) & grepl("HR",  COP), 1, COP_A1toA2),
    COP_A2toA1 = ifelse(调类 == "A2" & !is.na(COP) & grepl("HF",  COP), 1, COP_A2toA1),
    COP_B1toB2 = ifelse(调类 == "B1" & !is.na(COP) & grepl("LF",  COP), 1, COP_B1toB2),
    COP_B2toB1 = ifelse(调类 == "B2" & !is.na(COP) & grepl("LR",  COP), 1, COP_B2toB1),
    
    # ---- EXT 相邻调类混类 ----
    EXT_A1toA2 = ifelse(调类 == "A1" & !is.na(EXT) & grepl("HLR", EXT), 1, EXT_A1toA2),
    EXT_A2toA1 = ifelse(调类 == "A2" & !is.na(EXT) & grepl("HHF", EXT), 1, EXT_A2toA1),
    EXT_B1toB2 = ifelse(调类 == "B1" & !is.na(EXT) & grepl("LHF", EXT), 1, EXT_B1toB2),
    EXT_B2toB1 = ifelse(调类 == "B2" & !is.na(EXT) & grepl("LLH", EXT), 1, EXT_B2toB1),
    
    # ---- 跨调域混类 ----
    COP_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(COP) & grepl("^L", COP), 1, COP_AtoB),
    COP_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(COP) & grepl("^H", COP), 1, COP_BtoA),
    EXT_AtoB = ifelse(调类 %in% c("A1","A2") & !is.na(EXT) & grepl("^L", EXT), 1, EXT_AtoB),
    EXT_BtoA = ifelse(调类 %in% c("B1","B2") & !is.na(EXT) & grepl("^H", EXT), 1, EXT_BtoA),
    
    # ---- EXT 起首 R + 两个大写字母 ----
    EXT_iso = ifelse(!is.na(EXT) & grepl("^R[A-Z]{2}", EXT), 1, EXT_iso)
  )

# ---------- 4. 保存前自检 ----------
flag_cols <- c(
  "COP_A1toA2", "COP_A2toA1", "COP_B1toB2", "COP_B2toB1",
  "EXT_A1toA2", "EXT_A2toA1", "EXT_B1toB2", "EXT_B2toB1",
  "COP_AtoB", "COP_BtoA", "EXT_AtoB", "EXT_BtoA", "EXT_iso"
)
cat("\n===== 各混类列事件数（全部记录） =====\n")
event_summary <- data.frame(
  混类列 = flag_cols,
  事件数 = sapply(flag_cols, function(c) sum(raw_df[[c]], na.rm = TRUE))
)
print(event_summary, row.names = FALSE)

# ---------- 5. 另存为新 Excel ----------
out_file <- "Luguhu_tone_variation_261110_with_flags.xlsx"
write_xlsx(raw_df, out_file)
cat("\n已保存：", normalizePath(out_file), "\n")