#generate graph for ppt
# ---- Packages ----
library(tiff)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(dplyr)

# ---- Parameters (edit these) ----
analysis_file <- "statsdir/rep2_analysis.txt"  # <-- path to your _analysis.txt
frame_folder  <- "avi_frames/Rep_2_Hcr_frames"              # folder with .tif images
start_date    <- as.Date("2025-09-01")
start_time    <- "08:00"
chamber_split <- 980                              # vertical split in pixels

# ---- Load analysis data ----
df <- read.table(analysis_file, header = TRUE, sep = "\t")
df$frame_num <- as.numeric(df$frame_num)

# ---- Earliest appearance for each ID ----
first <- aggregate(frame_num ~ id, data = df, FUN = min)
df_first <- merge(first, df, by = c("id", "frame_num"))

# ---- Classify by side (using bounding-box x coords) ----
df_first$side <- ifelse(
  (df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4) / 4 <= chamber_split,
  "Left", "Right"
)

# ---- Convert frames → datetime (assumes 5 min per frame) ----
start_dt <- ymd_hm(paste(start_date, start_time))
df_first$datetime <- start_dt +
  minutes((df_first$frame_num - min(df$frame_num)) * 5)

# ---- Cumulative counts ----
df_cum <- df_first %>%
  arrange(datetime) %>%
  group_by(side) %>%
  mutate(cumN = row_number())

# ---- Plot ----
p <- ggplot(df_cum, aes(x = datetime, y = cumN, colour = side)) +
  geom_step(linewidth = 1.2) +
  labs(x = "Time", y = "Cumulative pupariations", colour = "Side") +
  theme_minimal(base_size = 32)

print(p)
