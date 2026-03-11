# 加载必要的包
library(tidyverse)
library(mosaic)
library(RColorBrewer) # 用于获取柔和的配色

# 1. 读取数据并转换时长为分钟
Songs <- read.csv("Songs.csv")
Songs$Duration_Minutes <- Songs$Duration_Seconds / 60

# ==========================================
# 拼贴海报专用主题：全透明背景 + 极简粗线条
# ==========================================
poster_theme <- theme_minimal(base_size = 14) +
  theme(
    # 强制背景透明，去掉所有自带的白色底板
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    # 统一字体颜色，模仿深灰蓝色水笔
    text = element_text(color = "#2c3e50", family = "sans", face = "bold"),
    axis.text = element_text(color = "#34495e", face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#e74c3c", size = 16),
    # 把网格线改成虚线，模仿手账本的格子
    panel.grid.major = element_line(color = "#bdc3c7", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# 2. 生成图表并保存 (注意 bg = "transparent" 参数)

# 【图 1】 直方图 (Histogram) - 使用粉红色系
p1 <- ggplot(data = Songs, aes(x = Duration_Minutes)) +
  geom_histogram(binwidth = 1, fill = "#ff7979", color = "#2c3e50", linewidth = 1) +
  labs(title = "Duration Distribution", x = "Mins", y = "Count") + 
  poster_theme

ggsave("plot1_histogram.png", plot = p1, width = 5, height = 4, dpi = 300, bg = "transparent")

# 【图 2】 总体箱线图 (Overall Boxplot) - 使用淡黄色系，并把异常值改成显眼的紫色
p2 <- ggplot(data = Songs, mapping = aes(x = Duration_Minutes)) +
  geom_boxplot(fill = "#f6e58d", color = "#2c3e50", linewidth = 1, outlier.size = 3, outlier.color = "#e056fd") +
  labs(title = "Spread & Outliers", x = "Mins") + 
  poster_theme +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) # 隐藏不必要的y轴

ggsave("plot2_boxplot.png", plot = p2, width = 5, height = 3, dpi = 300, bg = "transparent")

# 【图 3】 流派频率柱状图 (Genre Bar Plot) - 使用抹茶绿色系
Genre_Counts <- Songs %>% 
  group_by(Genre) %>% 
  summarise(Count = n()) %>% 
  mutate(Proportion = Count / sum(Count))

p3 <- ggplot(Genre_Counts, aes(x = reorder(Genre, -Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#badc58", color = "#2c3e50", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Genre Breakdown", x = "Genre", y = "%") + 
  poster_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 倾斜x轴文字防重叠

ggsave("plot3_barplot.png", plot = p3, width = 5, height = 4, dpi = 300, bg = "transparent")

# 【图 4】 分组箱线图 (Boxplot By Genre) - 使用调色盘柔和色系
p4 <- ggplot(data = Songs, mapping = aes(x = reorder(Genre, Duration_Minutes, FUN = median), y = Duration_Minutes, fill = Genre)) +
  geom_boxplot(show.legend = FALSE, color = "#2c3e50", linewidth = 1) +
  scale_fill_brewer(palette = "Pastel1") + # 使用柔和色系
  labs(title = "Durations By Genre", x = "", y = "Mins") + 
  poster_theme + 
  coord_flip() # 翻转坐标轴方便阅读

ggsave("plot4_boxplot_genre.png", plot = p4, width = 5, height = 5, dpi = 300, bg = "transparent")

print("4张手绘风全透明图表已成功导出！")