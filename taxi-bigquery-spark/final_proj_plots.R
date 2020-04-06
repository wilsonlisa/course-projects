nyc_7 <- scale_color_manual(values = c("#C60C30", "#CE8E00", "#f3901d", "#00985F", "#006983", "#8E258D", "#6E3219"))
nyc_4_c <- scale_color_manual(values = c("#003262", "#257ea5", "#5e0844", "#f3901d"))
nyc_4_f <- scale_fill_manual(values = c("#003262", "#257ea5", "#5e0844", "#f3901d"))
nyc_lh <- scale_color_gradient(low = "#52a382", high = "#016334")

chrys_avg <- read.csv("~/Documents/CS512/final/Chrysler results/pyspark_demo_output3_part-00000", header=FALSE, quote="'")
colnames(chrys_avg) <- c("dayweek", "avg_dist")
chrys_avg$dayweek <- gsub("\\(", "", chrys_avg$dayweek)
chrys_avg$avg_dist <- gsub("\\)", "", chrys_avg$avg_dist)

chrys_std1 <- read.csv("~/Documents/CS512/final/Chrysler results/pyspark_demo_output3_part-00001", header=FALSE, quote="'")
colnames(chrys_std1) <- c("dayweek", "std_dist")
chrys_std1$dayweek <- gsub("\\(", "", chrys_std1$dayweek)
chrys_std1$std_dist <- gsub("\\)", "", chrys_std1$std_dist)

chrys_std2 <- read.csv("~/Documents/CS512/final/Chrysler results/pyspark_demo_output3_part-00002", header=FALSE, quote="'")
colnames(chrys_std2) <- c("dayweek", "std_dist")
chrys_std2$dayweek <- gsub("\\(", "", chrys_std2$dayweek)
chrys_std2$std_dist <- gsub("\\)", "", chrys_std2$std_dist)

chrys <- inner_join(chrys_avg, rbind(chrys_std1, chrys_std2))
chrys$avg_dist <- as.numeric(chrys$avg_dist)
chrys$std_dist <- as.numeric(chrys$std_dist)

ggplot(chrys) +
    geom_errorbar(aes(ymin = avg_dist-std_dist, ymax = avg_dist+std_dist, x = dayweek, color = as.factor(dayweek)), width = 0.2, size = 1) +
    geom_point(aes(y = avg_dist, x = dayweek), size=2) +
    nyc_7 + 
    labs(x = "Day of the week", y = "Average distance (miles)", title = "Average taxi trip distance with standard deviation by day of the week") +
    scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday",
                                "Thursday", "Friday", "Saturday",
                                "Sunday")) +
    theme_bw() +
    theme(legend.position = "none") 

time_avg <- read.csv("~/Documents/CS512/final/hour_time.txt", header=FALSE, quote="'")
time_avg$V3 <- NULL
colnames(time_avg) <- c("hour", "avg_time")
time_avg$hour <- gsub("\\(", "", time_avg$hour)
time_avg$avg_time <- gsub("\\)", "", time_avg$avg_time)

time_avg$hour <- as.numeric(time_avg$hour)
time_avg$avg_time <- as.numeric(time_avg$avg_time)
time_avg$time_day <- cut(time_avg$hour, breaks = c(0, 4, 10, 16, 22, 24), labels = c("late night", "morning", "midday", "evening", "late night"))
time_avg[1, 3] <- "late night"

# color code by 10-4, 4-10, 10-4, 4-10
ggplot(time_avg) +
    geom_col(aes(x=hour, y=avg_time, fill=time_day)) +
    nyc_4_f + 
    scale_x_continuous(breaks = seq(0, 23, 1),
                       labels = c("midnight", "1:00", "2:00", "3:00",
                                  "4:00", "5:00", "6:00", "7:00",
                                  "8:00", "9:00", "10:00", "11:00",
                                  "noon", "13:00", "14:00", "15:00",
                                  "16:00", "17:00", "18:00", "19:00",
                                  "20:00", "21:00", "22:00", "23:00")) +
    labs(x = "Hour of pickup", y = "Average trip time (minutes)", title = "Average taxi trip time by hour", fill = "Time of day") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(time_avg) +
    geom_boxplot(aes(x=time_day, y=avg_time, color=time_day)) +
    nyc_4_c +
    labs(x = "Pickup time of day", y = "Average trip time (minutes)", title = "Average taxi trip time by time of day") +
    theme_bw() +
    theme(legend.position = "none")

tip_month <- read.csv("~/Documents/CS512/final/tip_month.txt", header=FALSE, quote="'")
tip_month$V3 <- NULL
colnames(tip_month) <- c("month", "avg_tip")
tip_month$month <- gsub("\\(", "", tip_month$month)
tip_month$avg_tip <- gsub("\\)", "", tip_month$avg_tip)
tip_month$payment <- c(rep("Card", 12), rep("Cash", 12))

tip_month$month <- as.factor(tip_month$month)
tip_month$avg_tip <- as.numeric(tip_month$avg_tip)
tip_month$payment <- as.factor(tip_month$payment)

tip_max_card <- 200.0
tip_min_card <- 0
tip_max_cash <- 121.5
tip_min_cash <- 0

ggplot(filter(tip_month, payment=="Card"), aes(month, avg_tip)) +
    geom_linerange(aes(x = month, ymin = 0, ymax = avg_tip, color = avg_tip), position = position_dodge(0.8), size=1) +
    geom_point(aes(color = avg_tip), position = position_dodge(0.8), size=2) +
    nyc_lh + 
    scale_y_continuous(breaks = seq(0, 2.7, 0.5)) +
    labs(x = "Month", y = "Average tip amount", title = "Card tips") +
    theme_bw() +
    theme(legend.position = "none")

ggplot(filter(tip_month, payment=="Cash"), aes(month, avg_tip)) +
    geom_linerange(aes(x = month, ymin = 0, ymax = avg_tip, color = avg_tip), position = position_dodge(0.8), size=1) +
    geom_point(aes(color = avg_tip), position = position_dodge(0.8), size=2) +
    nyc_lh + 
    labs(x = "Month", y = "Average tip amount", title = "Cash tips") +
    theme_bw() +
    theme(legend.position = "none")