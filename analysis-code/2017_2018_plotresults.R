library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
library(cowplot)
library(bb2enchist)

windowsFonts(Times=windowsFont("TT Times New Roman"))

# set custom theme for all plots

theme_ajm <- function() {
  theme_classic(base_family = "Times") %+replace%
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(size=8, colour = "black"),
          axis.title.y = element_text(size=10, angle = 90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_text(size=8, colour = "black"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          strip.text.x = element_text(size=12, face = "bold"),
          legend.text = element_text(size=8),
          legend.key.height = unit(1, "mm"),
          plot.title = element_text(size = 12, hjust = 0, vjust = 1.5),
          panel.border = element_rect(size =0.5, fill = "transparent"),
          plot.margin = margin(10, 10, 10, 15))
}

# load data

phitpc_results <- read_csv("data-files/2017_2018_dataforplots_v2.csv")
#phitpc_results$Date <- dmy(phitpc_results$Date)
phitpc_results$Date <- as.Date(phitpc_results$Date, format = "%d-%b")
phitpc_results$Year <- as.factor(phitpc_results$Year)

# stopover population

Nstop_plot <- ggplot(phitpc_results, aes(x=Date, y=Nstop, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=Nstop_low, ymax=Nstop_high), width=0, size=0.5, colour="black", linetype =1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("f) Stopover population") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  #ylim(0, 15000) +
  ylab("Estimated population size") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.85),
        legend.title = element_blank())

# entries to site

b_plot <- ggplot(phitpc_results, aes(x=Date, y=b, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=b_low, ymax=b_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("a) Entry probability") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1)) +
  ylab("Proportion arriving") +
  theme_ajm () +
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())

Bstop_plot <- ggplot(phitpc_results, aes(x=Date, y=Bstop, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=Bstop_low, ymax=Bstop_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("a) New entries") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  #scale_y_continuous(breaks = seq(0, 0.5, 0.1)) +
  ylab("Number arriving") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.85),
        legend.title = element_blank())

# proportion adult

padult_plot <- ggplot(phitpc_results, aes(x=Date, y=padult, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=padult_low, ymax=padult_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("e) Proportion adult") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("Proportion adult") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.15),
        legend.title = element_blank())

# persistence probability

phi_plot <- ggplot(phitpc_results, aes(x=Date, y=phi, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=phi_low, ymax=phi_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("b) Persistence probability") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("Persistence probability") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.15),
        legend.title = element_blank())

# resighting probability

psight_plot <- ggplot(phitpc_results, aes(x=Date, y=psight, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=psight_low, ymax=psight_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("c) Resighting probability") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("Resighting probability") +
  ylim(0,1) +
  theme_ajm () +
  theme(legend.position = c(0.8,0.85),
        legend.title = element_blank())

# flagged proportion

pflag_plot <- ggplot(phitpc_results, aes(x=Date, y=pflag, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=pflag_low, ymax=pflag_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  ggtitle("d) Proportion flagged") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylim(0,0.25) +
  ylab("Proportion flagged") +
  theme_ajm () +
  theme(legend.position = c(0.8,0.85),
        legend.title = element_blank())

# plot in grid

all_plots <- arrangeGrob(b_plot, phi_plot, psight_plot, pflag_plot,
                         padult_plot, Nstop_plot, nrow=3, ncol=2)

png(filename = paste0("figures/2017_2018_stopoverplots_3Oct2020.png"),
    width=6, height=7, units="in", res=600)

plot(all_plots)

dev.off()

# Histogram for stopover duration

sims_list_2018 <- read_csv("analysis-output/ALLCAMPS2018_RDO_phitpc_sims.list_2020-03-08.csv")
sims_list_2018 <- sims_list_2018 %>% 
  select(zes.days) %>% 
  mutate(year = 2018)

sims_list_2017 <- read_csv("analysis-output/PISKLR2017_superpop_phicpt_sims.list_2020-04-23.csv")
sims_list_2017 <- sims_list_2017 %>% 
  select(zes.days) %>% 
  mutate(year = 2017)

zes_plot_2018 <- ggplot(sims_list_2018, aes(x=zes.days)) +
  geom_histogram(binwidth = 0.2, colour="black", fill="grey") +
  geom_vline(aes(xintercept=12.2),
             color="black", size=1, linetype="dashed") +
  annotate("text", label = "c)", family = "Times", x = 5.3, y = 2100, size = 5) +
  annotate("text", label = "2018", family = "Times", x = 15.5, y = 2100, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(5, 17), breaks = seq(5, 17, 1)) +
  scale_y_continuous(limits = c(0,2250), expand = c(0,0), breaks = seq(0, 2250, 500)) +
  xlab("Stopover duration (days)") +
  ylab("Frequency (MCMC iterations)")

zes_plot_2017 <- ggplot(sims_list_2017, aes(x=zes.days)) +
  geom_histogram(binwidth = 0.21, colour="black", fill="grey") +
  geom_vline(aes(xintercept=9.2),
             color="black", size=1, linetype="dashed") +
  annotate("text", label = "a)", family = "Times", x = 5.3, y = 1400, size = 5) +
  annotate("text", label = "2017", family = "Times", x = 15.5, y = 1400, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(5, 17), breaks = seq(5, 17, 1)) +
  scale_y_continuous(limits = c(0,1500), expand = c(0,0), breaks = seq(0, 1500, 300)) +
  xlab("Stopover duration (days)") +
  ylab("Frequency (MCMC iterations)")

# MINLOS plots
# 2017
bandedbirds_2017 <- readRDS("data-files/ALLCAMPS2017_bandedbirds_resights.rds")

bandedbirds_2017 <- bandedbirds_2017 %>%
  fill(LocationID) %>% 
  filter(!str_detect(LocationID, "NORTHBLUFFPT"))

# reformat data
resight_data_2017 <- bandedbirds_2017 %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data_2017 <- format_bandedbirds(resight_data_2017, sp = "REKN", cert = FALSE)

resight_data_2017$ResightDate <- mdy(resight_data_2017$ResightDate)

minlos_data_2017 <- resight_data_2017 %>%
  group_by(FlagID) %>% 
  summarize(first_resight = min(ResightDate),
            last_resight = max(ResightDate),
            minlos = as.numeric(last_resight - first_resight))

minlos_data_2017 <- minlos_data_2017 %>% 
  filter(!minlos == 0)

max(minlos_data_2017$minlos)
median(minlos_data_2017$minlos)

minlos_plot_2017 <- ggplot(minlos_data_2017, aes(x = minlos)) +
  geom_histogram(binwidth = 1, colour="black", fill="grey") +
  geom_vline(aes(xintercept=6),
             color="black", size=1, linetype="dashed") +
  annotate("text", label = "b)", family = "Times", x = 1.5, y = 14, size = 5) +
  annotate("text", label = "2017", family = "Times", x = 26, y = 14, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,31), expand = c(0,0), breaks = seq(0, 31, 5)) +
  scale_y_continuous(limits = c(0,15), expand = c(0,0), breaks = seq(0, 15, 5)) +
  xlab("Minimum length of stay (days)") +
  ylab("Number of Red Knots")

# 2018
bandedbirds_2018 <- readRDS("data-files/ALLCAMPS2018_bandedbirds_resights.rds")

# reformat data
resight_data_2018 <- bandedbirds_2018 %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data_2018 <- format_bandedbirds(resight_data_2018, sp = "REKN", cert = FALSE)

resight_data_2018$ResightDate <- mdy(resight_data_2018$ResightDate)

minlos_data_2018 <- resight_data_2018 %>%
  group_by(FlagID) %>% 
  summarize(first_resight = min(ResightDate),
            last_resight = max(ResightDate),
            minlos = as.numeric(last_resight - first_resight))

minlos_data_2018 <- minlos_data_2018 %>% 
  filter(!minlos == 0)

max(minlos_data_2018$minlos)
median(minlos_data_2018$minlos)

minlos_plot_2018 <- ggplot(minlos_data_2018, aes(x = minlos)) +
  geom_histogram(binwidth = 1, colour="black", fill="grey") +
  geom_vline(aes(xintercept=9),
             color="black", size=1, linetype="dashed") +
  annotate("text", label = "d)", family = "Times", x = 2, y = 29, size = 5) +
  annotate("text", label = "2018", family = "Times", x = 26, y = 29, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,31), expand = c(0,0), breaks = seq(0, 31, 5)) +
  scale_y_continuous(limits = c(0,31), expand = c(0,0), breaks = seq(0, 31, 5)) +
  xlab("Minimum length of stay (days)") +
  ylab("Number of Red Knots")

# zes and minlos plots
stopover_plots <- arrangeGrob(zes_plot_2017, minlos_plot_2017, zes_plot_2018, minlos_plot_2018, nrow=2, ncol=2)

png(filename = paste0("figures/2017_2018_lengthofstayplots.png"),
    width=6, height=5, units="in", res=600)

plot(stopover_plots)

dev.off()


ggsave(file = "2017_2018_lengthofstayplots_finalthesis.jpeg", stopover_plots, dpi = 600)


#### COLOUR PLOTS FOR PRESENTATIONS ####

# set custom theme for all plots

theme_ajm <- function() {
  theme_classic() %+replace%
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(size=12, colour = "black"),
          axis.title.y = element_text(size=14, angle = 90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_text(size=12, colour = "black"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          legend.text = element_text(size=12),
          strip.text.x = element_text(size=12, face = "bold"),
          plot.title = element_text(size = 16, hjust = 0, vjust = 1),
          panel.border = element_rect(size =0.5, fill = "transparent"),
          plot.margin = margin(10, 10, 10, 15))
}

phitpc_results <- read_csv("passage population/2017_2018_dataforplots.csv")
#phitpc_results$Date <- dmy(phitpc_results$Date)
phitpc_results$Date <- as.Date(phitpc_results$Date, format = "%d-%b")
phitpc_results$Year <- as.factor(phitpc_results$Year)
phitpc_results <- phitpc_results %>% 
  filter(Year == "2018")

# population size

Stop_plot <- ggplot(phitpc_results, aes(x=Date, y=Nstop)) +
  geom_errorbar(aes(ymin=Nstop_low, ymax=Nstop_high), width=0, size=0.5, colour="black") +
  geom_line() +
  geom_point(size=3, shape = 21, fill = "#440154FF") +
  ggtitle("Stopover population size") +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("# REKN") +
  theme_ajm ()

# entry probability

b_plot <- ggplot(phitpc_results, aes(x=Date, y=b, group=1)) +
  geom_errorbar(aes(ymin=b_low, ymax=b_high), width=0, size=0.5, colour="black") +
  geom_line() +
  geom_point(size=3, shape = 21, fill = "#31688EFF") +
  ggtitle("Entry probability") +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("Proportion\narriving") +
  theme_ajm ()

# proportion adult

padult_plot <- ggplot(phitpc_results, aes(x=Date, y=(padult*100), group=1)) +
  geom_errorbar(aes(ymin=(padult_low*100), ymax=(padult_high*100)), width=0, size=0.5, colour="black") +
  geom_line() +
  geom_point(size=3, shape = 21, fill = "#35B779FF") +
  ggtitle("Percentage adult birds") +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylim(-2, 105) +
  xlab("Date") +
  ylab("\n% adult") +
  theme_ajm ()

# persistence probability

phi_plot <- ggplot(phitpc_results, aes(x=Date, y=phi, group=1)) +
  geom_errorbar(aes(ymin=phi_low, ymax=phi_high), width=0, size=0.5, colour="black") +
  geom_line() +
  geom_point(size=3, shape = 21, fill = "#FDE725FF") +
  ggtitle("Persistence probability") +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  ylab("Persistence\nprobability") +
  theme_ajm ()

# plot in grid

all_plots1 <- plot_grid(Stop_plot, b_plot, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
all_plots2 <- plot_grid(Stop_plot, phi_plot, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
all_plots3 <- plot_grid(Stop_plot, padult_plot, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
all_plots4 <- plot_grid(Stop_plot, b_plot, phi_plot, align = "v", nrow = 3)

ggsave(file = "ALLCAMPS2018_phitpc_presentationplots_Nstop.jpeg", Stop_plot, dpi = 600)
ggsave(file = "ALLCAMPS2018_phitpc_presentationplots_Nstopb.jpeg", all_plots1, dpi = 600)
ggsave(file = "ALLCAMPS2018_phitpc_presentationplots_Nstopphi.jpeg", all_plots2, dpi = 600)
ggsave(file = "ALLCAMPS2018_phitpc_presentationplots_Nstoppadult.jpeg", all_plots3, dpi = 600)
ggsave(file = "ALLCAMPS2018_phitpc_presentationplots_Nstopbphi.jpeg", all_plots4, dpi = 600)

# length of stay plots

sims_list_2018 <- read_csv("ALLCAMPS2018_RDO_phitpc_sims.list_2019-02-11.csv")
sims_list_2018 <- sims_list_2018 %>% 
  select(zes.days) %>% 
  mutate(method = "IPM")

# MINLOS

bandedbirds_2018 <- read_csv("passage population/ALLCAMPS2018_bandedbirds_resights.csv")

# reformat data
resight_data_2018 <- bandedbirds_2018 %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100)

resight_data_2018 <- format_bandedbirds(resight_data_2018, sp = "REKN", cert = FALSE)

resight_data_2018$ResightDate <- mdy(resight_data_2018$ResightDate)

minlos_data_2018 <- resight_data_2018 %>%
  group_by(FlagID) %>% 
  summarize(first_resight = min(ResightDate),
            last_resight = max(ResightDate),
            minlos = as.numeric(last_resight - first_resight))

minlos_data_2018 <- minlos_data_2018 %>% 
  filter(!minlos == 0) %>% 
  dplyr::rename(zes.days = minlos) %>% 
  select(zes.days) %>% 
  mutate(method = "MINLOS")

# append datasets

los_data_2018 <- bind_rows(sims_list_2018, minlos_data_2018)

# density plot

los_plot_2018 <- ggplot(los_data_2018, aes(x=zes.days, fill=method)) +
  geom_density(alpha=0.3) +
  geom_vline(aes(xintercept=12),
             color="black", size=1, linetype="dashed") +
  geom_vline(aes(xintercept=9),
             color="black", size=1, linetype="dashed") +
  theme_ajm() +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0,0), breaks = seq(0, 0.5, 0.1)) +
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank()) +
  xlab("Stopover duration (days)") +
  ylab("Density")

ggsave(file = "los_plot_2018_colour.jpeg", los_plot_2018, dpi = 600)

# boxplot

los_boxplot_2018 <- ggplot(los_data_2018, aes(x=method, y=zes.days, colour = method)) +
  scale_color_manual(values = c("#440154FF", "#21908CFF")) +
  geom_point(shape = 1, size = 2, position = position_jitterdodge(), alpha = 0.3) +
  geom_boxplot(outlier.size = -1, lwd = 1, alpha=0) +
  scale_x_discrete(labels = c("Integrated\nPopulation\nModel", "Minimum\nLength of Stay")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme_ajm() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ylab("Stopover duration (days)")

ggsave(file = "los_boxplot_2018_colour.jpeg", los_boxplot_2018, dpi = 600)

# points plot

los_plot_data <- data.frame(method = c("IPM", "MINLOS"),
                            zes.days = c(12, 9.361564),
                            lcl = c(10.6, (9.361564-1.96*3.759708)),
                            ucl = c(14, (9.361564+1.96*3.759708)))

los_point_plot_2018 <- ggplot(los_plot_data, aes(x=method, y=zes.days, fill = method)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width=0, size=0.5, colour="black") +
  geom_point(shape = 21, size = 6) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF")) +
  scale_x_discrete(labels = c("Integrated\nPopulation\nModel", "Minimum\nLength of Stay")) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme_ajm() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ylab("Stopover duration (days)")

ggsave(file = "los_point_plot_2018_colour.jpeg", los_point_plot_2018, dpi = 600)

