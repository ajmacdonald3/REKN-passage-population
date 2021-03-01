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
    theme(axis.title.x = element_text(size=10),
          axis.text.x  = element_text(size=8, colour = "black"),
          axis.title.y = element_text(size=10, angle = 90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_text(size=8, colour = "black"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          strip.text.x = element_text(size=12, face = "bold"),
          legend.text = element_text(size=8),
          legend.key.height = unit(1, "mm"),
          plot.title = element_text(size = 12, hjust = 0, vjust = 1.5),
          #panel.border = element_rect(size =0.5, fill = "transparent"),
          plot.margin = margin(10, 10, 10, 15))
}

# load data

phitpc_results <- read_csv("data-files/PlotModelResults.csv")
#phitpc_results$Date <- dmy(phitpc_results$Date)
phitpc_results$Date <- as.Date(phitpc_results$Date, format = "%d-%b")
phitpc_results$Year <- as.factor(phitpc_results$Year)

model_means <- read_csv("data-files/PlotModelMeans.csv")

# stopover population

Nstop_plot <- ggplot(phitpc_results, aes(x=Date, y=Nstop/1000, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=Nstop_low/1000, ymax=Nstop_high/1000),
                width=0, size=0.5, colour="black", linetype =1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  #ggtitle("**F** Stopover population") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  xlab("Date") +
  ylab("Estimated population size (x1000)") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.85),
        legend.title = element_blank())

# entries to site

b_plot <- ggplot(phitpc_results, aes(x=Date, y=b, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=b_low, ymax=b_high), width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  #ggtitle("a) Entry probability") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1)) +
  ylab("Entry probability") +
  theme_ajm () +
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank(),
        axis.title.x = element_blank())

Bstop_plot <- ggplot(phitpc_results, aes(x=Date, y=Bstop, linetype=Year, fill=Year)) +
  geom_errorbar(aes(ymin=Bstop_low, ymax=Bstop_high),
                width=0, size=0.5, colour="black", linetype=1) +
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
  geom_errorbar(aes(ymin=padult_low, ymax=padult_high),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(size=2, shape=21) +
  #ggtitle("e) Proportion adult") +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_date(date_breaks="10 days", labels = date_format("%d %b")) +
  xlab("Date") +
  ylab("Proportion adult") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.15),
        legend.title = element_blank())

# persistence probability

phi_plot <- ggplot() +
  # geom_rect(data = filter(model_means, parameter == "phi" & year == 2017),
  #                  aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  # geom_rect(data = filter(model_means, parameter == "phi" & year == 2018),
  #           aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(model_means, parameter == "phi" & year == 2017),
             aes(yintercept=mean)) +
  geom_hline(data = filter(model_means, parameter == "phi" & year == 2018),
             aes(yintercept=mean), linetype = 2) +
  geom_errorbar(data = phitpc_results, aes(x=as.numeric(Date),
                                           ymin=phi_low, ymax=phi_high),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = phitpc_results, aes(x=as.numeric(Date), y=phi, linetype=Year),
            size=0.5) +
  geom_point(data = phitpc_results, aes(x=as.numeric(Date), y=phi, fill=Year),
             size=2, shape=21) +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylab("Persistence probability") +
  theme_ajm () +
  theme(legend.position = c(0.4,0.15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title.x = element_blank())

# resighting probability

psight_plot <- ggplot() +
  # geom_rect(data = filter(model_means, parameter == "psight" & year == 2017),
  #           aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  # geom_rect(data = filter(model_means, parameter == "psight" & year == 2018),
  #           aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(model_means, parameter == "psight" & year == 2017),
             aes(yintercept=mean)) +
  geom_hline(data = filter(model_means, parameter == "psight" & year == 2018),
             aes(yintercept=mean), linetype = 2) +
  geom_errorbar(data = phitpc_results, aes(x=as.numeric(Date),
                                           ymin=psight_low, ymax=psight_high),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = phitpc_results, aes(x=as.numeric(Date), y=psight, linetype=Year),
            size=0.5) +
  geom_point(data = phitpc_results, aes(x=as.numeric(Date), y=psight, fill=Year),
             size=2, shape=21) +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylab("Resighting probability") +
  ylim(0,1) +
  theme_ajm () +
  theme(legend.position = c(0.2,0.85),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title.x = element_blank())

# flagged proportion

pflag_plot <- ggplot() +
  # geom_rect(data = filter(model_means, parameter == "pflag" & year == 2017),
  #           aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  # geom_rect(data = filter(model_means, parameter == "pflag" & year == 2018),
  #           aes(xmin=-Inf, xmax=Inf, ymin=low, ymax=high), fill = "grey50", alpha=0.3) +
  geom_hline(data = filter(model_means, parameter == "pflag" & year == 2017),
             aes(yintercept=mean)) +
  geom_hline(data = filter(model_means, parameter == "pflag" & year == 2018),
             aes(yintercept=mean), linetype = 2) +
  geom_errorbar(data = phitpc_results, aes(x=as.numeric(Date),
                                           ymin=pflag_low, ymax=pflag_high),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(data = phitpc_results, aes(x=as.numeric(Date), y=pflag, linetype=Year),
            size=0.5) +
  geom_point(data = phitpc_results, aes(x=as.numeric(Date), y=pflag, fill=Year),
             size=2, shape=21) +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  ylim(0,0.2) +
  ylab("Proportion flagged") +
  theme_ajm () +
  theme(legend.position = c(0.2,0.85),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title.x = element_blank())

# plot in grid

all_plots <- plot_grid(b_plot, phi_plot, psight_plot, pflag_plot, padult_plot, Nstop_plot,
                       labels = c("A", "B", "C", "D", "E", "F"),
                       nrow = 3, ncol = 2)

# all_plots <- arrangeGrob(b_plot, phi_plot, psight_plot, pflag_plot,
#                          padult_plot, Nstop_plot, nrow=3, ncol=2)

png(filename = paste0("figures/PlotModelResults_noCI.png"),
    width=6, height=7, units="in", res=600)

plot(all_plots)

dev.off()

# Histogram for stopover duration

sims_list_2018 <- readRDS("analysis-output/ALLCAMPS2018_superpop_phitpt_sims.list_randomeffect2021-01-29_PXDA500.rds")
sims_list_2018 <- sims_list_2018 %>%
  as.data.frame() %>% 
  select(zes.days) %>% 
  mutate(year = 2018)

sims_list_2017 <- readRDS("analysis-output/PISKLR2017_superpop_phitpt_sims.list_randomeffect_2021-01-31_fixlastentry_PXDA500.rds")
sims_list_2017 <- sims_list_2017 %>% 
  as.data.frame() %>% 
  select(zes.days) %>% 
  mutate(year = 2017)

zes_plot_2018 <- ggplot(sims_list_2018, aes(x=zes.days)) +
  geom_histogram(binwidth = 0.2, colour="black", fill="grey") +
  geom_vline(aes(xintercept=12),
             color="black", size=1, linetype="dashed") +
  # annotate("text", label = "c)", family = "Times", x = 5.3, y = 2100, size = 5) +
  # annotate("text", label = "2018", family = "Times", x = 15.5, y = 2100, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(5, 19), breaks = seq(5, 19, 2)) +
  scale_y_continuous(limits = c(0,3500), expand = c(0,0), breaks = seq(0, 3500, 500)) +
  xlab("Stopover duration (days)") +
  ylab("Frequency (MCMC iterations)")

zes_plot_2017 <- ggplot(sims_list_2017, aes(x=zes.days)) +
  geom_histogram(binwidth = 0.21, colour="black", fill="grey") +
  geom_vline(aes(xintercept=10.5),
             color="black", size=1, linetype="dashed") +
  # annotate("text", label = "a)", family = "Times", x = 5.3, y = 1400, size = 5) +
  # annotate("text", label = "2017", family = "Times", x = 15.5, y = 1400, size = 5) +
  theme_ajm() +
  theme(axis.title.x = element_text(size = 10)) +
  scale_x_continuous(limits = c(5, 19), breaks = seq(5, 19, 2)) +
  scale_y_continuous(limits = c(0,2200), expand = c(0,0), breaks = seq(0, 2200, 500)) +
  xlab("Stopover duration (days)") +
  ylab("Frequency (MCMC iterations)")

# save plots
stop_plots <- plot_grid(zes_plot_2017, zes_plot_2018, labels = c("A", "B"),
                        ncol = 2)

png(filename = paste0("figures/StopHistPlots.png"),
    width=6, height=3, units="in", res=600)

plot(stop_plots)

dev.off()


# parameter identifiability checks
sims.list <- readRDS("./analysis-output/PISKLR2017_superpop_phitpt_sims.list_randomeffect_2021-01-31_fixlastentry_PXDA500.rds")
sims.list <- as.data.frame(sims.list)

theme_set(theme_bw())

for (i in colnames(sims.list)){
  
  png(filename = paste0("analysis-output/parameter-identifiability-2017/",
                        i, "-", "check.png"),
      width = 4, height = 3, units = "in", res = 600)
  
  print(ggplot(sims.list, aes(sims.list[,i])) +
          geom_density() +
          geom_hline(yintercept = 1, linetype = "dashed") +
          xlab(i))
  
  dev.off()
  
}

sims.list <- readRDS("./analysis-output/ALLCAMPS2018_superpop_phitpt_sims.list_randomeffect2021-01-29_PXDA500.rds")
sims.list <- as.data.frame(sims.list)

theme_set(theme_bw())

for (i in colnames(sims.list)){
  
  png(filename = paste0("analysis-output/parameter-identifiability-2018/",
                        i, "-", "check.png"),
      width = 4, height = 3, units = "in", res = 600)
  
  print(ggplot(sims.list, aes(sims.list[,i])) +
          geom_density() +
          geom_hline(yintercept = 1, linetype = "dashed") +
          xlab(i))
  
  dev.off()
  
}
