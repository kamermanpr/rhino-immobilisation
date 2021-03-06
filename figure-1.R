############################################################
#                                                          #
#                       Plot 1: Boma                       #
#                                                          #
############################################################
# Load packages
library(cowplot)
library(dplyr)
library(ggplot2)

# Respiratory rate
plot_rr <- readr::read_rds('./data/RR_boma.rds') %>%
    # Recode supplement
    mutate(supplement = ifelse(supplement == 'oxygen',
                               yes = 'Oxygen',
                               no = 'No oxygen')) %>%
    # Get summary stats for plotting
    group_by(intervention, time_min, supplement) %>%
    summarise(y = mean(resp_rate),
              ymin = y - sd(resp_rate),
              ymax = y + sd(resp_rate)) %>%
    # Plot
    ggplot(data = .) +
    aes(x = time_min,
        y = y,
        ymin = ymin,
        ymax = ymax,
        shape = intervention,
        colour = intervention,
        linetype = intervention) +
    geom_line(position = position_dodge(width = 2)) +
    geom_point(size = 4,
               position = position_dodge(width = 2)) +
    geom_errorbar(linetype = 1,
                  position = position_dodge(width = 2)) +
    # Add normal values
    annotate(geom = 'rect',
             xmin = -Inf, xmax = Inf,
             ymin = 16, ymax = 23,
             fill = '#999999',
             size = 0,
             alpha = 0.2) +
    geom_vline(xintercept = 6, 
               linetype = 2) +
    scale_colour_manual(values = c('#000000', '#555555', '#999999'),
                        name = 'Intervention') +
    scale_linetype_manual(values = c(1, 2, 3),
                        name = 'Intervention') +
    scale_shape_manual(values = c(15, 16, 17),
                        name = 'Intervention') +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25)) +
    labs(x = 'Time after lateral recumbancy (min)',
         y = 'Breaths per min') +
    facet_grid(. ~ supplement) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.title.y = element_text(size = 18,
                                      margin = margin(r = 1.2, unit = 'lines')),
          axis.title.x = element_text(size = 18,
                                      margin = margin(t = 1, unit = 'lines')),
          axis.text = element_text(colour = '#000000',
                                   size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14))

# PO2
## Use the legend from plot_PO2 in the final stacked figure
plot_PO2 <- readr::read_rds('./data/PO2_boma.rds') %>%
    # Recode supplement
    mutate(supplement = ifelse(supplement == 'oxygen',
                               yes = 'Oxygen',
                               no = 'No oxygen')) %>%
    # Get summary stats for plotting
    group_by(intervention, time_min, supplement) %>%
    summarise(y = mean(PO2),
              ymin = y - sd(PO2),
              ymax = y + sd(PO2)) %>%
    # Plot
    ggplot(data = .) +
    aes(x = time_min,
        y = y,
        ymin = ymin,
        ymax = ymax,
        shape = intervention,
        colour = intervention,
        linetype = intervention) +
    geom_line(position = position_dodge(width = 2)) +
    geom_point(size = 4,
               position = position_dodge(width = 2)) +
    geom_errorbar(linetype = 1,
                  position = position_dodge(width = 2)) +
    # Add normal values
    annotate(geom = 'rect',
             xmin = -Inf, xmax = Inf,
             ymin = 90.2, ymax = 108.6,
             fill = '#999999',
             size = 0,
             alpha = 0.2) +
    geom_vline(xintercept = 6, 
               linetype = 2) +
    # Added extra linebreaks for legend position in final stacked figure
    scale_colour_manual(values = c('#000000', '#555555', '#999999'),
                        name = '\n\n\nIntervention') +
    scale_linetype_manual(values = c(1, 2, 3),
                          name = '\n\n\nIntervention') +
    scale_shape_manual(values = c(15, 16, 17),
                       name = '\n\n\nIntervention') +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25)) +
    labs(x = 'Time after lateral recumbancy (min)',
         y = expression(P[a]*O[2]~(mm~Hg))) +
    facet_grid(. ~ supplement) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18,
                                      margin = margin(t = 1, unit = 'lines')),
          axis.text = element_text(colour = '#000000',
                                   size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 14,
                                    colour = '#FFFFFF'),
          strip.background = element_rect(fill = '#000000'))

# PCO2
plot_PCO2 <- readr::read_rds('./data/PCO2_boma.rds') %>%
   # Recode supplement
    mutate(supplement = ifelse(supplement == 'oxygen',
                               yes = 'Oxygen',
                               no = 'No oxygen')) %>%
    # Get summary stats for plotting
    group_by(intervention, time_min, supplement) %>%
    summarise(y = mean(PCO2),
              ymin = y - sd(PCO2),
              ymax = y + sd(PCO2)) %>%
    # Plot
    ggplot(data = .) +
    aes(x = time_min,
        y = y,
        ymin = ymin,
        ymax = ymax,
        shape = intervention,
        colour = intervention,
        linetype = intervention) +
    geom_line(position = position_dodge(width = 2)) +
    geom_point(size = 4,
               position = position_dodge(width = 2)) +
    geom_errorbar(linetype = 1,
                  position = position_dodge(width = 2)) +
    # Add normal values
    annotate(geom = 'rect',
             xmin = -Inf, xmax = Inf,
             ymin = 44.4, ymax = 53.2,
             fill = '#999999',
             size = 0,
             alpha = 0.2) +
    geom_vline(xintercept = 6, 
               linetype = 2) +
    scale_colour_manual(values = c('#000000', '#555555', '#999999'),
                        name = 'Intervention') +
    scale_linetype_manual(values = c(1, 2, 3),
                          name = 'Intervention') +
    scale_shape_manual(values = c(15, 16, 17),
                       name = 'Intervention') +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25)) +
    labs(x = 'Time after lateral recumbancy (min)',
         y = expression(P[a]*CO[2]~(mm~Hg))) +
    facet_grid(. ~ supplement) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.title.y = element_text(size = 18, 
                                      margin = margin(r = 0.9, unit = 'lines')),
          axis.title.x = element_text(size = 18, 
                                      margin = margin(t = 1, unit = 'lines')),
          axis.text = element_text(colour = '#000000',
                                   size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14)) 

# Piece together the plots using cowplot::plot_grid
# (use margins and rel_height to align and size plots)
## Sub-plot 1 (PaO2)
sub_1 <- plot_grid(plot_PO2 + theme(legend.position = 'none',
                                axis.text.x = element_blank(),
                                axis.title.x = element_blank(),
                                plot.margin = unit(c(1, 0, 0, 1), 
                                                   units = 'lines')),
                   ncol = 1,
                   nrow = 1)

## Sub-plot 2 (add PaCO2)
sub_2 <- plot_grid(sub_1,
                   plot_PCO2 + theme(legend.position = 'none',
                                     axis.text.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     strip.background = element_blank(),
                                     strip.text = element_blank(),
                                     plot.margin = unit(c(1, 0, 0, 1.2), 
                                                        units = 'lines')),
                   axis = 'r',
                   align = 'hv',
                   nrow = 2,
                   ncol = 1,
                   scale = c(1, 1),
                   rel_heights = c(1, 0.87))

## Sub-plot 3 (add RR)
sub_3 <- plot_grid(sub_2,
                   plot_rr + theme(legend.position = 'none',
                                   strip.background = element_blank(),
                                   strip.text = element_blank(),
                                   plot.margin = unit(c(1, 1, 1, 1.4), 
                                                      units = 'lines')),
                   axis = 'r',
                   align = 'hv',
                   nrow = 2,
                   ncol = 1,
                   scale = c(1, 1),
                   rel_heights = c(1, 0.64))

## Get and add legend
legnd <- get_legend(plot_PO2)
plot <- plot_grid(sub_3,
                  legnd,
                  ncol = 2,
                  axis = 'top',
                  align = 'h',
                  rel_widths = c(1, 0.35))

## Save plot to square sheet
ggplot2::ggsave(filename = './figures/figure-1.pdf',
               plot = plot,
               height = 21,
               width = 21,
               units = 'cm')
