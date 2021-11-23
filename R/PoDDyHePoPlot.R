#' Description of PoDDyHePoPlot()
#' 
#' @title Plot Function Developed for PoDDy-Hepo Project
#'
#' @description PoDDyHePoPlot returns a plot.
#'
#' @details Enables plotting even there is a variable has been separated proportionally in the previous step.
#'
#' @param data A data frame from PoDDyHePoPool.
#' @param year The lastest year that existing data.
#' @param title The tile of the plot. It changes due to we will have different reponse variable.
#' @param sepvarlbl Lable of separated variable. The default is \code{NULL}. Accoring to dummy_cols, the separated variables will be in the name for example obe_0, obe_1, obe_2,
#' if we taking obe for example. To label, sepvarlbl = c("Normal Weight", "Over Weight", "Obesity"). As the levels are sorted, so please make sure that the labels are in right order. 
#'
#'
#' @return Returns a graph
#' @export
#' 
#' @import ggplot2


PoDDyHePoPlot <- function(data, year, title, sepvarlbl = NULL){
  
  # 
  if(nrow(data) != length(unique(data$Year)) * 2 ){
    
    # Levels for seperated variable
    levels <- sort(unique(data$Group))
    
    # Label seperated variable
    data$Group <- factor(data$Group, 
                         labels = sepvarlbl,
                         levels = levels)
    
    # Label Sex
    data$Sex <- factor(data$Sex,
                       levels = c(1, 2),
                       labels = c("Men", "Women"))
    
    # Plot
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, color = .data$Group) ) +
      geom_point(shape = 16, size = 2, position = position_dodge(width = 2.5)) +
      geom_point(data = data[data$Year > year, ], shape = 1, size = 1.5, position = position_dodge(width = 2.5)) +
      geom_line(size = 0.7, position = position_dodge(width = 2.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 0.7, position = position_dodge(width = 2.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      scale_color_manual(name = " ",
                         labels = sepvarlbl,
                         values = c("grey25", "red", "blue")) + 
      scale_y_continuous(name = "%",
                         limits = c(min(100 * data$CI_LOWER, 0), ceiling(max(100 * data$CI_UPPER) + 5)),
                         breaks = seq(0, ceiling(max(100 * data$CI_UPPER)) + 5, 10)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      ggtitle(title) +
      facet_wrap( ~ Sex) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5 ,margin = margin(10, 0, 20, 0), size = 18),
            axis.title.x = element_text(margin = margin(10, 0, 20, 0), size = 15),
            axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5),
            axis.title.y = element_text(margin = margin(0, 10, 0, 20), size = 15),
            axis.text.y = element_text(size = 11),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text.x = element_text(size = 14),
            strip.background = element_rect(fill = "grey90"),
            panel.border = element_rect(color = "black"),
            panel.spacing = unit(0.35, "lines"))
  } else{
    
    # Plot (not seperated variable)
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, group = .data$Sex, color = .data$Sex)) +
      geom_line(size = 1, position = position_dodge(width = 1.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 1, position = position_dodge(width = 1.5)) +
      geom_point(shape = 16, size = 2.5, show.legend = T, position = position_dodge(width = 1.5)) +
      geom_point(data = data[data$Year > year, ], shape = 21, size = 2.5, show.legend = T, position = position_dodge(width = 1.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      scale_color_manual(name = "",
                         labels = c("Men", "Women"),
                         values = c("blue", "red")) +
      scale_y_continuous(name = "%",
                         limits = c(min(100 * data$CI_LOWER, 0), ceiling(max(100 * data$CI_UPPER) + 5)),
                         breaks = seq(0, ceiling(max(100 * data$CI_UPPER)) + 5, 10)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      theme_bw() +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5 ,margin = margin(10, 0, 20, 0), size = 18),
            axis.title.x = element_text(margin = margin(10, 0, 20, 0), size = 15),
            axis.text.x = element_text(size = 11),
            axis.title.y = element_text(margin = margin(0, 10, 0, 20), size = 15),
            axis.text.y = element_text(size = 11),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      theme(aspect.ratio = 2/3)
  }
  
}
