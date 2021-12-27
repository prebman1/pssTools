#' Draw the consort diagram for PSS
#'
#' @param num_main_boxes Number of boxes down the middle
#' @param height_main_box Height of each main box
#' @param main_box_annotations Character vector for each annotation for the middle boxes.
#' @param right_side_box_locations Character vector with box numbers for where right side boxes should be located.
#' @param right_side_box_height Height of each right box.
#' @param right_side_box_annotations Character vector for each annotation for the right boxes.
#' @param font_size Font size
#' @param left_side_box_locations Character vector with box numbers for where left side boxes should be located.
#' @param left_side_box_height Height of each left box.
#' @param left_side_box_annotations Character vector for each annotation for the left boxes.
#' @param title Title of diagram
#' @param title_color Title color
#' @param title_size Title size
#' @param subtitle Subtitle of diagram
#' @param subtitle_color Subtitle color
#' @param subtitle_size Subtitle size
#'
#' @return
#' @export
#'
#' @examples
#' num_assessed <- sum(tmp_completed_forms$total_records)
#' num_consented <- sum(tmp_completed_forms$consent)
#' num_excluded <- 0
#' num_ineligible <- 0
#' num_refused_consent <- 0
#' num_baseline <- sum(tmp_completed_forms$baseline)
#' num_baseline_idi <- sum(tmp_completed_forms$baseline_idi)
#' num_baseline_idi_child <- sum(tmp_completed_forms$baseline_idi_child)
#' num_followup_idi <- 0
#' num_followup_idi_child <- 0
#' num_baseline_wdrn <- 0
#' num_baseline_ltfu <- 0
#' num_wave2 <- sum(tmp_completed_forms$wave2)
#' num_wave3 <- sum(tmp_completed_forms$wave3)
#' num_wave4 <- sum(tmp_completed_forms$wave4)
#'
#' annotations_center <- c(paste0(num_assessed, " participants have initial contact forms"),
#'                         paste0(num_consented, " participants consented"),
#'                         paste0(num_baseline, " participants completed baseline"),
#'                         paste0(num_wave2, " participants completed wave 2"),
#'                         paste0(num_wave3, " participants completed wave 3"))
#' annotations_left <- c(paste0(num_baseline_idi, " baseline idis\n", num_baseline_idi_child, " child baseline idis"),
#'                       paste0(num_followup_idi, " follow-up idis\n", num_followup_idi_child, " child follow-up idis"))
#' annotations_right <- c(paste0(num_ineligible, " participants ineligible\n", num_refused_consent, " refused consent"),
#'                        paste0(num_baseline_wdrn, " participants withdrawn\n", num_baseline_ltfu, " participants LTFU"),
#'                        paste0(num_baseline_wdrn, " participants withdrawn\n", num_baseline_ltfu, " participants LTFU"),
#'                        paste0(num_baseline_wdrn, " participants withdrawn\n", num_baseline_ltfu, " participants LTFU"))
#'
#' consort <- draw_consort(num_main_boxes = 5,
#'                         height_main_box = 10,
#'                         main_box_annotations = annotations_center,
#'                         right_side_box_locations = c("1,2", "2,3", "3,4", "4,5"),
#'                         right_side_box_height = 10,
#'                         right_side_box_annotations = annotations_right,
#'                         font_size = 3,
#'                         left_side_box_locations = c("3,4", "4,5"),
#'                         left_side_box_height = 10,
#'                         left_side_box_annotations = annotations_left,
#'                         title = "Project Safeschools", title_color = safeschools_colors[1], title_size = 4,
#'                         subtitle = paste0(site_name,"\n",format(Sys.Date(), '%B %d, %Y')), subtitle_color = safeschools_colors[2], subtitle_size = 3.5)
#'
#' consort
pss_draw_consort <- function(num_main_boxes = 5,
                             height_main_box = 10,
                             main_box_annotations = c(""),
                             right_side_box_locations = c(""),
                             right_side_box_height = 10,
                             right_side_box_annotations = c(""),
                             font_size = 2.5,
                             left_side_box_locations = c(""),
                             left_side_box_height = 10,
                             left_side_box_annotations = c(""),
                             title = "",
                             title_color = "black",
                             title_size = 7,
                             subtitle = "",
                             subtitle_color = "black",
                             subtitle_size = 5) {
  data <- tibble(x = 1:100, y = 1:100)
  consort <- data %>%
    ggplot(aes(x, y)) +
    scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
    scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
    theme_linedraw()

  space_remaining_main_box = 100 - num_main_boxes * height_main_box
  if (space_remaining_main_box < 0)
    errorCondition("Invalid main box dimensions")
  space_between_main_box <-
    space_remaining_main_box / (num_main_boxes - 1)

  curr_y_max <- 100
  curr_y_min <- 100 - height_main_box
  main_box_dims <-
    data.frame(box_num = numeric(),
               y_max = numeric(),
               y_min = numeric())

  #main boxes
  for (i in 1:num_main_boxes) {
    #set annotations to "" when null to avoid errors
    curr_box_annotation <- main_box_annotations[i]
    if (is.na(curr_box_annotation) | is.null(curr_box_annotation)) {
      curr_box_annotation <- ""
    }
    if (!grepl("\n", curr_box_annotation)) {
      #if the curr box annotation does not already have paragraph breaks, add paragraph breaks
      curr_box_annotation <-
        paste(strwrap(curr_box_annotation, width = 28), collapse = "\n")
    }

    if (i < num_main_boxes) {
      # add box and arrow for all except last box
      consort <- consort +
        #add main box
        geom_rect(
          xmin = 30,
          xmax = 68,
          ymin = curr_y_min,
          ymax = curr_y_max,
          color = 'black',
          fill = 'white',
          size = 0.25
        ) +
        #add label
        annotate(
          'text',
          x = 50,
          y = (mean(c(
            curr_y_min, curr_y_max
          ))),
          label = curr_box_annotation,
          size = font_size
        ) +
        #add connecting line
        geom_segment(
          x = 50,
          xend = 50,
          y = curr_y_min,
          yend = curr_y_min - space_between_main_box,
          size = 0.15,
          linejoin = "mitre",
          lineend = "butt",
          arrow = arrow(length = unit(1, "mm"), type = "closed")
        )
    } else {
      # add only box for last box
      consort <- consort +
        #add main box
        geom_rect(
          xmin = 30,
          xmax = 68,
          ymin = curr_y_min,
          ymax = curr_y_max,
          color = 'black',
          fill = 'white',
          size = 0.25
        ) +
        #add label
        annotate(
          'text',
          x = 50,
          y = (mean(c(
            curr_y_min, curr_y_max
          ))),
          label = curr_box_annotation,
          size = font_size
        )
    }

    #add box and y max and min to data frame
    main_box_dims[i, ] <- c(i, curr_y_max, curr_y_min)

    #update y max and min for next box
    curr_y_max <- curr_y_min - space_between_main_box
    curr_y_min <- curr_y_max - height_main_box

  }

  #right side boxes
  for (i in 1:length(right_side_box_locations)) {
    main_boxes <- strsplit(right_side_box_locations[i], ",")
    main_boxes <- main_boxes[[1]]
    selected_boxes <- main_box_dims[main_boxes, ]

    white_space <-
      (max(selected_boxes$y_max) - min(selected_boxes$y_min) - right_side_box_height) / 2

    right_box_y_max <- max(selected_boxes$y_max) - white_space
    right_box_y_min <- min(selected_boxes$y_min) + white_space

    curr_side_box_annotation <- right_side_box_annotations[i]
    if (is.na(curr_side_box_annotation) |
        is.null(curr_side_box_annotation)) {
      curr_side_box_annotation <- ""
    }
    if (!grepl("\n", curr_side_box_annotation)) {
      #if the curr box annotation does not already have paragraph breaks, add paragraph breaks
      curr_side_box_annotation <-
        paste(strwrap(curr_side_box_annotation, width = 22), collapse = "\n")
    }

    consort <- consort +
      #add main box
      geom_rect(
        xmin = 72,
        xmax = 100,
        ymin = right_box_y_min,
        ymax = right_box_y_max,
        color = 'black',
        fill = 'white',
        size = 0.25
      ) +
      geom_segment(
        x = 50,
        xend = 72,
        y = mean(c(right_box_y_max, right_box_y_min)),
        yend = mean(c(right_box_y_max, right_box_y_min)),
        size = 0.15,
        linejoin = "mitre",
        lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type = "closed")
      ) +
      #add label
      annotate(
        'text',
        x = 86,
        y = (mean(c(
          right_box_y_max, right_box_y_min
        ))),
        label = curr_side_box_annotation,
        size = font_size
      )

  }

  #left side boxes
  for (i in 1:length(left_side_box_locations)) {
    main_boxes <- strsplit(left_side_box_locations[i], ",")
    main_boxes <- main_boxes[[1]]
    selected_boxes <- main_box_dims[main_boxes, ]

    white_space <-
      (max(selected_boxes$y_max) - min(selected_boxes$y_min) - left_side_box_height) / 2

    left_box_y_max <- max(selected_boxes$y_max) - white_space
    left_box_y_min <- min(selected_boxes$y_min) + white_space

    curr_side_box_annotation <- left_side_box_annotations[i]
    if (is.na(curr_side_box_annotation) |
        is.null(curr_side_box_annotation)) {
      curr_side_box_annotation <- ""
    }
    if (!grepl("\n", curr_side_box_annotation)) {
      #if the curr box annotation does not already have paragraph breaks, add paragraph breaks
      curr_side_box_annotation <-
        paste(strwrap(curr_side_box_annotation, width = 22), collapse = "\n")
    }

    consort <- consort +
      #add main box
      geom_rect(
        xmin = 0,
        xmax = 25,
        ymin = left_box_y_min,
        ymax = left_box_y_max,
        color = 'black',
        fill = 'white',
        size = 0.25
      ) +
      geom_segment(
        x = 50,
        xend = 25,
        y = mean(c(left_box_y_max, left_box_y_min)),
        yend = mean(c(left_box_y_max, left_box_y_min)),
        size = 0.15,
        linejoin = "mitre",
        lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type = "closed")
      ) +
      #add label
      annotate(
        'text',
        x = 12.5,
        y = (mean(c(
          left_box_y_max, left_box_y_min
        ))),
        label = curr_side_box_annotation,
        size = font_size
      )

  }

  #add title
  consort <- consort +
    annotate(
      'text',
      x = 12.5,
      y = 94.5,
      label = title,
      size = title_size,
      color = title_color
    ) +
    annotate(
      'text',
      x = 12.5,
      y = 85,
      label = subtitle,
      size = subtitle_size,
      color = subtitle_color
    )

  #final formatting for consort diagram
  consort <-
    consort + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  return(consort)
}
