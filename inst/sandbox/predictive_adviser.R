library(flightdeck)
library(htmltools)


message = "This feature has `95% fitness`, but `0% believability`. The low believability is driven by a bivariate importance weight of 1, which indicates that it is too good a predictor to be true.

Please answer the following questions to help decide how
we should handle this feature:

- Is this feature available prior to observing the target variable?
- Is this feature constructed from the target variable based on some formula?
- Is there any data leakage from the target variable?
"

message_data = list(
  text = "This feature has `95% fitness`, but `0% believability`. The low believability is driven by a bivariate importance weight of 1, which indicates that it is too good a predictor to be true. Please answer the following questions to help decide how
  we should handle this feature:",
  questions = list(
    "Is this feature available prior to observing the target variable?",
    "Is this feature constructed from the target variable based on some formula?"
  )
)

fdCheckbox <- function(x){
  tags$div(class = 'checkbox',
    tags$label(tags$input(type = 'checkbox'), x)
  )
}

message <- tags$div(
  tags$p(class = 'lead', HTML(commonmark::markdown_html(message_data$main))),
  tagList(lapply(message_data$questions, fdCheckbox))
)

d <- read.csv('datadoctor.csv')

# Actions:
# Inspect /Fix /Drop /None
#

d1 <- fdSimpleTable(d)

d$Fitness <- d$Fitness*100
d$Believability <- d$Believability*100
d$Action <- "<span class='label bg-green'>Keep</span>"
d$Action = ifelse(d$Fitness == 0 | (!is.na(d$Percentage.Uniques) & d$Percentage.Uniques == 100),
  "<span class='label bg-orange modal-click'>Inspect</span>",
  "<span class='label bg-green'>Keep</span>"
)
d <- cbind(
  Column = d$Column,
  Action = d$Action,
  d[,!(names(d) %in% c("Column", "Action"))]
)

library(DT)
table1 <- datatable(d,
  rownames = FALSE,
  extensions = c("Buttons", "Responsive"),
  options = list(),
  style = "bootstrap",
  width = "100%",
  height = if (NROW(d) > 10) 550 else NULL,
  class = c("stripe", "hover", "cell-border"),
  escape = FALSE
) %>%
  formatStyle("Percentage.Missing",
    background = styleColorBar(c(0, 100), 'steelblue'),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = "transparent"
  ) %>%
  formatStyle("Percentage.Uniques",
   background = styleColorBar(c(0, 100), 'steelblue'),
   backgroundSize = "98% 88%",
   backgroundRepeat = "no-repeat",
   backgroundPosition = "center",
   color = "transparent"
  )


# fdBoard(
#   fdHeader(title = 'DataDoctor'),
#   fdSidebar(),
#   fdBody(
#     fdRowBox(width = 12, table1),
#     tags$script(HTML(sprintf("
#       $(function(){
#        $('.modal-click').on('click', function(){
#          bootbox.alert(%s)
#        })
#       })
#      ",
#      jsonlite::toJSON(list(
#         title = as.character(
#           span('Explanation', span(class = 'label label-warning', 'INSPECT'))
#         ),
#         message = as.character(message)
#     ), auto_unbox = TRUE)
#     )))
#   )
# )

scr1 <- tags$script(HTML(
  paste(readLines('trigger-modal.js', warn = F), collapse ='\n'))
)

fdBoard(
  fdHeader(title = 'DataDoctor'),
  fdSidebar(),
  fdBody(
    fdRowBox(width = 12, table1), scr1
  )
)
