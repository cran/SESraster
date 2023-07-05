## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----table1-data, echo=FALSE--------------------------------------------------
type <- c("Equiprobable", "Proportional", "Fixed")
typeabv <- c("E", "P", "F") #c("Equipr.", "Prop.", "Fixed")
typeinit <- c("E", "P", "F")
# Species - Sites
d <- data.frame("Rows" = c("Rows (Layer)", rep("", 2)),
                type=type,
                paste0(
                  paste("Spp occur.:", typeabv, 
                        "<br> Site rich.:", typeabv[1]),
                  "<br>", "SIM", c(1,7,2), ": ", typeinit, typeinit[1], 
                  "") ,
                paste0(
                  paste("Spp occur.:", typeabv, 
                        "<br> Site rich.:", typeabv[2]), 
                  "<br>", "SIM", c(6,8,4), ": ", typeinit, typeinit[2], 
                  "") ,
                paste0(
                  paste("Spp occur.:", typeabv, 
                        "<br> Site rich.:", typeabv[3]), 
                  "<br>", "SIM", c(3,5,9), ": ", typeinit, typeinit[3], 
                  "") )
colnames(d) <- c(" ", " ", paste0("", type))

## ----table1, echo=FALSE-------------------------------------------------------
require(kableExtra)

kable(d, dbooktabs = TRUE, align=c("r", "c", "c", "c", "c"), escape = F, 
      caption = "Nine null model algorithms for species co-occurrence analysis listed in Gotelli (2000). Algorithms marked in green are already implemented in SESraster") %>%
  collapse_rows() %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  add_header_above(c(" " = 2, "Cols (Cell)" = 3), background = "#F2F2F2") %>%
  row_spec(0, background = "#F2F2F2") %>% # , extra_css = c("border-bottom-style: none", "", "", "")
  column_spec(1:2, bold = TRUE, background="#F2F2F2", border_right = TRUE) %>%
  column_spec(3:4, border_right = TRUE) %>%
  column_spec(3, background=c("#B4EEB4")) %>%
  column_spec(4, background=c("#FFFAFA")) %>%
  column_spec(5, background=c("#B4EEB4"))


