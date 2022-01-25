- 👋 Hi, I’m @radupaunescu696
- 👀 I’m interested in R language
- 🌱 I’m currently learning R language
- 💞️ I’m looking to collaborate on R development
- 📫 How to reach me https://radupaunescu696.wixsite.com/radupaunescu696 

<!---
radupaunescu696/radupaunescu696 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
# I. Activarea pachetelor necesare:
Packages<-c("quantmod","plotly","shiny","tidyquant","dplyr","installr","tidyverse","knitr","DescTools","devtools",
            "formattable","data.table","fmsb","foreign","readxl","ggradar",
            "writexl","rio","ggplot2","pacman","lars","caret","rmarkdown",
            "shiny","plotly","reactable","reactablefmtr")
lapply(Packages, library, character.only = TRUE)

# Culori in R: http://WWW.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

## II. Incarcarea & exportarea unui tabel Excel:  
Tab<-read_excel("C:/Radu/Radar Chart Alaptare Dec si Ian.xlsx")

# Aranjarea coloanelor intr-un tabel se face cu ajutorul functiei select()
Tab<-select(Tab, Zi, Media,Ml_Dec,Ml_Ian, Obs)

## Modificarea coloanelor si adaugarea de noi valori Tabel$Nume_coloana[nr_rand]
Tab$Ml_Ian[23]=710
Tab$Ml_Ian[19:22]=c("710","850","790","680")
view(Tab)
Tab$Obs[22]<-c("introdus pastarnac")

# Exportarea unui tabel Excel: 
write_xlsx(Tab,"C:/Radu/Radar Chart Alaptare Dec si Ian.xlsx")

### III. Formatarea Tabelului cu functia reactable():

reactable(Tab,bordered = TRUE,striped = FALSE, 
          highlight = TRUE,filterable = TRUE,
          showPageSizeOptions =TRUE,
          selection = "multiple", onClick = "select",
          searchable = TRUE,
          
          #formatarea tabelului:
          theme = reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            cellPadding = "8px 12px",
            style = list(fontFamily ="Century"),
            searchInputStyle = list(width = "100%")),
       
           # formatarea coloanelor
          defaultColDef = colDef(
            style=highlight_min_max(Tab,
              min_font_color = "red",
              max_font_color = "white",
              min_highlighter = NULL,
              max_highlighter = "limegreen"
            ),
            header = function(value) gsub(".", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#f7f7f8")
          )
                    )

https://glin.github.io/reactable/articles/examples.html#conditional-styling-1    

#### IV. Radarchart
radarchart(rbind(Tab$Media, Tab$`Zi`, data.frame(t(Tab$Ml_Dec)),Tab$Ml_Ian),  
vlabels=Tab$`Zi`,
paxislabels = Tab$Media,
            
#setari radar:linia axului, nr. segmente;
axistype=5, seg=8.5, pty="*",
cglcol="dark grey", cglty=7, axislabcol="green", 
caxislabels=c("0","100","200","300","400","500","600","700","800"), 
cglwd=0.7,
            
#custom polygon
pcol=c("red","blue"),pfcol=c(rgb(0.9,0.55,0.5,0.5),rgb(0.1,0.2,0.2,0.2)) , plwd=1.5 , 
            
#custom labels
vlcex=0.85, calcex=0.65,palcex=1,
)

# Titlul Radar Chart:
windowsFonts(T=windowsFont("Lucida Calligraphy"))
title(main="Alaptare Petrus în Decembrie & Ianuarie",cex.main=2,family="T", col.main=124)

# legenda radarchart:
legend("left",
       legend=c("Ml_Dec / zi","Media din carte / zi","Ml_Ian / zi"),
       col=c("red","green","blue"),
       lty=7:8,
       lwd=2,
       pch=20:20:20,
       bty="n",
       cex=0.6,
       text.font=4,
       text.col ="black",
       #pentru centru: left cu inset=0.3
       inset=0.39,
       pt.cex=1.2,
       horiz=FALSE,
       merge=TRUE,
       trace = FALSE)

##### V. Grafic Interactiv:

# A.  Grafic interactiv 2 luni de tip lines:
library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# incarcarea tabelului excel:
df <- read_excel("C:/Radu/Petrus.xlsx")

# realizarea graficului:
fig <- df
fig <- fig %>%
  plot_ly(
    x = ~Zi, 
    y = ~Ml,
    split = ~Luna,
    # frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F),
    text = ~paste("Ziua: ", Zi, "<br>Ml:", df$Ml), 
    hoverinfo = 'text'
  )

fig <- fig %>% layout(
  xaxis = list(
    title = "Ziua",
    zeroline = F
  ),
  yaxis = list(
    title = "Mililitri",
    zeroline = F
  )
) 

fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = F
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)
fig


# B. Alaptare Petrus doar in Decembrie:
library(plotly)
library(quantmod)

df <- read_excel("C:/Radu/Radar Chart Alaptare Dec si Ian.xlsx")
df <- tail(df, 31)
df$ID <- seq.int(nrow(df))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~ID)
fig <- df %>% plot_ly(
  x = ~ID, 
  y = ~df$Ml_Dec, 
  frame = ~frame,
  type = 'scatter', 
  mode = 'lines', 
  fill = 'tozeroy', 
  fillcolor='aliceblue',
  line = list(color = 'cadetblue2'),
  text = ~paste("Day: ", ID, "<br>Ml:", df$Ml_Dec), 
  hoverinfo = 'text'
)
fig <- fig %>% layout(
  title = "ML Petrus Decembrie",
  yaxis = list(
    title = "Mililitri Dec", 
    range = c(0,1000), 
    zeroline = F,
    tickprefix = "ml "
  ),
  xaxis = list(
    title = "Zi", 
    range = c(0,31), 
    zeroline = F, 
    showgrid = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Zi "
  )
)

fig

# C. Alaptare Petrus doar in Ianuarie:
library(plotly)
library(quantmod)

df <- read_excel("C:/Radu/Radar Chart Alaptare Dec si Ian.xlsx")
df <- tail(df, 31)
df$ID <- seq.int(nrow(df))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~ID)
fig <- df %>% plot_ly(
  x = ~ID, 
  y = ~df$Ml_Ian, 
  frame = ~frame,
  type = 'scatter', 
  mode = 'lines', 
  fill = 'tozeroy', 
  fillcolor='aliceblue',
  line = list(color = 'cadetblue2'),
  text = ~paste("Day: ", ID, "<br>Ml:", df$Ml_Ian), 
  hoverinfo = 'text'
)
fig <- fig %>% layout(
  title = "ML Petrus Ianuarie",
  yaxis = list(
    title = "Mililitri Ian", 
    range = c(0,1000), 
    zeroline = F,
    tickprefix = "ml "
  ),
  xaxis = list(
    title = "Zi", 
    range = c(0,31), 
    zeroline = F, 
    showgrid = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Zi "
  )
)

fig


