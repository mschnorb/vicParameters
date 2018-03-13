set_veg_legend <- function(c){
  
  class_label <- c("N-TeSp.1",
                "N-TeSp.2",
                "N-TeSp.3",
                "N-TeSp.4",
                "N-SpTa",
                "BE-TrSr",
                "BD-TrSr",
                "BD-TeSp",
                "MF",
                "S-TrSr",
                "S-TeSp",
                "G-TrSr",
                "G-TeSp",
                "SLM-SpP",
                "GLM-SpP",
                "BaLM-SpP",
                "Wetland",
                "Crop",
                "Barren",
                "Urban",
                "Water",
                "Ice")
  
  class_color <- c("#00af00",
              "#007d00",
              "#004b00",
              "#002800",
              "#949c70",
              "#001900",
              "1fab05",
              "#148c3d",
              "#5c752b",
              "#b39e2b",
              "#b38a33",
              "#e8db5e.",
              "#e0cf8a",
              "#9c7554",
              "#bad48f",
              "#408a70",
              "#6ba38a",
              "#e6ad66",
              "#a8abad",
              "#db2126",
              "#4d70a3",
#              "#ddfffd")
              "#05FFFF")
  
  index <- match(c, seq(1:22))
  
  return(list(lbl=class_label[index], clr=class_color[index]))
  
}