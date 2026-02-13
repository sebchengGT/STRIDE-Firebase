# ui_parts/03_loading_overlay.R

ui_loading <- tags$div(
  id = "loading-overlay",
  style = "
   display: none;
   position: fixed;
   top: 0; left: 0;
   width: 100%; height: 100%;
   background-color: rgba(255, 255, 255, 0.6);
   backdrop-filter: blur(2px);
   z-index: 99999;
   text-align: center;
  ",
  tags$div(
    style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
             display: flex; flex-direction: column; align-items: center;",
    tags$img(src = "Stride_official_logo.png", height = "450px", style = "animation: paintingReveal 3s ease-out forwards;")
  ),
  tags$style(HTML("
    @keyframes paintingReveal {
      0% { 
        filter: blur(15px) grayscale(80%); 
        opacity: 0; 
        transform: scale(0.9); 
      }
      100% { 
        filter: blur(0) grayscale(0%); 
        opacity: 1; 
        transform: scale(1); 
      }
    }
  "))
)