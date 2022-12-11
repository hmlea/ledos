source("ledos-core.R")

shinyServer(function(input, output, session){
  # gets the cartesian coordinates of the orbital
  coords = reactive({
    n = as.numeric(input$n_num)
    l = as.numeric(input$l_num)
    m = as.numeric(input$m_num)
    points = as.numeric(input$num_points)
    
    # calculate and plot the orbital
    cart_coords = sim_data(n, l, m, num_points=points)
  })
  
  # creates a plotly of the orbital
  fig = reactive({
    plot_data(coords())
  })
  
  # plots the orbital
  output$output_plotly = renderPlotly({
    # plot the reactive data
    fig()
  })
  
  # old plotting output function
  # output$output_plotly = renderPlotly({
    # # get inputs
    # n = as.numeric(input$n_num)
    # l = as.numeric(input$l_num)
    # m = as.numeric(input$m_num)
    # points = as.numeric(input$num_points)
    # # set the maximum wavefunction to cut off for the monte carlo simulation
    # max_psi = 0.0026
    # 
    # # calculate and plot the orbital
    # d = sim(n, l, m, max_psi, num_points=points, r_lim=35)
    # plot_data(data, n=n, l=l, m=m)
  # })
  
  # reactive watching all the inputs
  change_orb = reactive({
    list(input$n_num, input$l_num, input$m_num, input$num_points)
  })
  
  # update sidebar note with number of points and orbital name
  observeEvent(change_orb(), {
    # get quantum numbers and number of points from input
    n = as.numeric(input$n_num)
    l = as.numeric(input$l_num)
    m = as.numeric(input$m_num)
    points = as.numeric(input$num_points)
    # concatenate and display the message in italics
    output$details = renderUI({
      detail_msg = paste("<i>Viewing the",
                         get_html_name(n, l, m),
                         "orbital using ",
                         points,
                         "points</i>")
      HTML(detail_msg)
    })
  })
  
  # update l quantum num based on n quantum num
  observeEvent(input$n_num, {
    # get current and previous values
    cur_n = as.numeric(input$n_num)
    prev_l = as.numeric(input$l_num)
    
    # see if the value for m needs to be changed
    new_set = 0
    if(prev_l < cur_n) {
      new_set = prev_l
    }
    
    # update the selectInput
    updateSelectInput(session, "l_num", selected=new_set,
                      choices=seq(0, (cur_n-1)))
  })
  
  # update m quantum num based on l quantum num
  observeEvent(input$l_num, {
    # get current and previous values
    cur_l = as.numeric(input$l_num)
    prev_m = as.numeric(input$m_num)
    
    # see if the value for m needs to be changed
    new_set = 0
    if(abs(prev_m) <= cur_l) {
      new_set = prev_m
    }
    
    # update the selectInput
    updateSelectInput(session, "m_num", selected=new_set,
                      choices=seq(-cur_l, cur_l))
  })
  
  # exports the data
  output$export = downloadHandler(
    filename = function() {
      paste0("orb-data_", format(Sys.time(), "%m%d%Y-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(coords(), file)
      showNotification("Data exported successfully")
    }
  )

  # saves the image
  output$save = downloadHandler(
    filename = function() {
      paste0("orbital_", format(Sys.time(), "%m%d%Y-%H%M%S"), ".png")
    },
    content = function(file) {
      reticulate::py_run_string("import sys")
      save_image(fig(), file, scale=3)
      showNotification("Image saved successfully")
    }
  )
    
  # the about button in the top right hand corner
  observeEvent(input$about, {
    showModal(
      modalDialog(title = "About",
                  # h4("About"),
                  HTML("<h5>Help</h5>
                  <p style = 'margin-bottom: 10px;'>Leatherwood's Electron 
                  Density Orbital Simulator (<b>LEDOS</b>) allows the 
                  visualization of hydrogenic orbitals using the Schrödinger 
                  wave equation for the hydrogen atom using a Monte Carlo 
                  simulation. Using a Monte Carlo method allows us to simulate 
                  the position of thousands of electrons around a hydrogen 
                  nucleus. This lets us visualize the general shape of the 
                  orbital in a pointillist manner. Each \"point\" represents 
                  the position of an electron and the darker it is, the higher 
                  probability it has being found there in an atom.
                  <p style = 'margin-bottom: 20px;'>The controls on the left 
                  can be used to visualize orbitals with different quantum 
                  numbers. The quality can be changed by increasing or 
                  decreasing the number of points used to visualize the 
                  orbital. The data can then be exported as a CSV file or an 
                  image of the plot can be downloaded.</p>
                  <h5>Acknowledgements</h5>
                  <p style = 'margin-bottom: 20px;'>I would like to express my 
                  greatest appreciation to Alvin Q. Meng for sharing his 
                  knowledge on implementing the spherical harmonics and a Monte 
                  Carlo algorithm for the probability density of the 
                  wavefunction. I was truly inspired by his project, 
                  Evanescence, and his insight helped immensely in my own 
                  implementation. You should check out his project
                  <a href='https://al2me6.github.io/evanescence'>here</a></p>
                  <h5>Refrences</h5>
                  <ul>
                      <li>Meng 2022, <a href='https://al2me6.github.io/evanescence'>Evanescence</a> (<a href='https://github.com/al2me6/evanescence'>GitHub</a>)</li>
                      <li>Lobo <i>et al.</i> 2019, <a href='https://doi.org/10.1590/1806-9126-RBEF-2019-0073'>A smooth path to plot hydrogen atom via Monte Carlo method</a></li>
                      <li>DeBruyne 2003, <a href='https://faculty.washington.edu/seattle/physics441/441xxxindex.html'>Bra, Ket, Dirac, and all that...</a></li>
                      <li>Tully <i>et al.</i> 2013, <a href='https://doi.org/10.1021/ed300393s'>Interactive Web-Based Pointillist Visualization of Hydrogenic Orbitals Using Jmol</a></li>
                      <li>Blanco <i>et al.</i> 1997, <a href='https://doi.org/10.1016/S0166-1280(97)00185-1'>Evaluation of the rotation matrices in the basis of real spherical harmonics</a></li>
                  </ul>"),
                  footer=span(div(HTML("<p>Hayden Leatherwood 2022 - built in R with Shiny and Plotly</p>"),
                                  style="float:left;"),
                              div(HTML("<a href='https://github.com/hmlea/ledos'>Source</a>"),
                                  style="float:right;")),
                  easyClose=T))
  })
})

