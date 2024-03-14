# Caroline Cutter and Frank Bautista
# FINAL PROGRAMMING PROJECT 
# Friday, February 4, 2022

# -------------------------------- CODE -----------------------------------

# IMPORTS
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(htmltools)
library(tidytext)
library(grid)
library(gridtext)
library(gridExtra)
library(shinyalert)
library(markdown) 
library(shinythemes)




# DATA 
experiment1 <- read_csv("18FB.csv")

#DATA COMPARISON

#SMF data
spikessmf <- experiment1%>%
  filter(cellID == "SMF")%>%
  ggplot(aes(x = spikes, y = "Cell 1"))+
  geom_tile(width = 2) +
  labs(
    title = "Spike Times SMF Trial",
    x = "Spike Times (ms)",
    y = "Neuron 1"
  )+
  theme(text = element_text(size = 23))

SMF3 <- experiment1 %>%
  filter(cellID == "SMF")

SMFx3 <- cut(SMF3$spikes, seq(-950,880, by = 91), dig.lab = 5)
SMF3 <- data.frame(table(SMFx3))
SMF3 <- rename(SMF3, `Firing Rate` = Freq)
SMF3 <- rename(SMF3, `Time (ms)` = SMFx3)


firingsmf <- SMF3%>%
  ggplot(aes(x = `Time (ms)`, y = `Firing Rate`))+
  geom_col(fill = "blue", color = "dark blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Firing Rate SMF Trial",
  )+
  theme(text = element_text(size = 23))

ISIsmf <- experiment1%>%
  filter(cellID == "SMF")%>%
  summarise(time.difference = diff(spikes))%>%
  ggplot(aes(x = time.difference)) + 
  geom_histogram(color = "dark blue", fill = "blue")+
  xlab("Time difference (ms)")+
  labs (
    title = "Inter-spike Interval SMF Trial",
    y = "Count"
  )+
  theme(text = element_text(size = 23))

#FMS Data
spikesfms <- experiment1%>%
  filter(cellID == "FMS")%>%
  ggplot(aes(x = spikes, y = "Cell 1"))+
  geom_tile(width = 2) +
  labs(
    title = "Spike Times FMS Trial",
    x = "Spike Times (ms)",
    y = "Neuron 1"
  )+
  theme(text = element_text(size = 23))

FMS3 <- experiment1 %>%
  filter(cellID == "FMS")

FMSx3 <- cut(FMS3$spikes, seq(-1080,800, by = 94), dig.lab = 5)
FMS3 <- data.frame(table(FMSx3))
FMS3 <- rename(FMS3, `Firing Rate` = Freq)
FMS3 <- rename(FMS3, `Time (ms)` = FMSx3)


firingfms <- FMS3%>%
  ggplot(aes(x = `Time (ms)`, y = `Firing Rate`))+
  geom_col(fill = "blue", color = "dark blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Firing Rate FMS Trial",
  )+
  theme(text = element_text(size = 23))

ISIfms <- experiment1%>%
  filter(cellID == "FMS")%>%
  summarise(time.difference = diff(spikes))%>%
  ggplot(aes(x = time.difference)) + 
  geom_histogram(color = "dark blue", fill = "blue")+
  xlab("Time difference (ms)")+
  labs (
    title = "Inter-spike Interval FMS Trial",
    y = "Count"
  )+
  theme(text = element_text(size = 23))






# USER INTERFACE 
ui <- fluidPage(


fluidPage(theme = shinytheme("darkly"),
  # TITLE 

  titlePanel(
    h1(strong("The Somatosensory System"), align = "center", style = "font-size:65px"),
  ),
  
  titlePanel(
    h3("Frank Bautista & Caroline Cutter", align = "center"),
  ),
  
  
  # MAIN IMAGE 
  div(img(src = "somatosensory.png", height="25%", width="25%"),
      style = "text-align: center;"),
  
  

  
         
  titlePanel(
    h2("Click the tabs for more information!", align = "center")
  ),


  
  
  
  # NAVBAR 
  

  # TAB PANELS 
  tabsetPanel(type = "tabs", 
              
              
              # RECEPTORS AND THE SKIN 
              tabPanel(h4(strong("Receptors & the Skin")),
                       
                       # SKIN STRUCTURE
                       h2(strong("STRUCTURE OF THE SKIN"), align = "center" ),
                       div(img(src = "skin_structure.png", height="35%", width="30%"),
                           style = "text-align: center;"),
                       
                       h3("The skin is a protective organ that surrounds the body and holds
                            the receptors that allows sensations to be turned into conscious thought
                            and action. There are many parts to the skin: ", align = "center"),
                       tags$ul(
                         tags$li(tags$b("Epidermis: ", style = "font-size:25px"), 
                                 "the outermost part of the skin that forms a protective layer to the body", style = "font-size:25px"),
                         tags$li(tags$b("Dermis: ", style = "font-size:25px"), 
                                 "the layer beneath the epidermis that has connective tissues, hair follicles, and sweat glands", style = "font-size:25px"),
                         tags$li(tags$b("Hypodermis: ", style = "font-size:25px"), 
                                 "the deep tissue that contains fat and blood vessels", style = "font-size:25px"),
                         tags$li(tags$b("Glabrous Skin: ", style = "font-size:25px"), 
                                 "smooth skin that lacks hair and hair follicles", style = "font-size:25px"),
                         tags$li(tags$b("Hairy Skin: ", style = "font-size:25px"), 
                                 "skin that has hair covering the surface. The hair acts as a sensor because
                                   the follicles are innervated by free nerve endings that respond
                                   to deformations of the hair", style = "font-size:25px"),
                       ),
                       
                       
                       
                       # MECHANORECEPTION 
                       h2(strong("MECHANORECPTION"), align = "center" ),
                       div(img(src = "mechanoreceptors.png", height="25%", width="25%"),
                           style = "text-align: center;"),
                       h3("Mechanoreception is the detection of physical stimuli such as
                         touch, vibration, and textures. There are four main types of mechanoreceptors:", align = "center"),
                       tags$ul(
                         tags$li(tags$b("Merkel's Disks: ", style = "font-size:25px"), 
                                 "respond to light touch and are slow 
                                   adapting sensors which generate a more sustained response
                                   during a long stimulus.", style = "font-size:25px"),
                         tags$li(tags$b("Meissner’s Corpuscles: ", style = "font-size:25px"), 
                                 "respond to touch and low-frequency vibration and
                                   rapidly adapt to stimulus by responding quickly then
                                   stoppig firing as the stimulus continues", style = "font-size:25px"),
                         tags$li(tags$b("Ruffini Endings: ", style = "font-size:25px"), 
                                 "detect stretching and deformation within the joints and
                                   rapidly adapt to stimulus by responding quickly then
                                   stoppig firing as the stimulus continues", style = "font-size:25px"),
                         tags$li(tags$b("Pacinian Corpuscles: ", style = "font-size:25px"), 
                                 "detect pressure and high-frequency vibrations and slowly 
                                   adapt to generate a longer response during continued stimuli", style = "font-size:25px")
                       ),
                       
                       
                       # NOICEPTION 
                       h2(strong("NOICEPTION"), align = "center" ),
                       div(img(src = "pain_receptors.png", height="30%", width="30%"),
                           style = "text-align: center;"),
                       h3("Nociception is the detection of painful stimuli and itching.", align = "center"),
                       tags$ul(
                         tags$li(tags$b("Polymodal Nociceptors: ", style = "font-size:25px"), 
                                 "respond to most painful stimuli including mechanical, thermal and chemical.", style = "font-size:25px"),
                         tags$li(tags$b("Mechanical Nociceptors: ", style = "font-size:25px"), 
                                 "selectively responds to strong pressure, such as stubbing a toe", style = "font-size:25px"),
                         tags$li(tags$b("Thermal Nociceptors: ", style = "font-size:25px"), 
                                 "responds to burning (above 43° C) and extreme cold (below 0° C)", style = "font-size:25px"),
                         tags$li(tags$b("Chemical Nociceptors: ", style = "font-size:25px"), 
                                 "responds to noxious chemcials or ligands that are generated after injury or stress ", style = "font-size:25px")
                       ),
                       
                       
                       
                       
                       # THERMORECEPTION
                       h2(strong("THERMORECEPTION"), align = "center" ),
                       div(img(src = "temp_receptors.png", height="30%", width="30%"),
                           style = "text-align: center;"),
                       h3("Thermoreception is the detection of outside temperature.
                            Although all cells are sensitive to temperature due to their chemcial reactions,
                            there are sepcialized receptors for hot and cold sensations. The cold sensors
                            respond to temperature drops, and the hot sensors respond to temperature increases. 
                            Here is an example spike train for each of these sesnors in response to temperature changes:", align = "center"),
                       
                       
                       
                       # PROPRIOCEPTION 
                       h2(strong("PROPRIOCEPTION"), align = "center" ),
                       div(img(src = "proprioception.png", height="30%", width="30%"),
                           style = "text-align: center;"),
                       
                       h3("Proprioception is known as the 'sixth sense' which is responsible
                         for sensing body position, movement and posture. It does not always have to involve conscious thought. 
                            It involves proprioceptors in muscles, tendons, joints and ligaments that monitor changes
                            throughout the body and gives feedback to the brain on how to adjust the body. Proprioception 
                            can be conscious or unconscious - for example adjusting balance in the body is typically
                            unconscious while decision making is typically conscious.", align = "center"),
                       
                       br(),
                       br()
                       
                       
                       
              ),
              
              
              # SPINAL TAB 
              tabPanel(h4(strong("Spinal Structures")), 
                       
                       h2(strong("SPINAL CORD ORGANIZATION"), align = "center" ),
                       div(img(src = "spinal_full.png", height="20%", width="20%"),
                           style = "text-align: center;"), 
                       
                       tags$ul(
                         tags$li("The spinal cord consists of 30 segments divided into four sections: Cervical (C 1-8),
                            Thoracic (T 1-12), Lumbar (L 1-5), Sacracl (S 1-5)", style = "font-size:25px"),
                         tags$li( "The spinal cord is organized in a ", style = "font-size:25px", tags$b("dermatome", style = "font-size:25px"),
                                  " which means there are areas of the skin which derive from specific spinal nerve roots"),
                         tags$li("When an area of the spinal cord is damaged, the pathway from that sight in the spinal cord 
                                   and down is damaged because the pathway to the brain is cut off. In other words, when the dorsal 
                                   is cut, the corresponding dermatome loses all sensation perception.", style = "font-size:25px"),
                       ),
                       
                       
                       h2(strong("CROSS SECTION "), align = "center" ),
                       div(img(src = "spinal_cross.png", height="20%", width="20%"),
                           style = "text-align: center;"), 
                       
                       
                       tags$ul(
                         tags$li("The inner spinal cord consists of gray matter (the neuronal cell bodies) surrounded by white matter tracts 
                                  (the axons), which form tracts called columns that signals can travel through", style = "font-size:25px"),
                         tags$li( "The cross section of the spinal cord can be divided into the dorsal (back of human) horn, 
                                   intermediate zone, and the ventral (front of human)", style = "font-size:25px"),
                         tags$li("Sensory neurons move information up the spine to the brain through the dorsal columns", style = "font-size:25px"),
                       ),
                       br(),
                       br()
              ),
              
              
              # TOUCH PATHWAYS
              tabPanel(h4(strong("Touch Pathways")),
                       
                       h2(strong("OVERVIEW"), align = "center" ),
                       
                       br(),
                       
                       div(img(src = "touch_pathway.png", height="20%", width="20%"),
                           style = "text-align: center;"), 
                       
                       br(),
                       
                       h3("The lemniscal pathway is responsible for bringing touch sensations
                            from receptors throughout the body to be processed throughout the brain. The
                            electrical impulses pass through a series of structures before reaching the 
                            final destination in the somatosensory cortex: ", align = "center"),
                       tags$ul(
                         tags$li(tags$b("The Dorsal Column-Medial Lemniscal Pathway: ", style = "font-size:25px"), 
                                 "touch sensations pass through this pathway to the primary somatosensory cortex (S1).
                                   Here are the steps below...", style = "font-size:25px"),
                         tags$li(tags$b("First Order Neurons: ", style = "font-size:25px"), 
                                 "touch reception first starts in first order neurons which carry 
                                   information from the touch receptors to the dorsal root galgia of the 
                                   spinal cord.", style = "font-size:25px"),
                         tags$li(tags$b("Second Order Neurons: ", style = "font-size:25px"), 
                                 "then the signal is passed to the second order neurons in the 
                                   spinal cord and travel up the dorsal column to the brainstem and then
                                   decussate (cross over) before entering the brain. This decussation
                                   causes sensations felt on the right side of the body to be processed
                                   in the left side of the brain and vice versa.", style = "font-size:25px"),
                         tags$li(tags$b("Third Order Neurons: ", style = "font-size:25px"), 
                                 "the electrical signal then passes to third order neurons which pass 
                                   through the thalamus and relay information to the primary somatosensory cortex (S1)
                                   where the impulses can turn into conscious sensations and responses.", style = "font-size:25px")
                       ),
                       br(),
                       br()
                       
              ),
              
              
              
              # PAIN PATHWAYS         
              
              tabPanel(h4(strong("Pain Pathways")),
                       h2(strong("OVERVIEW"), align = "center" ),
                       br(),
                       div(img(src = "pain_pathway.png", height="20%", width="20%"),
                           style = "text-align: center;"),
                       br(),
                       
                       h3("Pain sensations take a different path than typical touch sensations.
                         Instead of traveling up the Leminiscal Pathway, pain impulses travel up the
                            Spinothalamic Pain Pathway. The spinothalamic path is similar to the lemiiscal pathway with a 
                                   few key differences.", align = "center"),
                       
                       tags$ul(
                         
                         tags$li("Touch is characterized by special structures int he skin while the pain
                                   pathway only has a few free nerve endings", style = "font-size:25px"),
                         tags$li("The spinnothalamic pain pathway immediately decussates in the spinal cord
                                   rather than decussating in the brainstem", style = "font-size:25px"),
                         tags$li("The pain is sent through the spinal cord, brainstem and thalamus to the 
                                   primary sensory cortex (S1) where it can be processed and trigger the autonomic
                                   nervous system to react and find a way to stop the pain", style = "font-size:25px"),
                         tags$li("The body has a natural opioid system that reduces pain once it is processed
                                    by the brain. This works as natural endorphins bind to pain reducing receptors", style = "font-size:25px"),
                         tags$li("There are two main types of erve fibers that carry pain: A-Delta, which carry 
                                   sharp, localize pain to the brain and are covered in myelinated fibers, and C-Fibers,
                                   which have nerve endings that are spread over larger areas.", style = "font-size:25px"),
                         tags$li("Pain is important for survival because it let's the body know when 
                                   it is in posssible danger so it can respond. Due to it's imporance,
                                   almost every part of the brain is involved in processing pain signals.", style = "font-size:25px")
                       ),
                       br(),
                       br()
              ),
              
              
              
              
              
              
              # SOMATOSENSORY CORTEX      
              tabPanel(h4(strong("Somatosensory Cortex")),
                       h2(strong("OVERVIEW"), align = "center" ),
                       br(),
                       div(img(src = "cortex_structure.png", height="20%", width="20%"),
                           style = "text-align: center;"),
                       
                       h3("The somatosensory cortex is where touch senations are turned into
                     perceptions. Decussation of the signals during travel up to the brain
                     cause the left side of the body sensations to be processed in the right
                     hemisphere and vice versa. ", align = "center"),
                       
                       
                       
                       br(),
                       
                       h2(strong("Sensory"), align = "center" ),
                       
                       div(img(src = "somato_map.png", height="20%", width="20%"),
                           style = "text-align: center;"),
                       
                       h3("Each part of the cortex processes singals from differet parts of the body.
                     Due to this organization, researchers were able to make a 
                     cortical map that maps body parts to their areas of the brain.
                     However, the body parts do not correleate with the size of the 
                     cortex, the size depends on how many touch receptors that body part has.
                     So, body parts like the lips and hands have the largest area of the 
                     cortex while the legs have a smaller area. Sometimes during 
                     injuries this cortical map can be rearranged, and this causes
                     the sensation of phantom limb in amputee patients.", align = "center"),
                       
                       br(),
                       br()
              ),
              
              
              # **************************************************************************               
              
              
              # DATA EXPLORATION 
              navbarMenu(h4(strong("Data Exploration")),
                         
                         tabPanel(h4(strong("Paper Background")), 
                                  
                                  
                                  br(), br(),
                                  
                                  h2(strong('OVERVIEW OF: "Short Time-Scale Sensory Coding in S1 during 
                                       Discrimination of Whisker Vibrotactile Sequences"' ), align = "center"),
                                  
                                  
                                  h4('FULL PAPER: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5004814/', align = "center"),
                                  
                                  
                                  
                                  
                                  
                                  br(),
                                  
                                  
                                  h3(strong("AUTHORS"), align = "center" ), 
                                  
                                  
                                  tags$ul(
                                    tags$li(tags$b("Leah M. McGuire", style = "font-size:25px"), 
                                            "Leah completed a PhD and Postdoctoral fellowship in computational 
                                       neuroscience at University of California, San Francisco, and at 
                                       University of California, Berkeley. She studied neural encoding 
                                       and integration of sensory signals. She was also a lead data 
                                       scientist for LinkedIn. She is currently a machine learning 
                                       engineer as well.", style = "font-size:25px"),
                                    
                                    tags$li(tags$b("Gregory Telian:", style = "font-size:25px"), 
                                            "There is not much information publicly posted about this author, 
                                       but he is a postdoctoral scholar in Psychiatry and received his PhD 
                                       from University of California Berkeley. He now studies how hippocampal 
                                       networks represent and distribute information across its network of 
                                       interconnected brain regions.", style = "font-size:25px"),
                                    tags$li(tags$b("Keven J. Laboy-Juárez::", style = "font-size:25px"), 
                                            "Keven completed his PhD at university of California Berkeley and 
                                       studied neurons in the somatosensory cortex and how they represent 
                                       tactile and vibrotactile sequences. He is now a postdoc in a lab that 
                                       studies how subcortical structures mediate learning and motor sequences.",
                                            style = "font-size:25px")
                                    
                                  ),
                                  br(), br(), br(),
                                  
                                  # INTRODUCTION       
                                  h3(strong("INTRODUCTION"), align = "center" ), 
                                  
                                  br(), br(), 
                                  
                                  div(img(src = "rat_whisker.png", height="20%", width="20%"),
                                      style = "text-align: center;"), 
                                  
                                  br(), br(),
                                  
                                  
                                  tags$ul(
                                    tags$li("Sensory input creates a wide array of temporal patters, but how the brain processes this information is not well understood", style = "font-size:25px"),
                                    tags$li("The whiskers of rodents process tactile information which creates different firing patterns in the brain", style = "font-size:25px"),
                                    tags$li("Rats can discriminate vibrotactile sequences that the whisker senses and from this can involve behavioral integrations that calculates mean whisker speed", style = "font-size:25px"),
                                    tags$li("The authors recorded neural activity in the primary somatosensory cortex while rats discriminated different vibrotactile sequences applied to their whiskers", style = "font-size:25px"),
                                    tags$li("They found that neurons in the primary somatosensory cortex encoded whisker sensory information at very fast time scales (<20 ms)", style = "font-size:25px"),
                                    tags$li("They also found a subset of neurons that strongly encoded the rat’s behavioral choices in response to the tactile information", style = "font-size:25px"),
                                    tags$li("They concluded that the “primary sensory cortex represents immediate sensory input, suggesting that temporal integration occurs in downstream brain areas”", style = "font-size:25px")
                                    
                                  ), 
                                  
                                  
                                  br(), br(), br(),
                                  
                                  
                                  
                                  
                                  # METHODS                  
                                  h3(strong("MEHTODS & EXPERIMENT"), align = "center" ), 
                                  
                                  h4(strong("EXPERIMENT")), 
                                  
                                  tags$ul(
                                    tags$li("Neuronal firing data was collected from female Long-Evan rats > 3 months of age", style = "font-size:25px"),
                                    tags$li("Rat whiskers were trimmed to 15mm and placed on a (2cm x 2cm) “stimulus panel” where the data was recorded from", style = "font-size:25px"),
                                    tags$li("The rats were trained to discriminate whisker vibrotacticle sequences with a range of different rise/fall velocities", style = "font-size:25px"),
                                    tags$li("The rise/fall stimulus was classified as S (slow), M (medium) and F(fast) ", style = "font-size:25px"),
                                    tags$li("The rats were placed in a housing area and their right whiskers were stimulated by different patterns of rise/fall velocities", style = "font-size:25px"),
                                    tags$li("The four main stimulus tested were all elicited in a series of three, including SSS, SMF, FMS, and FFF (the mean speed was taken into account with these sequences (34 ms between end of one pulse and start of another)", style = "font-size:25px"),
                                    tags$li("When a certain sequence was given the rat was trained to go to a certain water port", style = "font-size:25px"),
                                    tags$li("The drink port dispensed a carefully measured “water reward”. The water reward was roughly .05 - .1 mL water", style = "font-size:25px"),
                                    tags$li("If the rats chose the wrong port then they would receive a 4 - 6 second timeout and no water", style = "font-size:25px"),
                                    tags$li("The researchers ensured that the whiskers constantly were on the whisker stimulus panel as a constant, and it was their right whisker set that felt stimulus", style = "font-size:25px"),
                                    tags$li("The main goal of this experiment was to gather behavioral feedback (drink port choosing) and neuron stimulus data such 
                                      as spike trains from 5 rats' somatosensory cortex whose “fixed- panel control task showed substantial task performance 
                                      in the absence of panel movement” through four tetrodes", style = "font-size:25px"),
                                  ),
                                  
                                  br(), br(),
                                  h4(strong("DATA FORMAT")), 
                                  
                                  tags$ul(
                                    tags$li("The data was collected through extracellular spike recordings in the primary somatosensory 
                                        cortex (S1) whisker area of rats when they were performing whisker discrimination tasks", 
                                            style = "font-size:25px"),
                                    tags$li("The recordings are the spike times and they are clustered into single and multiunit arrays after sorting the data", style = "font-size:25px"),
                                    tags$li("The stimulus of the data was based on whisker impulses of different speed, labeled by fast (F), Medium (M), and slow (S) velocity.", style = "font-size:25px"),
                                    tags$li("The researchers tried different variations of these impulses as stimuli when the data was collected", style = "font-size:25px"),
                                    tags$li("Each trial has one stimulus. There were 5 rats and over 8- recording sessions. The data is formatted 
                                        in a 80 x 1 array in the code, where each recording session is a row of the structure", style = "font-size:25px"),
                                    tags$li("All the neurons in one session were recorded simultaneously", style = "font-size:25px"),
                                    tags$li("The spike times were recorded in columns, each column of the trial location has data from a different tetrode", style = "font-size:25px"),
                                  ),
                                  
                                  br(), br(), br(),
                                  
                                  
                                  
                                  
                                  # RESULTS        
                                  h3(strong("RESULTS"), align = "center" ), 
                                  
                                  
                                  h3(strong("Here is an example result from the paper. If you
                             would like to see the full results, please click the link for the paper above.
                             If you would like to actively explore some of the data, please click on the 'Data' tab
                             under 'Data Exploration'"), align = "center" ),
                                  
                                  br(), br(),
                                  
                                  h4(strong("FIGURE 5")), 
                                  
                                  div(img(src = "fig5.png", height="40%", width="40%"),
                                      style = "text-align: center;"), 
                                  
                                  
                                  tags$ul(
                                    tags$li("This shows us not only a raster plot of spike train data,
                               but it also shows us a line graph that helps us
                                       understand the change of frequency compared to a stimulus. ", 
                                            style = "font-size:25px"),
                                    tags$li("Figure 5 shows a spike raster plot and population peri-stimulus time 
                                       histograms (PSTHs) which are histograms that show the rate and timing of 
                                       neuronal spikes in relation to a stimulus being elicited", 
                                            style = "font-size:25px"),
                                    tags$li("The blue vertical lines in this instance represent the stimulus, the black 
                                       dots represent the spikes, and the multicolored graph lines represent the
                                       firing rate.", style = "font-size:25px"),
                                    tags$li("The researchers split this up to be across multiple trials for the FFF, FMS, SMF and SSS firing patterns.", 
                                            style = "font-size:25px"),
                                    tags$li("The researchers observed from this graph that individual units showed a phasic response to individual impulses/stimuli.", 
                                            style = "font-size:25px"),
                                    tags$li("They also found that the units showed an increasing firing rate when the stimulus was being recorded.", 
                                            style = "font-size:25px"),
                                    tags$li("What the authors learned from this information is that the S1 neurons rarely spike with individual whisker deflections.", 
                                            style = "font-size:25px")
                                    
                                  ),
                                  
                                  
                                  
                                  br(), br(), br(),
                                  
                                  
                                  
                                  # DISCUSSION     
                                  
                                  h3(strong("DISCUSSION"), align = "center" ), 
                                  
                                  br(), br(),
                                  
                                  tags$ul(
                                    tags$li("The rats seemed to be using the mean speed of the stimulus to
                                            guide them in their discrimination tasks of choosing a water dispenser", style = "font-size:25px"),
                                    tags$li("52% of neuronal units measured in the primary somatosensory cortex (s1) encoded the sequences at the fast time scales (mostly 5 – 10 ms) which reflects the single impulse representation", style = "font-size:25px"),
                                    tags$li("17% of the neuronal units showed weaker impulse responses and a slower time scale, but these units did not effectively integrate whisker impulses, but used weak impulse responses correlated to a behavioral choice", style = "font-size:25px"),
                                    tags$li("Neural decoders could identify sequences from fast unit spike trains and behavioral choices", style = "font-size:25px"),
                                    tags$li("They concluded that S1 encoded fast time scale whisker input without substantial temporal integration across whisker impulses ", style = "font-size:25px"),
                                    
                                  ),
                                  br(), br(), br()
                                  
                         ),
                         
                         
                         
                         tabPanel(h4(strong("Data")), 
                                  
                                  br(),
                                  h2(strong("FMS VS. SMF Comparison"), align = "center"), 
                                  br(),
                                  plotOutput(outputId = "comparison.spikes"),
                                  h3("This FMS spike times graph easily shows where the rat had the most concentrated spikes. 
                   During 250ms there seems to be spike interval differences of what seems to be less than
                   10ms, or almost instant. Throughout the paper it acknowledged the rat's somatosensory 
                   system or its neurons have 'very fast' or almost instant spike times as low as 5-20ms.
                   This level of concentrated spikes is only seen in this trial and not so much throughout 
                   the other 3 graphs other than during the SMF Trial but not to this degree."),
                                  br(),
                                  h3("The SMF spike time trial was also one that shows possible anticipation from the rat. 
                Throughout the experiment the rat was expected to put its nose through the 'nose poke' for
                a certain time limit and wait for a stimulus. Once the rat entered the nose poke it's likely
                it anticipated some sort of stimulus hence why spike times during the SMF trial go from widely
                spread to clustered right before the whisker stimulus test especially since they were required
                to pay attention and accurately depict which of the four different type of stimulus it could be.
                Another unique rapid/instant cluster of spike times is again seen around 450ms as seen during the FMS trial."),
                                  plotOutput(outputId = "comparison.firing"),
                                  h3("The FMS Firing Rate is split over 20 bins which is a constant throughout all the graphs so they can 
                  be compared. It also explains why its bin size is even but oddly numbered. This firing rate graph is
                  seen to have the highest firing rate of about 9 throughout a 94ms time period only during the stimulus
                  time frame which would reflect the rat acknowledges it's being tested and is expected to output an answer 
                  for a 'water reward'."),
                                  br(),
                                  h3("The SMF firing rate graph also shows the possibility of largely delayed firing rates at the start of 
                  the graph where the rat likely anticipates whisker stimulus the least. Spike times because of this were
                  possibly delayed one after another since there was no 'load' on the rat's mind. These Firing rates of 
                  course spike the most during the stimulus also seen inside the spike times graph."),
                                  plotOutput(outputId = "comparison.ISI"),
                                  h3("The FMS interval difference graph also predicts rats under stimulus are likely to have clustered spike 
                  times therefore lower interval difference values overall. This is shown throughout all 4 graphs as 'true'.
                  All graphs have a shorter x-axis since they all lack high 'Time difference' values. It's also apparent 
                  there's 'low' time difference values overall. The highest value throughout all 4 graphs being 100ms.
                  This would suggest that these rats spike times overall stayed fairly compact and didn't have increasingly 
                  larger spike times overtime as sometimes seen. This could be due to the fact that these trials usually 
                  lasted at a little over a second and rats were likely already in anticipation of the whisker."),
                                  br(),
                                  h3("In the SMF graph it was previously mentioned that the rat likely had a large delay in spike times
                  during the start of the nose poke which is possible since it likely this is the point where it had
                  the least anticipation of stimulus until it grew over under a second. This delay in time is also seen
                  in the interval difference graph and is what gives the graph its spread out domain, and it's outlier."),
                                  br(),
                                  
                                  h2(strong("Explore Different Variations Of The Data!"), align = "center"),
                                  
                                  
                                  br(), br(), br(),
                                  fluidRow(
                                    
                                    column(4,
                                           
                                           selectInput(inputId = "colorChoice",
                                                       label = "Choose a color theme for the graphs!",
                                                       choices = c("red", "green", "blue", "aquamarine", "blueviolet", "coral")),
                                    ),
                                    
                                    column(4,
                                    selectInput(inputId = "stimulusChoice",
                                                label = "Choose a stimulus!",
                                                choices = c("FMS", "SMF", "FFF", "SSS")),
                                    
                                    ),
                                    
                                    column(4,
                                    selectInput(inputId = "timeChoice",
                                                label = "Choose a time of recording!",
                                                choices = c("before", "during", "whole")),
                                    )
                                  ),
                                  
                                  
                                  plotOutput(outputId = "spike.time.graph"),
                                  plotOutput(outputId = "firing.rate.graph"),
                                  plotOutput(outputId = "ISI.graph")
                                  
                         )
              )        
  
  )
)
)







# SERVER 

server <- function(input, output, session) {
  
  # WELCOME POPUP 
  shinyalert(html = TRUE,
             title = "WELCOME",
             text = h4("Welcome to the somatosensory information page! Our goals for this page were to 
             create in informative and interactive website that allows people to learn more about the
             somatosensory system as a whole, even if you don’t have a background in neuroscience or 
             data science! By clicking on the tabs, you can learn the parts of the somatosensory system
             and how we are able to feel, process and react to touch and pain. We included data from a 
             real experiment  in the primary somatosensory cortex in rats. You can learn about this data
             and play with real data under the 'data exploration' tab!  We would like to thank Professor Crodelle and Professor Lyford for guiding us through this information 
             and website design!"), 
             confirmButtonText = "Continue",
             type = "info")
  
  
  
  
  
  
  # SPIKE TIME GRAPH 
  
  output$spike.time.graph <- renderPlot({
    
    # before 
    if (input$timeChoice == "before") {
      experiment1%>%                             
        filter(cellID == input$stimulusChoice & spikes < 0)%>%
        ggplot(aes(x = spikes, y = "Cell 1"))+
        geom_tile(width = 2) +
        labs(title = "Spike Times Before Stimulus",
             x = "Spike Times (ms)",
             y = "Neuron 1" 
        )+
        theme(text = element_text(size = 23))
    }
    
    # during
    else if (input$timeChoice == "during"){
      experiment1%>%                                
        filter(cellID == input$stimulusChoice & spikes > 0)%>%
        ggplot(aes(x = spikes, y = "Cell 1"))+
        geom_tile(width = 2) +
        labs(
          title = "Spike Times During Stimulus",
          x = "Spike Times (ms)",
          y = "Neuron 1"
        )+
        theme(text = element_text(size = 23))
    }
    
    # whole 
    else {
      experiment1%>%
        filter(cellID == input$stimulusChoice)%>%
        ggplot(aes(x = spikes, y = "Cell 1"))+
        geom_tile(width = 2) +
        labs(
          title = "Spike Times",
          x = "Spike Times (ms)",
          y = "Neuron 1"
        )+
        theme(text = element_text(size = 23))
    }
    
  })
  
  
  
  
  
  # FIRING RATE GRAPHS 
  
  # before 
  output$firing.rate.graph <- renderPlot({
    
    if (input$timeChoice == "before") {
      FMS1 <- experiment1 %>%                               
        filter(cellID == input$stimulusChoice, spikes < 0)
      
      FMSx1 <- cut(FMS1$spikes, seq(-1080,0, by = 50), dig.lab = 5)
      FMS1 <- data.frame(table(FMSx1))
      FMS1 <- rename(FMS1, `Firing Rate` = Freq)
      FMS1 <- rename(FMS1, `Time (ms)` = FMSx1)
      
      
      FMS1%>%                             
        ggplot(aes(x = `Time (ms)`, y = `Firing Rate`))+
        geom_col(fill = input$colorChoice, color = "black")+
        theme(axis.text.x = element_text(angle = 90))+
        labs(
          title = "Firing Rate Before Stimulus",
        )+
        theme(text = element_text(size = 23))
      
    }
    
    # during
    else if (input$timeChoice == "during"){
      FMS2 <- experiment1 %>%
        filter(cellID == input$stimulusChoice, spikes > 0)
      
      FMSx2 <- cut(FMS2$spikes, seq(0,900, by = 40), dig.lab = 5)
      FMS2 <- data.frame(table(FMSx2))
      FMS2 <- rename(FMS2, `Firing Rate` = Freq)
      FMS2 <- rename(FMS2, `Time (ms)` = FMSx2)
      
      
      FMS2%>%
        ggplot(aes(x = `Time (ms)`, y = `Firing Rate`))+
        geom_col(fill = input$colorChoice, color = "black")+
        theme(axis.text.x = element_text(angle = 90))+
        labs(
          title = "Firing Rate During Stimulus",
        )+
        theme(text = element_text(size = 23))
      
    }
    
    # whole 
    else {
      FMS3 <- experiment1 %>%
        filter(cellID == input$stimulusChoice)
      
      FMSx3 <- cut(FMS3$spikes, seq(-1080,900, by = 94), dig.lab = 5)
      FMS3 <- data.frame(table(FMSx3))
      FMS3 <- rename(FMS3, `Firing Rate` = Freq)
      FMS3 <- rename(FMS3, `Time (ms)` = FMSx3)
      
      
      FMS3%>%
        ggplot(aes(x = `Time (ms)`, y = `Firing Rate`))+
        geom_col(fill = input$colorChoice, color = "black")+
        theme(axis.text.x = element_text(angle = 90))+
        labs(
          title = "Firing Rate Whole Trial",
        )+
        theme(text = element_text(size = 23))
      
    }
    
  })
  
  
  
  # ISI GRAPHS  
  
  # before 
  output$ISI.graph <- renderPlot({
    
    if (input$timeChoice == "before") {
      experiment1%>%
        filter(cellID == input$stimulusChoice & spikes < 0)%>%
        summarise(time.difference = diff(spikes))%>%
        ggplot(aes(x = time.difference)) + 
        geom_histogram(fill = input$colorChoice, color = "black")+
        xlab("Time difference (ms)")+
        labs (
          title = "Inter-spike Interval Before Stimulus",
          y = "Count"
        )+
        theme(text = element_text(size = 23))
    }
    
    
    
    # during
    else if (input$timeChoice == "during"){
      experiment1%>%
        filter(cellID == input$stimulusChoice & spikes > 0)%>%
        summarise(time.difference = diff(spikes))%>%
        ggplot(aes(x = time.difference)) + 
        geom_histogram(fill = input$colorChoice, color = "black")+
        xlab("Time difference (ms)")+
        labs (
          title = "Inter-spike Inteval During Trial",
          y = "Count"
        )+
        theme(text = element_text(size = 23))
    }
    
    
    
    # whole 
    else {
      experiment1%>%
        filter(cellID == input$stimulusChoice)%>%
        summarise(time.difference = diff(spikes))%>%
        ggplot(aes(x = time.difference)) + 
        geom_histogram(fill = input$colorChoice, color = "black")+
        xlab("Time difference (ms)")+
        labs (
          title = "Inter-spike Interval Whole Trial",
          y = "Count"
        )+
        theme(text = element_text(size = 23))
      
    }
    
  })   
  
  output$comparison.spikes <- renderPlot({
    grid.arrange(spikesfms, spikessmf, ncol = 2)
  })
  
  output$comparison.firing <- renderPlot({
    grid.arrange(firingfms, firingsmf, ncol = 2)
  })
  
  output$comparison.ISI <- renderPlot({
    grid.arrange(ISIfms, ISIsmf, ncol = 2)
  })
}

shinyApp(ui, server)