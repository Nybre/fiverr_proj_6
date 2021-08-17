performance_rating_UI <- function(id) {
   #user interface section
  ns <- NS(id)
  tagList(  
    sidebarLayout( 
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 250, left = "auto", right = 17, bottom = "auto",
                    width = 330, height = "auto",
                     
                    tags$div(id = 'demo5',  class="collapse",
                             helpText(strong("Application explainer text")),  
                             hr(), 
                             hr()
                    )
      ),
      mainPanel(
        column(width = 12, 
               column(width = 3,
                      gradientBox(
                        title = strong("Individual Information"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-user",
                        shinyjs::useShinyjs(),
                        shinyjs::inlineCSS(appCSS),
                        textInput(ns("name_u"), "Project Name"),
                        textInput(ns("company_name"), "Company Name"),
                        textInput(ns("fund_name"), "Fund Name"),
                        textInput(ns("subject_name"), labelMandatory("Subject Name")),
                        textInput(ns("title"), "Title"),
                        textInput(ns("email"), "Email Address")
                      )), 
               column(width = 3,
                      gradientBox(
                        title = strong("Archetypes"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-question-circle", 
                        selectInput(ns("hi_archetypes"), "Hi Archetypes",
                                    c("Sovereign",  "Lover", "Magician", "Warrior")),
                        selectInput(ns("lo_archetypes"), "Lo Archetypes",
                                    c("Sovereign",  "Lover", "Magician", "Warrior")),
                        selectInput(ns("theme"), "Theme/Pattern",
                                    c("Idealizing",  "Affecting", "Fostering", "Releasing", 
                                      "Yielding", "Relying", "Detaching", "Optionizing",
                                      "Alerting","Establishing","Contesting","Upholding")),
                        selectInput(ns("opp_type"), "Opposing Type",
                                    c("Fostering over Alerting", "Affecting over Yielding", 
                                      "Detaching over Releasing", "Contesting over Optionizing",
                                      "Upholding over Relying", "Idealizing over Establishing", 
                                      "Yielding over Affecting", "Alerting over Fostering","Releasing over Detaching",
                                      "Optionizing over Contestin","Relying over Upholding",
                                      "Establishing over Idealizing"))
                      )),
               column(width = 3,
                      gradientBox(
                        title = strong("Kolbe A Index"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-question-circle", 
                        textInput(ns("kolbe_score"), ("Kolbe Score")),
                        selectInput(ns("fact_finder"), "Fact Finder",
                                    c("Simplify", "Explain",  "Specify")),
                        selectInput(ns("follow_thru"), "Follow Thru",
                                    c("Adapt", "Maintain",  "Systematize")),
                        selectInput(ns("quick_start"), "Quick Start",
                                    c("Stabilize", "Modify",  "Innovate")),
                        selectInput(ns("implementor"), "Implementor",
                                    c("Envision", "Restore",  "Demonstrate"))
                      )),
               
               column(width = 3,
                      gradientBox( 
                        title = strong("Kantor Baseline Instrument"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-question-circle",  
                        selectInput(ns("action_mode"), "Action Mode",
                                    c("Move", "Follow",  "Bystand", "Oppose")),
                        selectInput(ns("operating_system"), "Operating System",
                                    c("Open", "Closed",  "Random")),
                        selectInput(ns("com_domain"), "Communication Domain",
                                    c("Power", "Affect",  "Meaning")),
                        
                        #input executioner controls
                        radioButtons(ns('format'), 'Document format', c('HTML', 'Word','PDF'),
                                     inline = TRUE),
                        downloadButton(ns("downloadReport"), "Download Report" ,
                                     class = "btn btn-success ")  
                      ))) 
    
        ,width = 12), 
      position = c("right"))
  )
}

performance_rating <- function(input, output, session, pool) { 
  #server code
   
  #User inputs
   name_u = reactive({ input$name_u }) 
   company_name = reactive({ input$company_name }) 
   fund_name = reactive({ input$fund_name }) 
   subject_name = reactive({ input$subject_name }) 
   title = reactive({ input$title }) 
   email = reactive({ input$email })  
  
  #reactive input controls [Archetypes]
   hi_archetypes = reactive({ input$hi_archetypes }) 
   lo_archetypes = reactive({ input$lo_archetypes }) 

  #theme/pattern duplication
   theme = reactive({ input$theme }) 
 
 
   #opposing types
   opp_type = reactive({ input$opp_type })
   #kolbe section
   kolbe_score = reactive({ input$kolbe_score }) 
   fact_finder = reactive({ input$fact_finder }) 
   follow_thru = reactive({ input$follow_thru }) 
   quick_start = reactive({ input$quick_start }) 
   implementor = reactive({ input$implementor }) 
   #kanto section
   action_mode = reactive({ input$action_mode }) 
   operating_system = reactive({ input$operating_system }) 
   com_domain = reactive({ input$com_domain })  
   
   #Archetype Hi-Lo
   text_1 = reactive({ 
      
     #hi_archetype relation check 
     if(hi_archetypes()=="Sovereign"){
       connect_value_hi = "Visionary leadership"
     }else if(hi_archetypes()=="Lover"){
       connect_value_hi = "Relational leadership"
     }else if(hi_archetypes()=="Warrior"){
       connect_value_hi = "Perfomance leadership"
     }else if(hi_archetypes()=="Magician"){
       connect_value_hi = "Perfomance leadership"
     }
      
     #lo_archetype relation check
     if(lo_archetypes()=="Sovereign"){
       connect_value_lo = "Visionary leadership"
     }else if(lo_archetypes()=="Lover"){
       connect_value_lo = "Relational leadership"
     }else if(lo_archetypes()=="Warrior"){
       connect_value_lo = "Perfomance leadership"
     }else if(lo_archetypes()=="Magician"){
       connect_value_lo = "Perfomance leadership"
     }
      
      #theme relation check
      if(theme()=="Idealizing"){
         theme_out = "Idealizer"
      }else if(theme()=="Affecting"){
         theme_out = "Affecter"
      }else if(theme()=="Fostering"){
         theme_out = "Fosterer"
      }else if(theme()=="Detaching"){
         theme_out = "Detacher"
      }else if(theme()=="Optionizing"){
         theme_out = "Optimizer"
      }else if(theme()=="Alerting"){
         theme_out = "Alterter"
      }else if(theme()=="Establishing"){
         theme_out = "Establisher"
      }else if(theme()=="Contesting"){
         theme_out = "Contester"
      }else{
         theme_out = theme()
      }
      
     print(paste("They indicate strength in",connect_value_hi, ", high in ",hi_archetypes(), 
                 "energy and low in", connect_value_lo,", or ",lo_archetypes()," energy, known as ",theme_out,"."))
   })
   
   #Archetype Theme/Pattern
   text_2 = reactive({
     
     if(theme()=="Idealizing"){
       theme_out = "They are full of confidence that the right answer always emerges and 
       things will work out for the best. This type sometimes misses the need to acknowledge 
       the pain or struggle that those around them may be experiencing."
     }else if(theme()=="Affecting"){
       theme_out = "They manifest a 'fake it till theymake it' chutzpah— working hard to 
       impress others with their competencies, affiliations and possessions."
     }else if(theme()=="Fostering"){
       theme_out = "They are natural coaches and mentors—taking people under their wing, 
       helping them to grow and improve. They may have difficulty holding their mentees 
       accountable or defining appropriate boundaries."
     }else if(theme()=="Releasing"){
       theme_out = "They have an intuitive connection to their senses and feelings. 
       Their body is an instrument of both sensing and expression, giving them a 
       rich sensory life filled with gusto, intuition and emotion. Under pressure, 
       their sensitivity can feel overwhelming."
     }else if(theme()=="Yielding"){
       theme_out = "They feel a common bond with those who endure the struggle of their lives. 
       They may be pilgrims on a spiritual path, understanding that there are important 
       lessons in the trials and tribulations we endure. Yielding helps them avoid 
       buckling under the extreme pressures they have experienced or witnessed in life."
     }else if(theme()=="Detaching"){
       theme_out = "They get on the balcony, leave the crowd and watch from afar. 
       This tendency to disengage can give them perspective. The challenge for 
       this type is accepting the invitation to join the fray and engage with their team."
     }else if(theme()=="Optionizing"){
       theme_out = "They see all the options laid out before them. This type avoids 
       hurting themselves or others by making decisions too quickly or leaping to 
       conclusions. Instead, they juggle the possibilities, watching and waiting for 
       the right one to show itself. They will resist decisions they feel are 
       being made to quickly."
     }else if(theme()=="Alerting"){
        theme_out = "TThey are the watchdog that sounds the alarm when things could go wrong.
        This type is two steps ahead, vigilantly protecting their own and the collective well-being. 
        They are willing to be unpopular if it means protecting what they care about. 
        Their innate risk-management voice should not be confused with negativity."
     }else if(theme()=="Establishing"){
        theme_out = "This type has their own distinctive brand and style, their 
        modus operandi, and cares little for what the rest of the pack might be doing. 
        They can create friction in their relationships with their willingness to go their own way."
     }else if(theme()=="Contesting"){
        theme_out = "This type can dominate the space with their willingness to take the 
        leadership role and their ability to drive their point home.  This type feels their 
        own place by pushing against boundaries and resistance. Those with this pattern thrive in
        an environment when they can assert themselves cleanly in a way that doesn't 
        cause collateral damage inside or outside the team."
     }else if(theme()=="Upholding"){
        theme_out = "This type is compelled to correct anything around them that is bent 
        or broken. They will fight hard in pursuit of excellence and can benefit by 
        building relationships with other competent souls they can trust- 
        finding a place to let their guard down and just relax."
     }
     
     print(paste(theme_out)) 
     
  })
   
   #Archetype Pattern [Get clarity] 
   text_4 = reactive({
     
    if(opp_type()=="Fostering over Alerting"){
      opp_out = "Under pressure, they find the best in people, and seek to support others, 
      showing a willingness for self-sacrifice to benefit the whole.  
      A way of describing this pattern is 'support without caution'."
    }else if(opp_type()=="Affecting over Yielding"){
      opp_out = "Under pressure, they rise above the perceived bounds defined by their 
      past conditions. Humility is secondary to aspiration for them, and rather than 
      worrying about feeling like an outsider, they have chosen to carve their own path."
    }else if(opp_type()=="Detaching over Releasing"){
      opp_out = "Under pressure, they show a fierce independence and step back to get 
      away from emotions and drama. This can allow them to see the system as a whole.  
      They are content to hold these observations privately and have no need to prove themselves to others. "
    }else if(opp_type()=="Contesting over Optionizing"){
      opp_out = "Under pressure, they take decisive action, and can feel uncomfortable with 
      those who hesitate in the face of tough choices or get lost in the weeds of pros 
      and cons.  Thinking too much can be harmful, and so they discern a path forward 
      for themselves and others when leadership is needed."
    }else if(opp_type()=="Upholding over Relying"){
      opp_out = "Under pressure, they correct the mistakes and hold a level of excellence 
      based on doing what's right, for its own sake.  If they want the job done right, 
      they do it themselves. "
    }else if(opp_type()=="Idealizing over Establishing"){
      opp_out = "Under pressure, they manifest a strong sense of inspiration that can 
      be contagious–an ability to hold a big vision and see the potential.  
      These big ideas and charisma can lead them to pursue dreams that are not achievable or grounded."
    }else if(opp_type()=="Yielding over Affecting"){
      opp_out = "Under pressure, they find solidarity with those who keep their heads down 
      and their nose to the grindstone.  They feel it is not up to them to change 
      the system or save the world, and can see too much ambition as dangerous."
    }else if(opp_type()=="Alerting over Fostering"){
      opp_out = "Under pressure, they manifest a watchful independence.  
      They prefer to keep their position secure, keeping a watchful eye on the darker 
      side of human nature, understanding that even good people can sometimes take 
      advantage of others and be hypocritical. "
    }else if(opp_type()=="Releasing over Detaching"){
      opp_out = "Under pressure, they dive passionately into intuition and emotional 
      intensity; feeling all the highs, the lows, and the emotional doldrums and 
      deriving vitality and interpersonal connection through it all."
    }else if(opp_type()=="Optionizing over Contesting"){
      opp_out = "Under pressure, they weigh all the options and hold off on hasty decisions. 
      They have an ability to step back and really think through the many costs
      and benefits of a particular line of action or menu of choices. "
    }else if(opp_type()=="Relying over Upholding"){
      opp_out = "Under pressure, they build strong connections with people that are important to you.  
      They maintain these connections with those they depend on, and can create a strong web of support, 
      creating safety for themselves, and for others by being unafraid to ask for what they need.  
      It can be difficult for them to navigate when they feel their support network shaken 
      or lose those they depend on. "
    }else if(opp_type()=="Establishing over Idealizing"){
      opp_out = "Under pressure, they distinguish themselves through hard work and a unique path - reinforcing 
      their sense of inner strength.  In the face of adversity or pressure to blend in, 
      they chose the difficult path of breaking out on their own and defining their own way"
    }
     
     print(paste(opp_out)) 
     
     })
   
   text_5 = reactive({
     
     print(paste(subject_name(),"’s action instincts are ",kolbe_score(),"."))
     
     })
   
   text_6 = reactive({
     
     if(fact_finder()=="Simplify"){
       fact_out = "Gathers and shares information by Simplifying. 
       This means he will summarize and help get to the point, cut through the red tape, 
       and offer bottom-line options."
     }else if(fact_finder()=="Explain"){
       fact_out = "gathers and shares information by Explaining; he works within priorities,
       tests analogies, and starts with the highest probability. "
     }else if(fact_finder()=="Specify"){
       fact_out = "This means he will gather and share information by Specifying. 
       This includes ranking and quantifying, defining objectives, and developing complex strategies. "
     }
     
     print(paste(fact_out)) 
     
     })
   
   text_7 = reactive({
     
     if(follow_thru()=="Adapt"){
       follow_out = "He organizes by Adapting; he switches task frequently, naturally 
       multitasks and will thrive on interruptions."
     }else if(follow_thru()=="Maintain"){
       follow_out = "He organizes through Maintaining; packaging things together that fit, 
       adjusting procedures, coordinating schedules, and drafting guidelines."
     }else if(follow_thru()=="Systematize"){
       follow_out = "He will organize by Systematizing; this means he will create the plan,
       coordinate needs, and graph the logistics. "
     }
     
     print(paste(follow_out)) 
     
     })
   
   text_8 = reactive({
     
     if(quick_start()=="Stabilize"){
       quick_out = "He will deal with risk and uncertainty by Stabilizing; this means 
       he will protect the status quo, clarify deadlines, and minimize risk factors."
     }else if(quick_start()=="Modify"){
       quick_out = "She handles risks and uncertainty by Modifying; trying new ideas, 
       sustaining innovations and adjusting deadlines as needed."
     }else if(quick_start()=="Innovate"){
       quick_out = "She deals with risks and uncertainty by Innovating; creating a sense 
       of urgency, initiating change, and defying the odds."
     }
     
     print(paste(quick_out)) 
     
   })
   
   text_9 = reactive({
     
     if(implementor()=="Envision"){
       implementor_out = "Finally, he will handle space and tangibles by Envisioning; this
       means he will create virtual presentations, sketch ideas and capture the essence of things."
     }else if(implementor()=="Restore"){
       implementor_out = "His best method for tackling space and intangibles is to 
       Restoring; testing ingredients, fixing moving parts and removing both real and imagined obstacles."
     }else if(implementor()=="Demonstrate"){
       implementor_out = "Finally, he handles space and tangibles by Demonstrating; building 
       prototypes and scalable solution, testing functionality and troubleshooting 
       malfunctions by designing and fabricating new parts."
     }
     
     print(paste(implementor_out)) 
     
   })
   
   text_10 = reactive({
     
     if(action_mode()=="Move"){
       action_out = "A Move initiates. Movers are often the first to suggest a direction
       or to introduce a new idea or concept. Moves start the action."
     }else if(action_mode()=="Follow"){
       action_out = "A Follow supports. Followers get behind others’ ideas and take 
       the necessary action to carry the idea forward to completion. Follows finish the action."
     }else if(action_mode()=="Bystand"){
       action_out = "A Bystand bridges. Bystanders observe, add neutral perspective, 
       and integrate seemingly disparate ideas. Bystands connect the elements of the action."
     }else if(action_mode()=="Oppose"){
       action_out = "An Oppose challenges. Opposers push back on ideas, providing alternatives, 
       and helping to shore up weaknesses. Opposes correct the action."
     }
     
     print(paste(action_out)) 
     
   })
   
   text_11 = reactive({
     
     if(operating_system()=="Open"){
       operating_out = "Individuals who have the Open propensity emphasize process, participation, 
       and teamwork. They look for ways to include others and place a high value on consensus. 
       They believe everyone has a contribution to make."
     }else if(operating_system()=="Closed"){
       operating_out = "Individuals with the Closed propensity emphasize structure and planning and 
       are more likely to value both tradition and hierarchy. They provide others with 
       clarity about individual roles and responsibilities and expect everyone to know and follow the rules."
     }else if(operating_system()=="Random"){
       operating_out = "Individuals demonstrating the Random propensity operate with no rules and few boundaries. 
       They tend to emphasize creative expression, autonomy and individuality. 
       They rarely consider imposing a system on others - they merely
       want the freedom to operate in their own unique way."
     } 
     
     print(paste(operating_out)) 
   
     })
   
   text_12 = reactive({
     
     if(com_domain()=="Power"){
       com_out = "The language of Power is about accountability, competence and completion. 
       Individuals with a Power Propensity tend to speak about specific goals 
       and are highly conscious of time. They enjoy crossing items from their 
       list and moving projects to closure."
     }else if(com_domain()=="Affect"){
       com_out = "The language of Affect is about connection between people, particularly emotional. 
       Individuals with an Affect Propensity take special note of others’ well-being and 
       how they are reacting to what is being said. They emphasize trust and 
       motivation and try to provide a climate of warmth and caring."
     }else if(com_domain()=="Meaning"){
       com_out = "The language of Meaning is about thinking, logic and a sense of purpose. 
       Individuals with a Meaning Propensity are concerned with 'what we stand for' 
       and with deeply understanding how things work. They love exploring ideas and concentrating 
       on the theoretical underpinnings of a discussion."
     } 
     
     print(paste(com_out)) 
     
   })
     
   #download button with format as html (Other formats commented out)
   output$downloadReport <- downloadHandler(
     filename = function() {
       paste('my-report', sep = '.', switch(
         input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
       ))
     },
     content = function(file) {
       src <- normalizePath('report/report.Rmd')
        
       owd <- setwd(tempdir())
       on.exit(setwd(owd))
       file.copy(src, 'report.Rmd', overwrite = TRUE)
       
       out <- rmarkdown::render('report.Rmd',
                                params = list(text = input$text),
                                switch(input$format,
                                       PDF = pdf_document(), 
                                       HTML = html_document(), 
                                       Word = word_document()
                                ))
       file.rename(out, file)
     }
   )
    
} 