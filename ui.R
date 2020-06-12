mand<-read.csv("data/berg_full.csv", sep=',', header = T)
modern<-read.csv("data/modern.csv", sep=',', header = T)
hist<-read.csv("data/hist.csv", sep=',', header = T)
pooled<-read.csv("data/pooled.csv", sep=',', header = T)
sex<-read.csv("data/sex.csv", sep=',', header = T)
library(shinythemes)
library(shiny)
shinyUI(navbarPage(title = div(icon("smile-o"), lib = "font-awesome", "(hu)MANid"), windowTitle = "(hu)MANid", theme = shinythemes::shinytheme("spacelab"),
                   position = "fixed-top", inverse = TRUE, collapsible = TRUE,
                   header = tags$style(type = "text/css", "body {padding-top: 70px; padding-bottom: 70px;}"),
                   footer = HTML("
                                 <div class = 'navbar navbar-inverse navbar-fixed-bottom'>
                                 <div class = 'navbar-inner'>
                                 <div class = 'container footer-margin-top'>
                                 <span class = 'pull-right' style = 'color: white; padding-top: 10px;'>&copy; Gregory E. Berg & Michael W. Kenyhercz  2017</span>
                                 </div>
                                 </div>
                                 </div>"),
                   ## main tab for data input
                   tabPanel("Input", value = "input", icon = icon("gear"),
                            sidebarPanel(id="sidebar",
                              h4(strong("Comparison Parameters")),
                              selectizeInput("refsamp", "Select Reference Sample", choices = c("Modern Mandibular Dataset" = "mandible"), selected = "mandible", multiple = TRUE, width = "150%"),
                              selectizeInput("numgroups", "Select Number of Groups to Compare", choices=c("Two Groups" = "twogroup", "More Than Two Groups"= "multigroup"), selected= "multigroup", multiple=FALSE, width="150%"),
                              selectizeInput("meth", "Select Classification Statistic", choices=c("Linear Discriminant Analysis"="lda", "Mixture Discriminant Analysis"="mda"), selected="lda", multiple=FALSE, width="100%"),
                              selectizeInput("stepw", "Select Stepwise Procedure", choices=c("Forward Wilks" = "wilks", "None" = "none"), selected = "none", multiple=FALSE, width="150%")),
                            tags$style(HTML('
                                            #sidebar  {
                                            background-color: #e0ffff;
                                            }')),
                            
                            h3(strong("Reference Samples")),
                          checkboxGroupInput("group", "Modern Comparison Groups", choices= levels(modern$Group), inline=T, width="100%"),
                          checkboxGroupInput("group1", "Historic/Prehistoric Comparison Groups", choices= levels(hist$Group), inline=T, width="100%"),
                          checkboxGroupInput("group2", "Composite Groups", choices= levels(pooled$Group), inline=T, width="100%"),
                          checkboxGroupInput("group3", "Pooled Sexes", choices= levels(sex$Group), inline=T, width="100%"),
                            br(),
                            br(),
                            br(),
                          fluidRow(column(12, align = "center", h4(strong("Data Entry")))),
                            mainPanel(
                              fluidRow(id="eltab", column(12, tableOutput("el_table"))),
                              fluidRow(column(12, tableOutput("el_table1"))),
                              fluidRow(
                                column(6, textInput("case", "Case Number:")),
                                column(6, textInput("tech", "Analyst Name:"))
                                ),
                              actionButton("evaluate", "Evaluate", icon = icon("calculator")),
                              h3("Results and Output"),
                            div(
                                tabsetPanel(
                                tabPanel("Classification Results",
                                         fluidRow(
                                           column(12, h4("Classified Into:")),
                                           column(12, h5(verbatimTextOutput("lda_pred"))),
                                           tags$head(tags$style("#lda_pred{ font-size: 14px;
                                                              font-style: bold;}")),
                                           column(12, h4("Posterior Probabilities:")),
                                           column(12, h5(verbatimTextOutput("lda_prob"))),
                                           tags$head(tags$style("#lda_prob{ font-size: 14px;
                                                              font-style: bold;}")),
                                           column(12, h4("Chi-Square Typicality Probabilities:")),
                                           column(12, h4(verbatimTextOutput("typs"))),
                                           tags$head(tags$style("#typs{ font-size: 14px;
                                                              font-style: bold;}")),
                                           column(12, h4("Distance from Group Centroids:")),
                                           column(12, verbatimTextOutput("cendist")),
                                           tags$head(tags$style("#cendist{ font-size: 15px;
                                                              font-style: bold;}"))
                                           )
                                         ),
                                tabPanel("Classification Matrix",
                                        fluidRow(
                                         column(12, h4("Classification Matrix")),
                                         column(12, h3(verbatimTextOutput("confusionm"))),
                                         tags$head(tags$style("#confusionm{ font-size: 15px;
                                                              font-style: bold;}")),
                                         column(12, h5(verbatimTextOutput("confusionm2"))),
                                         tags$head(tags$style("#confusionm2{ font-size: 15px;
                                                              font-style: bold;}")),
                                         column(12, h4("Classification Matrix (in Percentages)")),
                                         tags$head(tags$style("#confusionm1{ font-size: 15px;
                                                              font-style: bold;}")),
                                         column(12, h5(verbatimTextOutput("confusionm1")))
                                         ),
                                        fluidRow(
                                         column(5, h4("Positive Predictive Value"), h5(verbatimTextOutput("confusionm3"))),
                                         tags$head(tags$style("#confusionm3{ font-size: 15px;
                                                              font-style: bold;}")),
                                         column(5, h4("Negative Predictive Value"), h5(verbatimTextOutput("NPV"))),
                                         tags$head(tags$style("#NPV{ font-size: 15px;
                                                              font-style: bold;}"))
                                        )),
                                tabPanel("Classification Plot", plotOutput("ldaplot", height = 500 , width= 650),
                                         br(),
                                         h5(textOutput("number1")),
                                         h5(textOutput("number2"))),
                                tabPanel("Biodistance Results",
                                        h4("Mahalanobis Distance Matrix"), h4(verbatimTextOutput("mahout")),
                                        plotOutput("mahplot", height=500, width=650)
                                        ),
                                tabPanel("Summary Statistics", verbatimTextOutput("summarystat")),
                                tabPanel("Model Details", verbatimTextOutput("modelspec"))


                                ), class= "span1")

                          )
                    ),
                   ## definitions sub-page
                   tabPanel("Definitions and Diagrams", value = "output", icon = icon("flask"),
                            tabsetPanel(
                              tabPanel("Morphoscopic Definitions",
                                fluidRow(
                                  br(),
                                  br(),
                                  h4("* Note: Darkened portions in each figure highlight area to be observed."),
                                  h4("* Note: Numbers on figure represent trait expression score to be entered into the application."),
                                  column(6,  h3("Chin Shape"), img(src = "cs1.jpg")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(6, p("Chin shape (CS). The chin shape is viewed from above (superiorly) and scored as either blunt (smoothly rounded), pointed (the chin comes to a distinct point), square (the chin has a nearly straight front) or bilobate (the chin has a distinct central sulcus). Using a straight-edge is helpful for distinguishing between the traits, and, in particular, diagnosing the square and bilobate forms "))
                                ),
                                     br(),
                                fluidRow(
                                  column(6, h3("Lower Border of Mandible"),img(src= "lbm.jpg")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(6, p("Lower border of the mandible (LBM). Four categories are recognized for this trait, and it is easiest to score the trait by placing the mandible on a flat surface. If the majority of the lower border of the mandible is flush against the surface, it is scored as straight. If there is a deviation of the border upward, typically in the region of the lower second to third molars, it is scored as undulating. If the mandible inclines near the chin (and is somewhat rounded in the gonial region), and it rocks forward when gentle pressure is applied to the anterior dentition, it is scored as a partial rocker, and finally, if the mandible is sufficiently rounded on the bottom, such that pressure on the anterior teeth causes it to rock forward and back, it is scored as a rocker."))
                                ),
                                  br(),
                            fluidRow(
                                  column(6, h3("Ascending Ramus Profile"), img(src="arp.jpg")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(6, p("Ascending ramus shape (ARS). This trait is scored as pinched if the ascending ramus noticeably narrows about its midpoint, or wide if it is a relatively uniform width."))
                                ),
                            fluidRow(
                                  column(6, h3("Gonial Angle Flare"),img(src="gaf.jpg")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(6, p("Gonial angle flare (GAF). This trait is scored in five stages, the first being inverted, where the gonial process slants medially toward the midline; absent, when the gonial process is in line with the ramus; slight when the gonial process flares outward a short distance (~1-2 mm); medium, when the gonial process flares beyond slight to double that distance (~2-4 mm); and everted, which is greater than twice the distance of slight (>~4 mm). This trait is bestscored in relation to the line drawings found in the above figure, and familiarity with multiple mandibles is recommended prior to scoring the trait."))
                                     ),
                            fluidRow(
                                  column(6, h3("PREI"), img(src="prei.jpg")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  column(6, p("Posterior ramus edge inversion (PREI). The trait is observed on the posterior one-third of the ascending ramus. If no discernible flexure toward the midline is present, the mandible is scored as straight. If a small, but discernible flexure toward the midline is present, the trait is scored as slight. Medium is a very noticeable inward deviation, up to twice the distance of the slight category. The mandible is scored as turned when it is greater than a double expression of the slight category."))
                            ),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            fluidRow(
                              column(6, h3("Mandibular Torus")),
                              column(6, p("Mandibular Torus (MT). The mandibular torus is a bony protuberance of varying size and shape on the lingual surface, below the alveolar margin, typically in the region of the premolars (see Hauser and De Stefano 1989 for additional description). This trait is only scored as present (2) or absent (1)."))
                            )
                            ),
                            tabPanel("Metric Definition Table", img(src="table1.png"),
                                     br(),
                                    "Table is taken from Byrnes et al. 2017"),
                            tabPanel("How Not to Read a Mandibulometer",
                              fluidRow(
                                 column(6,img(src="howto1.jpg")),
                                     br(),
                                     br(),
                                     br(),
                                 br(),
                                 br(),
                                 br(),
                                column(6, p(strong("There are two common misreads of a mandibulometer. The first is to add an extra 10 degrees to the actual measurement.  The second error is to read the complement of the angle. In the first instance, the error is rather insidious, and not easily spotted, while the second instance should be identifiable to the experienced practitioner."))))
                            ))
                   ),
                   ###Download Report
                   tabPanel("Print Report", value="evaluate", icon=icon("print"),
                            fluidRow(
                              column(12, align="center", h3(textOutput('title')))
                              ),
                            fluidRow(
                              column(4, h4("Case Number:"), verbatimTextOutput('casenum')),
                              tags$head(tags$style("#casenum{ font-size: 15px; font-style: bold;}")),
                              column(4, h4("Analyst:"), verbatimTextOutput('analyst')),
                              tags$head(tags$style("#analyst{ font-size: 15px; font-style: bold;}")),
                              column(4, h4("Classification Statistic:"), verbatimTextOutput('namestat')),
                              tags$head(tags$style("#namestat{ font-size: 15px; font-style: bold;}"))
                   ),
                           fluidRow(
                             column(12, align="center", h4("Measurements Used:"), verbatimTextOutput('elnamez')),
                             tags$head(tags$style("#elnamez{ font-size: 15px; font-style: bold;}"))
                           ),
                            fluidRow(
                             column(6, align="center", h4("Classification Matrix"), verbatimTextOutput("confusionmp")),
                           tags$head(tags$style("#confusionmp{ font-size: 15px; font-style: bold;}")),
                           column(6, align="center", h4("Classification Matrix %"), verbatimTextOutput("confusionm1p")),
                           tags$head(tags$style("#confusionm1p{ font-size: 15px; font-style: bold;}"))
                    ),
                          fluidRow(
                            column(12, align="center", verbatimTextOutput("confusionm2p")),
                            tags$head(tags$style("#confusionm2p{ font-size: 15px; font-style: bold;}"))
                          ),
                          fluidRow(
                           column(12, align="center", verbatimTextOutput("ldapredp")),
                           tags$head(tags$style("#ldapredp{ font-size: 15px; font-style: bold;}"))
                   ),
                          fluidRow(
                            column(6, align = "center", h4("Posterior Probabilities"), verbatimTextOutput("lda_probp")),
                            tags$head(tags$style("#lda_probp{ font-size: 15px; font-style: bold;}")),
                            column(6, align = "center", h4("Chi-Square Typicalities"), verbatimTextOutput("typsp")),
                            tags$head(tags$style("#typsp{ font-size: 15px; font-style: bold;}"))
                          ),

                          fluidRow(
                          column(12, align="center",h4("Variable Means by Group"), verbatimTextOutput("gmm")),
                          tags$head(tags$style("#gmm{ font-size: 14px; font-style: bold;}"))
                   ),
                          fluidRow(
                            column(12, align="center",h4("Difference of New Data to Each Group Mean"), verbatimTextOutput("gmmdiff")),
                            tags$head(tags$style("#gmmdiff{ font-size: 14px; font-style: bold;}"))
                          ),
                          fluidRow(
                            column(12, align="center", plotOutput("ldaplotp", height = 500, width= 650))
                          )

                   ),

                   tabPanel("About", value = "output", icon = icon("info-circle"),
                        tabsetPanel(
                            tabPanel("Using (hu)MANid",
                              fluidRow(
                                column(12, h4("Welcome to (hu)MANid"),
                                       p("(hu)MANid uses Linear Discriminant Function Analysis (LDFA or LDA) or Mixture Discriminant Analysis (MDA) to classify an unknown mandible into one of many reference groups within a worldwide (and time) database established by Dr. Gregory E. Berg (see Reference Sample Description).   The backbone of the Graphic User Interface (GUI) was developed with R, therefore output and formatting are constrained by the capabilities of this program.  Because this is a hosted GUI, having R on a local computer is not necessary.

                                         The underlying database is continuously updated, and a major addition came from Dr. Michael Kenyhercz.  If you are so inclined, we would be happy to add your data to program database and list you as a contributor.  Please send us your data!

                                         In the event you are willing to submit your data, please follow the format presented below:
                                         "),
                                       strong(" ID | Group | GNI | HML | TML | GOG | CDL | WRB | XRH | MLT | MAN | XDA | TLM23 | CS | LBM | ARS | GF | MT | PREI |"))
                              ),
                              fluidRow(
                                column(12, h4("Analytics"),
                                     p("While LDFA was not intended to handle ordinal data, such as the morphoscopic traits, we are of the same mind as Walker:  “the test of the efficacy of a specific discriminant procedure in this context is not how well the data fit the assumptions of the technique, but how well the procedure solves the classification problem at hand” (2008:43). Please see discussions in Berg and Kenyhercz (2017).  Mixture discriminant analysis is available as an additional analytic (see below) and random forest modeling is in beta testing (available for testing on the tab above).")
                                     )
                              ),
                              fluidRow(
                                column(12, h4("Comparisons"),
                                       p("To select a reference group, simply click on the radio button to the left of the group code (see the Population Descriptions tab for information concerning group definitions and population codes).  Next, use the drop-down box labeled 'Select Number of Groups to Compare' to select either 'Two Groups' or 'More Than Two Groups'.  The output for discriminant function analysis within R is dependent on comparison size.  If you select 'More Than Two Groups' and only select two groups to compare, you will receive the following error: 'Incorrect Number of Dimensions'. Make sure that you select the appropriate number of groups from the dropdown menu.
                                        ")
                                )
                              ),
                              fluidRow(
                                column(12, h4("Compare Data"),
                                        p("Below the Group Selection you will find an area to enter in new data. There are two tables - one for metric variables, and another for morphoscopic.  For trait definitions please refer to the 'Definitions and Diagrams' tab located on the top menu bar of (hu)MANid.  You may enter as many or as few variables as you wish.  You can include and exclude variables that contribute the most to your analysis by examining the discriminant function coefficients that are contained within the Model Summary tab (see below).
                                        ")
                                       )
                                ),
                              fluidRow(
                                column(12, h4("Stepwise Variable Selection"),
                                        p("Currently, the only stepwise option is a forward Wilks procedure using the function “greedy.wilks” from the klaR package (Weihs et al., 2016). A forward Wilks stepwise procedure starts with the variable that separates groups best and then iteratively adds variables based on the Wilks’ lambda criterion. Standard forward and backward variable selection were tested for the current application and omitted due to poor performance – see Berg and Kenyhercz (In Press).
                                          ")
                                       )
                                ),

                               fluidRow(
                                column(12, h4("Name and Case Number"),
                                       p("You have the option to enter the analyst name and case number, if so desired.  These will be reported on the Print Report output.
                                         ")
                                )
                               ),
                              fluidRow(
                                column(12, h4("Results and Output"),
                                       p("Once you have entered in your data, click the 'Evaluate' button beneath the Analyst Name box.  The model will not run until then;"), em("note, that the results and output will be over-written on each subsequent analysis.  If more than one analysis is conducted, be sure hit print and then save each analysis in another document or as a running document."),
                                       br(),
                                       br(),
                                       h5("Classification Results Tab"),
                                       p("The Classification Results tab shows the predicted Group, Posterior Probabilities, Chi-Square Typicalities, and Distance the individual is from each group centroid.  The Predicted Group and Posterior Probabilities are derived from the LDA function in the MASS package (Ripley et al., 2016) for LDA and from the mda function from the mda package (Hastie and Tibshirani, 2016).  Posterior probabilities show the probability that the new individual belongs to one of the reference groups tested - the greater the posterior probability, the more likely the individual is from that reference group.  Because discriminant function analysis forces an individual into a reference group, the posterior probabilities will always equal 1 when summed.  Note, LDA will classify every case, and it is up to the user to make logical, informed decisions on population groups to include (see Ousley and Jantz [2012] on the uses and abuses of LDA).  The Chi-square typicalities are calculated from the Morpho package (Schlager and Jefferis, 2016).  The Chi-square typicality describes how typical, or atypical, the individual is from each of the reference groups.  Unlike posterior probabilities, typicality probabilities do not sum to 1.  Typicality probabilities can be thought of as the p-values from a hypothesis test - if the typicality probability for a group is less than 0.05, it is considered atypical of that group (i.e., it is dissimilar to 95% of the individuals within that group).  It is important to remember that using 0.05 as a cut-off for a typicality is a general rule-of-thumb.  Having higher typicality probabilities does not necessarily indicate group membership.  Posterior and typicality probabilities should be used in conjunction with one another when considering a classification.  For example, if you have a model where an individual classifies strongly as one group (posterior probability >/= 0.95), but is atypical of that group (typicality probability </= 0.05), it is likely that the individual does not belong to any of the groups in the analysis (Ousley and Jantz, 2012).  The distances from group centroids were calculated using the fields package (Nychka et al., 2016).  The distance from the centroid can be used as a measure of similarity - the closer an individual is to a group centroid, the more 'typical' it is."),
                                       br(),
                                       h5("Classification Matrix"),
                                       p("The Classification Matrix tab shows the raw classification (or confusion) matrix with the sample size (n) included as a reference.  Below the classification matrix is the cross-validated total correct classification accuracy.  All models in (hu)MANid use leave-one-out-cross-validation, in which one individual is removed from the analysis, a model is created with all other individuals, and that individual is then classified using the created model.  This procedure is done iteratively until each individual has been removed and classified into the model created without their membership.  The classifications are then tallied and presented.  Using leave-one-out-cross-validation gives more realistic views of model performance by eliminating upward bias in classification accuracy.  Below the total correct classification is the classification matrix in percentage correct form.  Lastly, the positive and negative predictive values are displayed from confusion matrix function in the caret package (Kuhn et al., 2016)."),
                                       br(),
                                       h5("Classification Plot"),
                                       p("The Classification Plot tab shows the scatterplot (with 90% confidence ellipses) of the first two discriminant functions (when comparing more than two groups), or a histogram (when comparing two groups), as well as the location of the case relative to group membership.  The plots are all generated using the package ggplot2 (Wickham and Chang, 2016).  Below the plot, the discriminant function score(s) are presented for the case, as well as how much variation is accounted for by each discriminant function."),
                                       br(),
                                       h5("Summary Statistics"),
                                       p("The Summary Statistics tab shows summary statistics organized by reference group from the describeBy function, taken from the psych package (Revelle, 2016).  The statistics shown in this output include variable name, variable number (arbitrary), sample size, mean, standard deviation, median, mad (median absolute deviation [deviation from the median]), minimum value, maximum value, skew, and standard error for each variable selected within each population."),
                                       br(),
                                       h5("Model Details"),
                                       p("The Model Details tab shows the standard output associated with the LDA function from the MASS package (Ripley et al., 2016).  The output includes the prior probabilities, which is the probability the individual being subjected to the discriminant function analysis belongs to a particular group without any novel information.  We have opted to use equal prior probabilities. The output shows the means of each variable by group, the coefficients of the linear discriminant functions, and the proportion of trace.  The coefficients reveal variable importance in the model; variables with greater (positive or negative) coefficients are impacting the model more heavily than those with nominal coefficients. The proportion of trace details the amount of variation accounted for by each discriminant function.")
                                )


                                       ),
                                fluidRow(
                                  column(12, h4("Definitions and Diagrams"),
                                         p("The Definition and Diagrams tab is separated into three sub-pages: Morphoscopic Definitions, Metric Definition Table, and How Not to Read a Mandibulometer.  Each of these tabs are self-explanatory, and contain the referenced definitions for each variable and variable state."))
                                ),
                              fluidRow(
                                column(12, h4("Print Report"),
                                       p("The Print Report tab has been formatted to include pertinent information that can be saved to your computer or printed for a case file.  To print the Sex and Ancestry Estimation report, simply push cntl + p (windows) or command + p (mac).  If you would wish to save instead of print, under your print options, select 'Print to PDF' (windows), or 'Save to PDF' (mac).  The Sex and Ancestry Estimation report will automatically insert today's date (YYYY/MM/DD), Case Number, Analyst, measurements used and the case specifics (measurements and scores), the classification and percentage matrix, total correct classification, predicted group, posterior and typicality probabilities, variable means by group, difference from case to each variable mean (wherein negative numbers reflect larger measurements from the new individual), and either the scatterplot or histogram of the classification."))
                              )
                              ),
                            tabPanel("Population Descriptions",
                              fluidRow(
                                column(12, h3("Reference Sample Decriptions"),
                                       p("The following details each of the sample components, to date, included in (hu)MANid.  Descriptions of each “individual” population are followed by descriptions of “composite” populations.  Please refer to Berg (2008) or the reference cited in text for more details on individual populations; Berg and Kenyhercz (in process) detail the testing, use, and abuse of the composite populations.  Full citations can be found on the references tab. "))
                              ),
                              br(),
                              br(),
                              fluidRow(
                                column(12, h4("U.S. White Male and Female 19th Century Sample [WM(19c), WF(19c)]"),
                                       p("Mandibular data for 19th century U.S. Whites (and Blacks) are gathered at the Terry collection, housed at the Smithsonian Institution, Washington, D.C.  The Terry collection was procured by Dr. Robert Terry in St. Louis, primarily between 1910 and 1941, though Dr. Mildred Trotter continued to add to the collection until near her retirement in 1967.  The collection is comprised of primarily U.S. Whites and Blacks, and records documenting the sex, age, race, and cause of death are available.  Birth years are primarily between 1822 and 1943 (the data here represent individuals with predominately 19th century birth years).  While efforts were made to select for complete, toothed mandibles and documented ages, no other criteria were used in the selection process. "))
                            ),
                            br(),
                            br(),
                             fluidRow(
                              column(12, h4("U.S. Black Male and Female 19th Century Sample [BM(19c), BF(19c)]"),
                                     p("The U.S. Black Male and Female samples are gathered from two collections, the Terry collection (see above) and the Memphis collection, housed at the University of Tennessee.  The Memphis collection stemmed from a cultural resource management archaeological excavation of an American Black cemetery in Shelby County, Tennessee (Oster et al. 2005).  The cemetery was in use ca. 1899 to 1933.  Given the adult status of these individuals, and the dates of the graveyard, the likely birth years for these individuals are between 1840 and 1900.  All of the selected mandibles had confident sex assignments from osteological findings.  Additional information on this collection can be found in Meadows-Jantz and Wilson (2005)."))
                             ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("U.S. White Male and Female 20th Century Sample [WM(20c), WF(20c)]"),
                                     p("These samples are composed of individuals from three primary collections, the William W. Bass Donated (WBD) collection, the University of Tennessee (UT) Forensic collection, and the Central Identification Laboratory, Hawaii (CILHI) collection.  The WBD collection began in the early 1980’s and is composed of individuals who specifically donated their remains to the University of Tennessee, Department of Anthropology.  The collection has documented sex, age, stature, and cause/manner of death for the majority of the cases.  The birth years range for the collection is from 1892-1987, with the vast majority falling between the years 1915 and 1962.  In addition, a few specimens were selected for analysis from the UT Forensic collection.  The majority of the individuals collected for this study had known ages, though a few only had age ranges.  The CILHI collection is a sample of U.S. war dead predominately from World War II and the Korean War.  All of these individuals have been identified and have known ages-at-death.  The majority were young, between 19 and 28 years at death.  The span of birth years is between 1901 and 1936, with the majority between 1917 and 1932.  In the instances of the WBD and the UT Forensic collections, individuals set were collected by both authors."))
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("U.S. Black Male and Female 20th Century Sample [BM(20c), BF(20c)]"),
                                     p("As with the U.S. White 20th century sample, the U.S. Black 20th century sample was collected solely from individuals housed at the WBD and the UT Forensic collections.  Sample parameters are the same as reported above.  In the instances of the WBD and the UT Forensic collections, individuals set were collected by both authors.")
                                     )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Chinese Males (CHM)"),
                                     p("The Chinese male sample stems from two populations, a Korean War sample of Chinese recovered by anthropologists at CILHI, and another population housed at the Smithsonian Institution.  The largest portion of the sample is from a Chinese cemetery in Uyak Bay, Kodiak Island excavated by A. Hrdlicka in 1931 (Hrdlicka 1944).  These individuals are thought to be labeled the “Canton District Workers” in his later publications (Hrdlicka 1940).  A smaller sample of Chinese males were recovered during excavations on the Korean peninsula, usually north of the 53rd parallel.  All associated cultural materials were Chinese in origin (e.g. buttons, clothing, and military equipment).  The excavation locations are consistent with Chinese military engagements with U.N. troops (e.g. the Chosin Reservoir area).  The likely birth dates for the Canton District Workers are from the late 1860s to the late 1890s, while the birth date for the CIL Chinese are from the 1900s to 1930s.  Sex and age data were based on osteological analysis.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Hispanic Males (HM)"),
                                     p("The Pima Country Office of the Medical Examiner (PCOME) composes the largest sample portion of Hispanic individuals in the program.  The POCME office has a relatively small, rotating collection of unidentified human remains that have been found in the desert between roughly Tucson, Arizona and the U.S./Mexico border.  Frequently, the remains have been found by hunters/hikers and are in a skeletonized condition.  Undoubtedly, these remains represent those individuals that attempted to cross the border into the U.S.  Anthropological analysis typically classifies them as Hispanic; eight individuals have been identified positively as Mexican nationals and the rest are held in a status of unidentified border crossers (Bruce Anderson, pers. comm. 2007).  All biological data are from osteological analysis.  Additional individuals are those that identified as Hispanics that are part of the WBD collection at UT.  In the instances of the WBD and the UT Forensic collections, individuals set were collected by both authors.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Guatemalan Males and Females (GUATM, GUATF)"),
                                     p("The Fundacion de Antropologia Forense de Guatemala (FAFG) is an organization in Guatemala dedicated to the recovery and identification of those individuals that were killed during a brutal, multiple-decade civil war (modern era).  Their collections are rapidly rotating; as individuals or groups of individuals are identified, their remains are returned to the appropriate village for internment.  All sex and age estimates are from osteological analysis by FAFG anthropologists.  The individuals in this sample were from several villages from the country’s interior and are considered indigenous Indians (Mayan).")
                              )
                            ),

                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Arikara Males and Females (ARKM, ARKF)"),
                                     p("This proto-historic sample of Native Americans is drawn from a single occupation site, the Larson site (39WW2), which is housed at the Department of Anthropology, University of Tennessee.  The Larson site was located on the east bank of the Missouri river in Walworth County, South Dakota (Owsley and Bass 1979).  The site is dated to between A.D. 1679 and A.D. 1733 and is associated with the Plain Indians group, the Arikara.  All age and sex estimates were derived from osteological analysis, and were recorded for this study from existing information sources.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Hohokam Males and Females (HOHM, HOHF)"),
                                     p("The Hohokam sample is from a prehistoric Native American group from Central Arizona (Southwest Indian).  Three sites (from the now greater Phoenix area and one site from approximately 50 miles north of Phoenix) are represented in the study sample.  The most northern site is considered part of the “Sinagua” tradition, though it still falls within the Hohokam culture area.  The collections are housed at two locations, the Maxwell Museum at Arizona State University (ASU) and the Smithsonian Institution.  The Smithsonian collection was procured by A. Hrdlicka, while the remainder was from several excavations conducted by ASU.  The associated time period for the sites is from approximately A.D. 1150-1450.  All biological data are osteologically determined.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Nubian Males and Females (NUBM, NUBF)"),
                                     p("The prehistoric Nubian osteology collection is housed at the Maxwell Museum, ASU.  The skeletal materials were excavated by the Oriental Institute and the University of Chicago during the 1966-1968 UNESCO Project, prior to flooding of the Nile River valley by the completion of the High Aswan Dam.  Only those individuals from the Meroitic time period, 100 B.C.-A.D. 350, are included in the data.  Sex or age assessment were osteological derived.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Cambodian Males and Females (CAMM, CAMF) "),
                                     p("From 1975-1979, the Khmer Rouge regime was directly and indirectly responsible for the deaths of approximately 1.5 million Cambodians.  One of the most notorious centers for execution was Choeung Ek, near the capital city of Phnom Penh; several thousand individuals were murdered there.  Approximately half buried at Choeung Ek were disinterred between 1979 and 1980 and were eventually placed into a stupa, constructed in 1988.  All age and sex data were osteologically derived.  As inferred from the ages represented and the timing of their probable deaths, the vast majority of birth years are between 1925 and 1955.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Vietnamese Males (VIETM)"),
                                     p("The sample of Vietnamese males predominately was derived from a single incident involving a C-130 aircraft that crashed in 1974.  The remains are housed at CILHI.  All individuals aboard the aircraft were male of “fighting age.”  The likely birth years for these individuals is between 1925 and 1955.  In addition to the single crash incident, multiple sets of remains, which have been identified as Vietnamese, are housed at the laboratory.  All sex and age estimates of these individuals are derived from osteological analysis.  Although nearly impossible to determine, the birth years for these individuals are likely similar to those involved in the C-130 crash, or slightly older.")
                              )
                            ),

                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Thai Males and Females (THAIM, THAIF)"),
                                     p("The Thai sample is composed of mostly known sex and age individuals housed at the Khon Kaen University, Khon Kaen, Thailand.  This modern sample is from northeast Thailand and is made up of individuals who willed themselves to science; typically, each individual was used for medical students’ training prior to being defleshed and incorporated into the skeletal collection.  Given the nature of the collection, the average age of the individuals was relatively high (53 years for both males and females), and the birth years ranged from approximately 1925 to 1985, with the bulk clustered between 1945 and 1970.")
                              )
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, h4("Korean Males and Females (KORM, KORF, phKORM, phKORF)"),
                                     p("There were two distinct Korean samples gathered for this study, a modern sample composed of War dead from the Korean War located at Chungbuk National University, the Ministry of Defense Missing/Killed in Action Accounting Command (MAKRI) in Seoul, and known age and sex forensic cases and willed individuals housed at Yonsei University, Dental School, Seoul.  Birth years for these individuals were estimated to be from 1925 to 1995, with the bulk clustering from 1945 to 1970.  These make up the KORM and KORF samples."),
                                     br(),
                                     p("The second sample was older, from the Late Joseon dynasty, circa the 18th and 19th century, AD.  Skeletal materials from the Joseon dynasty were from two distinct cemeteries excavated and maintained by separate universities, Seoul National University and Chongju National University.  Both populations are primarily archaeological in nature; sex determinations and age estimates were provided by Korean anthropologists.  These are collectively the prehistoric Korean sample (phKORM, phKORF)")
                              )
                            ),
                            br(),
                            fluidRow(column(12, h3("Composite Groups"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("American Indian (AIM, AIF)"),
                                     p("Pooled Hohokam and Arikara samples"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("American Black (BM, BF)"),
                                     p("Pooled 19th and 20th Century Black samples"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("Hispanic (HISPM, HISPF)"),
                                     p("Pooled Guatemalan and Hispanic males (HM) samples"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("Northeast Asian (NEAM, NEAF)"),
                                     p("Pooled Korean and Chinese samples"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("Southeast Asian (SEAM, SEAF)"),
                                     p("Pooled Cambodian, Thai, and Vietnamese samples"))
                                     ),
                            br(),
                            fluidRow(column(12, h4("American Whites (WM, WF)"),
                                     p("Pooled 19th and 20th Century American White"))
                                     ),
                            br(),
                            fluidRow(column(12, h3("Pooled Sexes"),
                                     p("Pooled all sexes excluding Cambodian males and females due to sex estimation being completed osteologically in many instances."))
                            )
                            ),
                            tabPanel("References",
                            fluidRow(
                              column(12, h3("Mandibular Metric/Morphology Presentations and Publications"),
                                     p("Berg, Gregory E. and Kenyhercz, Michael W. 2017. Introducing Human Mandible Identification [(hu)MANid]: A Free, Web-Based GUI to Classify Human Mandibles. Journal of Forensic Sciences. Mar 6. doi: 10.1111/1556-4029.13479."),
                                     p("Kenyhercz, Michael W. and Berg, Gregory E. 2017. Evaluating Mixture Discriminant Analysis to Classify Unknown Human Mandibles with (hu)MANid, A Free, R-Based GUI. In New Perspectives in Forensic Human Skeletal Identification, edited by Krista Latham, Eric Bartelink, and Michael Finnegan. CRC Press, Taylor & Francis Group, Boca Rotan, FL."),
                                     p("Berg, Gregory E. 2014.	Chapter 4:  Biological Affinity and Sex from the Mandible Utilizing Multiple World Populations.  In Biological Affinity in Forensic Identification of Human Skeletal Remains:  Beyond Black and White, edited by Gregory E. Berg and Sabrina C. Ta’ala, pp. 43-82, CRC Press, Taylor & Francis Group, Boca Rotan, FL."),
                                     p("Berg, Gregory E. 2008.	Biological Affinity and Sex Determination Using Morphometric and Morphoscopic Variables from the Human Mandible.  Dissertation, University of Tennessee, Knoxville."),
                                     p("Berg, Gregory E. 2008.	Discriminant Function Analysis as Applied to Mandibular Metrics to Assess Population Affinity.  Paper presented at the 60th Annual Meeting of the American Academy of Forensic Sciences, D.C."),
                                     p("Berg, Gregory E. 2006.	Discriminant Function Analysis as Applied to Mandibular Morphology to Assess Population Affinity.  Paper presented at the 58th Annual Meeting of the American Academy of Forensic Sciences, Seattle."),
                                     p("Berg, Gregory E. 2001.	Mandibular Morphology and its Relationship to Population Affinity. Paper presented at the 70th Annual Meetings of the American Association of Physical Anthropologists")
                              )
                              ),
                            br(),
                            fluidRow(
                              column(12, h3("Inter-observer Error Presentations and Publications"),
                                     p("Byrnes, Jennifer, Michael Kenyhercz, and Gregory Berg 2017.	Examining Inter-observer Reliability of Metric and Morphoscopic Characteristics of the Mandible.  Journal of Forensic Sciences Volume 62, Issue 4, pp. 981–985."),
                                     p("Byrnes, Jennifer, Michael Kenyhercz, Samantha Torres, Gregory Berg 2016.	Examining Inter-Observer Reliability of Metric and Morphoscopic Characteristics of the Mandible.  Paper presented at the 68th Annual Meeting of the American Academy of Forensic Sciences, Las Vegas.")
                                     )
                            ),
                            br(),
                            fluidRow(
                              column(12, h3("Population Background Publications"),
                                     p("Hrdlicka, Ales. 1940.	Lower Jaw:  The gonial angle and bigonial breadth.  American Journal of Physical Anthropology 27(2):281-308."),
                                     p("Hrdlicka, Ales. 1944.	The Anthropology of Kodiak Island.  Wister Institute, Philadelphia, PA."),
                                     p("Meadows-Jantz, Lee and Rebecca J. Wilson. 2005.	Osteological analysis of burial from Providence Baptist Church (40SY619).  In Oster, W.J., G.G. Weaver, J.P. Richardson, and J.M. Wyatt (eds.), Archaeological and Osteological Investigations of the Providence Baptist Church Cemetery (40SY619) Memphis-Shelby County Airport, Memphis, Shelby County, Tennessee.  Weaver and Associates LLC, Memphis, TN."),
                                     p("Oster, W. J., G. G. Weaver, J. P. Richardson, and J. M. Wyatt. 2005.	Archaeological and Osteological Investigations of the Providence Baptist Church Cemetery (40SY619) Memphis-Shelby County Airport, Memphis, Shelby County, Tennessee.  Weaver and Associates LLC, Memphis, TN."),
                                     p("Owsley, Douglas and William Bass. 1979.	A demographic analysis of skeletons from the Larson Site (39WW2) Walworth County, South Dakota:  Vital statistics.  American Journal of Physical Anthropology 51:145-154.")
                                     )
                            ),
                            fluidRow(
                              column(12, h3("R Packages and Statistics"),
                                     p("Hadley Wickham and Romain Francois (2016). dplyr: A Grammar of Data Manipulation. R package version 0.5.0. https://CRAN.R-project.org/package=dplyr"),
                                     p("Revelle, W. (2016) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, http://CRAN.R-project.org/package=psych Version = 1.6.6."),
                                     p("Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and Regression Training. R package version 6.0-71. https://CRAN.R-project.org/package=caret"),
                                     p("Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0"),
                                     p("Ripley, B. D. (1996) Pattern Recognition and Neural Networks. Cambridge University Press"),
                                     p("S original by Trevor Hastie & Robert Tibshirani. Original R port by Friedrich Leisch, Kurt Hornik and Brian D. Ripley. (2016). mda: Mixture and Flexible Discriminant Analysis. R package version 0.4-9. https://CRAN.R-project.org/package=mda"),
                                     p("Hastie T and Tibshirani R. Discriminant Analysis by Gaussian Mixtures. 1996, JRSS-B, 155-176."),
                                     p("Douglas Nychka, Reinhard Furrer, John Paige and Stephan Sain (2015). “fields: Tools for spatial data.” doi:10.5065/D6W957CT (URL: http://doi.org/10.5065/D6W957CT), R package version 8.4-1, <URL:www.image.ucar.edu/fields>"),
                                     p("Stefan Schlager (2016). Morpho: Calculations and Visualisations Related to Geometric Morphometrics. R package version 2.4. https://CRAN.R-project.org/package=Morpho"),
                                     p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009."),
                                     p("Kenyhercz MW, Passalacqua NP. 2016. Missing data imputation methods and their performance with biodistance analyses. Forensic and Bioarchaeological Perspectives on Biological Distance. Elsevier"),
                                     p("Kenyhercz MW, Passalacqua NP, Hefner JT. 2016. Missing data imputation methods using morphoscopic traits and their performance in the estimation of ancestry. Poster Presentation at the 68th Annual Scientific Meeting of the American Academy of Forensic Sciences, Las Vegas, NV. February 22-27."),
                                     p("Weihs, C., Ligges, U., Luebke, K. and Raabe, N. (2005). klaR Analyzing German Business Cycles. In Baier, D., Decker, R. and Schmidt-Thieme, L. (eds.). Data Analysis and Decision Support, 335-343, Springer-Verlag, Berlin.")

                                     )
                            )
                            )
                            
                              
                            )
                  ),
                  tabPanel("Random Forest Modeling Beta", value="evaluate1",
                  fluidRow(
                    column(5, textInput("rfntrees", "Number of Trees:")),
                    column(5, textInput("rfvartest", "# of Variables Tested At Each Node:")),
                    column(2, actionButton("evaluate1", "Evaluate", icon = icon("calculator")))
                  ),
                  fluidRow(
                    column(6, align="center", h4("Classification Matrix"), verbatimTextOutput("rmconm")),
                    tags$head(tags$style("#rmconm{ font-size: 15px; font-style: bold;}")),
                    column(6, align="center", h4("Classification Matrix (%)"), verbatimTextOutput("rmconmperc")),
                    tags$head(tags$style("#rmconmperc{ font-size: 15px; font-style: bold;}")),
                    column(12, align="center", verbatimTextOutput("tccrf")),
                    tags$head(tags$style("#tccrf{ font-size: 15px; font-style: bold;}")),
                    column(12, align="center", verbatimTextOutput("rfpred")),
                    tags$head(tags$style("#rfpred{ font-size: 15px; font-style: bold;}")),
                    column(12, align="center", verbatimTextOutput("rfprob")),
                    tags$head(tags$style("#rfprob{ font-size: 15px; font-style: bold;}")),
                    column(6, align="center", verbatimTextOutput("RFPPV")),
                    tags$head(tags$style("#RFPPV{ font-size: 15px; font-style: bold;}")),
                    column(6, align="center", verbatimTextOutput("RFNPV")),
                    tags$head(tags$style("#RFNPV{ font-size: 15px; font-style: bold;}")),
                    fluidRow(
                      column(12, align="center", plotOutput("rfplot"))),
                    fluidRow(
                      column(12, align="center", plotOutput("rfvimp", height = 500, width= 650)))
                    )
          )
    )
)
