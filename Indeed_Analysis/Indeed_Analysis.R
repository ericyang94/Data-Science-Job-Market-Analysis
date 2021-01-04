library(tidyverse)

listings <- read.csv('Indeed_Analysis/listings_unitedstates_Categorized.csv')

# search for key languages
listings <- listings %>%
    mutate(python = grepl("[ ,][pP]ython[ ,]", Description),
         r = grepl("[ ,][rR][ ,]", Description),
         sql = grepl("[ ,][sS][qQ][lL][ ,]", Description),
         java = grepl("[ ,][jJ]ava[ ,]", Description),
         scala = grepl("[ ,][sS]cala[ ,]", Description),
         c = grepl("[ ,]C[ ,]|[ ,]C\\+\\+[ ,]|[ ,]C/C\\+\\+[ ,]", Description),
         julia = grepl("[ ,][jJ]ulia[ ,]", Description),
         javascript = grepl("[ ,][jJ]ava[sS]cript[ ,]", Description))

languages_all <- listings %>%
  summarize(Python = mean(python),
            R = mean(r),
            SQL = mean(sql),
            Java = mean(java),
            Scala = mean(scala),
            `C/C++` = mean(c),
            Julia = mean(julia),
            JavaScript = mean(javascript))%>%
  gather(language, proportion, Python, R, SQL, Java, Scala, `C/C++`, Julia, JavaScript)

languages_all %>% ggplot(aes(x = reorder(language,-proportion), y = proportion)) +
  geom_bar(stat = "identity", aes(fill = language)) +
  labs(x = "Programming Languages", y = "Proportion Mentioned", title = "Top Data Science Programming Languages") +
  theme_light() +
  theme(legend.position = "none")
  
languages <- listings %>%
  group_by(Position) %>%
  summarize(Python = mean(python),
            R = mean(r),
            SQL = mean(sql),
            Java = mean(java),
            Scala = mean(scala),
            `C/C++` = mean(c),
            Julia = mean(julia),
            JavaScript = mean(javascript)) %>%
  gather(language, proportion, Python, R, SQL, Java, Scala, `C/C++`, Julia, JavaScript, -c(Position))

languages %>% ggplot(aes(x = reorder(language,-proportion), y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Position)) +
  labs(x = "Programming Languages", y = "Proportion Mentioned", title = "Top Data Science Programming Languages by Position") +
  theme_light()



# search for key tools
listings <- listings %>%
  mutate(excel = grepl("[ ,][eE]xcel[ ,]", Description),
         sas = grepl("[ ,][sS][aA][sS][ ,]", Description),
         tableau = grepl("[ ,][tT]ableau[ ,]", Description),
         spark = grepl("[ ,][sS]park[ ,]", Description),
         matlab = grepl("[ ,][mM][aA][tT][lL][aA][bB][ ,]", Description),
         hadoop = grepl("[ ,][hH]adoop[ ,]", Description),
         jupyter = grepl("[ ,][jJ]upyter[ ,]", Description),
         tensorflow = grepl("[ ,][tT]ensor[fF]low[ ,]", Description),
         ggplot2 = grepl("[ ,]ggplot2?[ ,]", Description),
         scikitlearn = grepl("[ ,][sS]cikit-?[lL]earn[ ,]", Description),
         matplotlib = grepl("[ ,][mM]atplotlib[ ,]", Description),
         pandas = grepl("[ ,][pP]andas[ ,]", Description))

tools_all <- listings %>%
  summarize(Excel = mean(excel),
            SAS = mean(sas),
            Tableau = mean(tableau),
            Spark = mean(spark),
            MATLAB = mean(matlab),
            Hadoop = mean(hadoop),
            Jupyter = mean(jupyter),
            TensorFlow = mean(tensorflow),
            ggplot2 = mean(ggplot2),
            `Scikit-learn` = mean(scikitlearn),
            Matplotlib = mean(matplotlib),
            pandas = mean(pandas)) %>%
  gather(tool, proportion, Excel, SAS, Tableau, Spark, MATLAB, Hadoop, Jupyter, TensorFlow, ggplot2, `Scikit-learn`, Matplotlib, pandas)

tools_all %>% ggplot(aes(x = reorder(tool,-proportion), y = proportion)) +
  geom_bar(stat = "identity", aes(fill = tool)) +
  labs(x = "Tools", y = "Proportion Mentioned", title = "Top Data Science Tools") +
  theme_light() +
  theme(legend.position = "none")

tools <- listings %>%
  group_by(Position) %>%
  summarize(Excel = mean(excel),
            SAS = mean(sas),
            Tableau = mean(tableau),
            Spark = mean(spark),
            MATLAB = mean(matlab),
            Hadoop = mean(hadoop),
            Jupyter = mean(jupyter),
            TensorFlow = mean(tensorflow),
            ggplot2 = mean(ggplot2),
            `Scikit-learn` = mean(scikitlearn),
            Matplotlib = mean(matplotlib),
            pandas = mean(pandas)) %>%
  gather(tool, proportion, Excel, SAS, Tableau, Spark, MATLAB, Hadoop, Jupyter, TensorFlow, ggplot2, `Scikit-learn`, Matplotlib, pandas, -c(Position))

tools %>% ggplot(aes(x = reorder(tool,-proportion), y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Position)) +
  labs(x = "Tools", y = "Proportion Mentioned", title = "Top Data Science Tools by Position") +
  theme_light()


# search for key technical concepts
listings <- listings %>%
  mutate(ml = grepl("[ ,][mM]achine [lL]earning[ ,]", Description),
         dl = grepl("[ ,][dD]eep [lL]earning[ ,]", Description),
         model = grepl("[ ,][mM]odel[ ,]|[ ,][mM]odeling[ ,]", Description),
         regression = grepl("[ ,][rR]egression[ ,]", Description),
         datamining = grepl("[ ,][dD]ata [mM]ining[ ,]", Description),
         visualization = grepl("[ ,][vV]isualizations?[ ,]", Description),
         ai = grepl("[ ,][aA][iI][ ,]|[ ,][aA]rtificial [iI]ntelligence[ ,]", Description),
         nlp = grepl("[ ,][nN][lL][pP][ ,]|[ ,][nN]atural [lL]anguage[ ,]", Description),
         statistics = grepl("[ ,][sS]tatistics?[ ,]", Description),
         cloud = grepl("[ ,][cC]loud[ ,]", Description),
         bigdata = grepl("[ ,][bB]ig ?[dD]ata[ ,]", Description))

concepts_all <- listings %>%
  summarize(`Machine Learning` = mean(ml),
            `Deep Learning` = mean(dl),
            Modeling = mean(model),
            Regression = mean(regression),
            `Data Mining` = mean(datamining),
            Visualization = mean(visualization),
            AI = mean(ai),
            NLP = mean(nlp),
            Statistics = mean(statistics),
            Cloud = mean(cloud),
            `Big Data` = mean(bigdata)) %>%
  gather(concept, proportion, `Machine Learning`, `Deep Learning`, Modeling, Regression, `Data Mining`, Visualization, AI, NLP, Statistics, Cloud, `Big Data`)

concepts_all %>% ggplot(aes(x = reorder(concept,-proportion), y = proportion)) +
  geom_bar(stat = "identity", aes(fill = concept)) +
  labs(x = "Concepts", y = "Proportion Mentioned", title = "Top Data Science Concepts") +
  theme_light() +
  theme(legend.position = "none")

concepts <- listings %>%
  group_by(Position) %>%
  summarize(`Machine Learning` = mean(ml),
            `Deep Learning` = mean(dl),
            Modeling = mean(model),
            Regression = mean(regression),
            `Data Mining` = mean(datamining),
            Visualization = mean(visualization),
            AI = mean(ai),
            NLP = mean(nlp),
            Statistics = mean(statistics),
            Cloud = mean(cloud),
            `Big Data` = mean(bigdata)) %>%
  gather(concept, proportion, `Machine Learning`, `Deep Learning`, Modeling, Regression, `Data Mining`, Visualization, AI, NLP, Statistics, Cloud, `Big Data`, -c(Position))

concepts %>% ggplot(aes(x = reorder(concept,-proportion), y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Position)) +
  labs(x = "Concepts", y = "Proportion Mentioned", title = "Top Data Science Concepts by Position") +
  theme_light()

# search for key soft skills
listings <- listings %>%
  mutate(communication = grepl("[ ,][cC]ommunication[ ,]|[ ,][cC]ommunicate[ ,]|[ ,][cC]ommunicating[ ,]", Description),
         leadership = grepl("[ ,][lL]eadership[ ,]|[ ,][lL]eader[ ,]|[ ,][lL]ead[ ,]", Description),
         solving = grepl("[ ,][pP]roblem [sS]olv", Description),
         presentation = grepl("[ ,][pP]resentations?[ ,]|[ ,][pP]resent[ ,]|[ ,][pP]ublic [sS]peak", Description),
         teamwork = grepl("[ ,][tT]eamwork[ ,]|[ ,][tT]eam[ ,]", Description),
         creativity = grepl("[ ,][cC]reativity[ ,]|[ ,][cC]reative[ ,]", Description),
         critical = grepl("[ ,][cC]ritical [tT]hink|[ ,][tT]hink [cC]ritical", Description),
         curiosity = grepl("[ ,][cC]urious[ ,]|[ ,][cC]uriosity[ ,]", Description),
         adaptability = grepl("[ ,][aA]dapt", Description))

softskills_all <- listings %>%
  summarize(Communication = mean(communication),
            Leadership = mean(leadership),
            `Problem Solving` = mean(solving),
            Presentation = mean(presentation),
            Teamwork = mean(teamwork),
            Creativity = mean(creativity),
            `Critical Thinking` = mean(critical),
            Curiosity = mean(curiosity),
            Adaptability = mean(adaptability)) %>%
  gather(softskill, proportion, Communication, Leadership, `Problem Solving`, Presentation, Teamwork, Creativity, `Critical Thinking`, Curiosity, Adaptability)

softskills_all %>% ggplot(aes(x = reorder(softskill,-proportion), y = proportion)) +
  geom_bar(stat = "identity", aes(fill = softskill)) +
  labs(x = "Soft Skills", y = "Proportion Mentioned", title = "Top Data Science Soft Skills") +
  theme_light() +
  theme(legend.position = "none")

softskills <- listings %>%
  group_by(Position) %>%
  summarize(Communication = mean(communication),
            Leadership = mean(leadership),
            `Problem Solving` = mean(solving),
            Presentation = mean(presentation),
            Teamwork = mean(teamwork),
            Creativity = mean(creativity),
            `Critical Thinking` = mean(critical),
            Curiosity = mean(curiosity),
            Adaptability = mean(adaptability)) %>%
  gather(softskill, proportion, Communication, Leadership, `Problem Solving`, Presentation, Teamwork, Creativity, `Critical Thinking`, Curiosity, Adaptability, -c(Position))

softskills %>% ggplot(aes(x = reorder(softskill,-proportion), y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Position)) +
  labs(x = "Soft Skills", y = "Proportion Mentioned", title = "Top Data Science Soft Skills by Position") +
  theme_light()



# search for education requirements
listings <- listings %>%
  mutate(bs = grepl("[ ,][bB].?[sS].?[ ,]|[ ,][bB]achelor'?s?[ ,]", Description),
         ms = grepl("[ ,][mM].?[sS].?[ ,]|[ ,][mM]aster'?s?[ ,]", Description),
         phd = grepl("[ ,][pP].?[hH].?[dD].?[ ,]|[ ,][dD]octor[ ,]|[ ,][dD]octorate[ ,]", Description))

degrees_all <- listings %>%
  summarize(BS = mean(bs),
            MS = mean(ms),
            PhD = mean(phd)) %>%
  gather(degree, proportion, BS, MS, PhD)

degrees_all %>% ggplot(aes(x = reorder(degree,-proportion), y = proportion)) +
  geom_bar(stat = "identity", aes(fill = degree)) +
  labs(x = "Degrees", y = "Proportion Mentioned", title = "Top Data Science Degrees") +
  theme_light() +
  theme(legend.position = "none")

degrees <- listings %>%
  group_by(Position) %>%
  summarize(BS = mean(bs),
            MS = mean(ms),
            PhD = mean(phd)) %>%
  gather(degree, proportion, BS, MS, PhD, -c(Position))

degrees %>% ggplot(aes(x = reorder(degree,-proportion), y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Position)) +
  labs(x = "Degrees", y = "Proportion Mentioned", title = "Top Data Science Degrees by Position") +
  theme_light()
