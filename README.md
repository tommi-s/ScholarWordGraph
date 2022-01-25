---
title: "Scholar WordGraph"
author: "Tommi Suvitaival, Steno Diabetes Center Copenhagen, tommi.raimo.leo.suvitaival@regionh.dk"
date: "2022-01-25"
output: 
  html_document:
    keep_md: true
    fig_width: 8
    fig_height: 8
    dev: jpeg
    toc: yes
  github_document:
    fig_width: 8
    fig_height: 8
    dev: jpeg
    toc: yes
always_allow_html: true
---

# Introduction

This is a tutorial script to producing an interactive visualization of words in a publication list extracted from [Google Scholar](https://scholar.google.com/). The appearance of the words is dependent on the frequency of their occurrence, and on which words they co-occur with in the titles of the listed articles.

The code is for R and it is, among others, based on packages [ggplot2](https://ggplot2.tidyverse.org/), [plotly](https://plotly.com/r/) and [htmlWidgets](https://www.htmlwidgets.org/).

This document is part of the Github repository [ScholarWordGraph](https://github.com/tommi-s/ScholarWordGraph) by [Tommi Suvitaival](https://tommi-s.com/).

View this document at https://tommi-s.com/ScholarWordGraph/ to show all output correctly.

<iframe 
  id="igraph" 
  scrolling="no" 
  style="border:none;" 
  seamless="seamless" 
  src="output-dark/index.html" 
  height="800" 
  width="800"
>
widget
</iframe>

# Preparations

1. Open a Google Scholar profile with browser (e.g., https://scholar.google.com/citations?user=SGgP0VQAAAAJ ).
1. Go to the bottom of the page and click "SHOW MORE" to show all entries on the page.
1. Go again to the bottom of the newly-expanded page.
1. Select everything in the table, starting from the year of the last entry (on the bottom-right corner) and ending at the title of the first entry (on the top-left corner).
1. Copy the selected text.
1. Paste the text to a text editor and save as "data/publications.txt" (see [example](https://github.com/tommi-s/ScholarWordGraph/blob/main/data/publications.txt) in the present repository).

# Load the Data


```r
data.loaded <- 
  readr::read_delim(
    file = "data/publications.txt",
    delim = "\t",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )
```

```
## Rows: 96 Columns: 1
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: "\t"
## chr (1): X1
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# Format the Data

## Create a Data Frame of Articles

* Create a matrix with articles as rows and title, authors and journal as the three columns.
* Convert to a data frame.


```r
data <- unlist( data.loaded )
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```r
data <-
  matrix(
    data = unlist( data ),
    ncol = 3,
    byrow = TRUE
  )

colnames( data ) <-
  c(
    "Title",
    "Authors",
    "Journal"
  )

data <-
  data.frame(
    data,
    stringsAsFactors = FALSE
  )
```

## Convert Titles to Lower Case

* To match identical words with different case, convert all to lowercase.


```r
data$"Title.lower" <-
  tolower( data$"Title" )
```

## Define Multi-Word Terms

* Define word sequences that should be considered as a single term by adding replacing whitespace with underscore.
* E.g., "type 1 diabetes" is one term.


```r
data$"Title.lower" <-
  stringr::str_replace_all(
    string = data$"Title.lower",
    # pattern = "type 1 diabete",
    pattern = "type 1 diabetes",
    replacement = "type_1_diabetes"
  )

data$"Title.lower" <-
  stringr::str_replace_all(
    string = data$"Title.lower",
    pattern = "type 2 diabetes",
    replacement = "type_2_diabetes"
  )
```

## Omit Special Characters

* Omit punctuation and special characters from the titles.


```r
words <-
  stringr::str_split(
    string = data[ , "Title.lower" ],
    pattern = "\\s"
  )

words <- 
  lapply(
    X = words,
    FUN = tolower
  )

words <-
  lapply(
    X = words,
    FUN = stringr::str_replace_all,
    pattern = "(\\()|(\\))|(\\:)|(\\,)|(\\.)",
    replacement = ""
  )

table.words <- 
  sort(
    x = table( unlist( words ) ),
    decreasing = TRUE
  )

head( table.words )
```

```
## 
##   in   of  and  the with  for 
##   25   21   17   13   11    8
```

## Extract Unique Words

* List all unique words that appear in the titles.


```r
words.unique <-
  sort(
    unique(
      unlist( words ) )
  )
```

## Define List of Blocked Words

* List general words that are not of interest.
* Add plain numbers to this list.


```r
blocklist <-
  c(
    "and",
    "an",
    "after",
    "are",
    "as",
    "at",
    "based",
    "by",
    "do",
    "during",
    "for",
    "from",
    "in",
    "is",
    "of",
    "on",
    "not",
    "the",
    "to",
    "through",
    "with",
    "without",
    "...",
    letters
  )

blocklist <- 
  c(
    blocklist,
    words.unique[ grepl( x = words.unique, pattern = "^[0-9]+$" ) ]
  )
```

## Omit Blocked Words

* Omit the blocked words from the words list of each article.


```r
words <-
  lapply(
    X = words,
    FUN = function( x ) {
      x[ !( x %in% blocklist ) ]
    }
  )

table.words <- 
  sort(
    x = table( unlist( words ) ),
    decreasing = TRUE
  )

head( table.words )
```

```
## 
## type_1_diabetes        analysis      associated          plasma            data 
##               6               5               5               5               4 
##      lipidomics 
##               4
```

# Re-Extract Unique Words


```r
words.unique <-
  sort(
    unique(
      unlist( words ) )
  )
```

# Create the Words-by-Articles Occurrence Matrix

* Matrix that has value 1 if the word (row) appears in the article (column) and 0 otherwise.


```r
wba <-
  array(
    data = 0,
    dim = c( length( words.unique ), nrow( data ) )
  )

rownames( wba ) <- words.unique
colnames( wba ) <- data[ , "Title" ]

for ( i in 1:nrow( data ) ) {
  
  wba[ words[[ i ]], i ] <- 1
  
}
```

## Log-Transform the Occurrences

* Trasform the occurrences with log (1+x) for a more Gaussian-like distribution.


```r
wba.log1px <- log10( 1 + wba )
```

## Autoscale the Occurrence Profiles

* Scale the data for PCA.


```r
wba.norm <- scale( wba.log1px )
```

# Principal Component Analysis

## Compute PCA


```r
result.pca <-
  princomp(
    x = wba.norm
  )
```

## Plot PCA


```r
library( "ggfortify" )
```

```
## Warning: package 'ggfortify' was built under R version 4.0.5
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```r
plot <-
  autoplot( 
    result.pca
    # ,
    # loadings = TRUE,
    # loadings.label = TRUE
  )

print( plot )
```

![](README_files/figure-html/PCA-Plot-1.jpeg)<!-- -->

# Cluster the Words

* Using a fixed number of nine clusters for a suitable visual representation with recognizable colors.


```r
result.clustering <-
  cluster::pam(
    x = result.pca$scores[ , 1:2 ], # based on PCA scores
    # x = wba.norm, # alternatively, based on original high-dimensional data
    k = 9
  )
```

# Figure

## Prepare Results for the Visualization

* Extract PCA-scores of the two first components.
* Extract cluster assignments for each data point.
* Compute each data point's Euclidean distance from the origin.
* Wrap multi-word terms to multiple lines for better visual impression.
* Create a text string listing all the article titles, where the term appears.


```r
data.plot <-
  data.frame(
    Word = rownames( result.pca$"scores" ),
    result.pca$"scores"
  )

data.plot$"Cluster" <- 
  factor(
    x = result.clustering$"clustering",
    levels = 1:length( unique( result.clustering$"clustering" ) ),
    labels = 
      c(
        "Cluster (C) 1",
        paste( "C", 2:length( unique( result.clustering$"clustering" ) ) )
      )
  )
  
data.plot$"Distance" <- 
  sqrt( data.plot$"Comp.1"^2 + data.plot$"Comp.2"^2 ) + 1

data.plot$"Term" <-
  stringr::str_replace_all(
    string = data.plot$"Word",
    pattern = "\\-",
    replacement = "-\n"
  )

data.plot$"Term" <-
  stringr::str_replace_all(
    string = data.plot$"Word",
    pattern = "\\_",
    replacement = "\n"
  )

data.plot$"Publications" <-
  apply(
    X = wba[ data.plot$"Word", ] == 1,
    MAR = 1,
    FUN = function( x ) {
      paste(
        "\n",
        names(
          which( x )
        ),
        collapse = "\n"
      )
    }
  )

data.plot <- 
  data.plot[ 
    order( data.plot$"Distance", decreasing = FALSE ),
  ]
```

## Define the Figure

* Visualization with ggplot2.
* PCA components 1 and 2, respectively, on the y- and x-axes.
* Term name as text label.
* Distance from origin as (increasing) size.
* Cluster assignment as color.
* Add jitter to reduce overlap of similar terms.
* Use pseudo-log-valued axes for reduced outliers.


```r
jitter <- 1

plot <-
  ggplot2::ggplot(
    data = data.plot,
    mapping =
      ggplot2::aes(
        x = Comp.2,
        y = Comp.1,
        label = Term,
        size = Distance,
        text = Publications
        ,
        color = Cluster,
      )
  ) +
  ggplot2::geom_text(
    position =
      ggplot2::position_jitter(
        height = jitter,
        width = jitter
      )
  ) +
  ggplot2::scale_x_continuous( trans = "pseudo_log" ) +
  ggplot2::scale_y_continuous( trans = "pseudo_log" ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::xlab( label = "\n\n\nMap of my research topics" ) +
  ggplot2::ylab( label = "" ) +
  ggplot2::scale_color_brewer( palette = "Set1" )
```

## Show the (Passive) Figure


```r
plot
```

![](README_files/figure-html/Figure-Show-1.jpeg)<!-- -->

## Create the Interactive Figure

* Make ggplot2 figure interactive with the **ggplotly**-function from the **plotly**-package.
* Show the term and the articles, where it appears, as text (tooltip) when hovering over a data point.
* Use the layout-function for fine-tuning of the legend.


```r
library( tidyr )

plot.interactive <-
  plotly::ggplotly( 
    p = plot,
    tooltip = c( "label", "Publications" )
  ) %>%
  plotly::layout(
    legend = 
      list(
        orientation = "h",
        title = list( text = "Cluster<br>" ),
        x = quantile( x = data.plot$"Comp.2", probs = 0.67 ),
        y = min( data.plot$"Comp.1" )
      )
  )
```

## View the Interactive Figure

* Words are shown on the map in increasing size with regard their PCA-score.
* Hover over a word to reveal, which article titles it occurs in.
* Clusters shown with distinct colors.
* Double-click on a cluster (Cs) in the legend to show it alone (and double-click again to return).
* Single-click on a cluster to hide it (and click again to return.
* Select area to zoom in the map (and double click the map area to return).


```r
plot.interactive
```

```{=html}
<div id="htmlwidget-7aff51991069093d3494" style="width:768px;height:768px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-7aff51991069093d3494">{"x":{"data":[{"x":[-0.9014495282715,-1.30716090018878,0.0568642320761639,-0.791302468842064,0.232365709049459,0.0721518106967913,-0.666441847788176,0.103650274160238,-0.691712686684887,-1.3901077297692,-0.433449264912758,-1.32305878011406,0.43589043913513,-0.0175917696709765,-0.0275811103004237,0.246524561027137,-1.25591012386784,-1.66034124596707,-0.385435072152982,0.128461898391028,-1.58049927084438,-0.734251039856739,-0.896271535483814,0.191606225623124,-0.522719912519373,-0.616545971305735,0.340696560784079,-1.39243344031102,-1.08846991159902],"y":[-0.506694710762579,1.22922234381981,0.29193027927972,-0.16515773017459,-0.154554244545627,1.22920703481964,1.01233953624758,1.08718676252482,-0.793318157434319,1.2041521970808,0.277847004558099,0.723238551958603,0.102981818343651,-0.509266176257738,0.750084239244051,0.682187139708138,-0.321585104149246,0.0918328705463907,0.119944421503355,0.0600646179389557,0.863134347762575,-0.448692627098006,0.118348852418769,0.743257774658907,0.665880059510875,-1.00580071502883,-0.237941477568974,-0.278683216409422,0.783262876497497],"text":["adolescent","characterization","dipis","increased","subjects","…","individuals","stratification","trial","albuminuria","gut","microbiota","selected","diabetic","circulating","lipids","retinopathy","abnormalities","pathogenesis","quantitative","review","all-cause","mortality","phosphatidylcholine","species","sphingomyelin","profile","cohort","lipidomics"],"hovertext":["Term: adolescent<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: characterization<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: dipis<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: increased<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: subjects<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: …<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: individuals<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: stratification<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: trial<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: albuminuria<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: gut<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: microbiota<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: selected<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: diabetic<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: circulating<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: lipids<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: retinopathy<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: abnormalities<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: pathogenesis<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: quantitative<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: review<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: all-cause<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: mortality<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: phosphatidylcholine<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: species<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: sphingomyelin<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: profile<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: cohort<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: lipidomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[8.67900209830133,8.67900209830133,8.67900209830133,8.67900209830133,8.67900209830133,8.92414011734841,8.9608783809658,9.00209143430284,9.01741614892878,9.24618494461737,9.24618494461737,9.24618494461737,9.24618494461737,9.27544247860767,9.79436965627137,9.79436965627137,9.79436965627137,10.0227562300786,10.0227562300786,10.0227562300786,10.0227562300786,10.2433622158286,10.2433622158286,10.2433622158286,10.2433622158286,10.2433622158286,10.6006891089719,10.6258578201705,10.7115110536004],"color":"rgba(228,26,28,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"Cluster (C) 1","legendgroup":"Cluster (C) 1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.699523104979662,0.773730129775586,1.10099099374283,-0.159149037833331,0.614999671307387,0.139604583223345,-0.468170360355644,0.416280146528961,1.1095013511687,0.166795619329762,-0.402533301120299,-0.294147172910768,-0.220961641953933,-0.198156536059129,0.834317155680013,0.0990288202039421,-0.826899205768156,-0.00452740487237571,0.115014931379939,-0.292798288348226,0.108399318815114,-0.45829142787969,-0.720949209942578,1.07148591131428,1.00629726639474,-0.620721547234084,-0.660973393193658,-0.274655707947191,-0.702952413588794,0.284705327123372,0.785879421744464,-0.17418609769273,0.512313856974213,-0.552814476426722,0.257156681966005,-0.0638536695643273,0.692321637061337,0.489528445408592,-0.547482985655865,-0.0887807236305383,0.099943200473586,0.112944756050507,-0.741479996566919,-0.566626750640498,1.14505277084194,-0.289739225301819,1.19608586071066,0.344508892614467,-0.627646457180308,0.783582437075747,1.23558772843338,0.699507838804348,-0.675158611803558,0.400780931741689,1.19946388590405,0.0365710008635561,1.01741838378156,1.19183118023163,-0.422053516039641,0.667915570886789,-0.0274525414534738,0.941266894402272,0.29622766160269,1.07826528576066,0.89338889828746,1.40029047121157,0.796386027682473,0.2464355656669,-0.00954188101212583,1.20599792504464,0.859181626492784,-0.401824532749458,-0.242621217862137,0.728076025835805,0.834792201399438,-0.028216852839407,1.0136275767394,0.0310619751664253,-0.0141141104265388],"y":[-0.163576430586508,1.04088217304892,-0.811663175412795,0.245922830010423,-0.248476023731163,-0.656757497174075,0.0984950047532411,0.602187580483625,1.10012537507965,-0.742241779466431,0.193918332799066,0.80368297441556,0.37459969834118,-0.342113729224812,0.293173379606296,-0.217126021285813,1.11998998454465,0.364657612393339,0.119455116121208,-0.287058385175358,0.861536428246011,0.43048121678393,1.18238269203093,0.929181542161216,0.0470462673411861,1.06082295069793,1.11252073107609,0.22709163280526,-0.464415794787833,0.132112166760585,-0.676686959424892,0.00408520308332777,1.12309725574227,0.990022921851001,-0.193474534051397,-0.101938074357447,-0.0898839141201344,0.0660547889809939,0.179364600781712,0.820201811477903,-0.123741602893558,-0.148061629753199,0.529168608886708,-0.381790781833632,-0.317896225463999,1.05773360657279,-0.560051702868594,0.557857994481521,-0.136982901927484,0.252554412025558,-0.61335737162496,-0.0956992292519938,-1.10411363742849,-0.382688590749855,0.63175764358068,-0.362312434665616,0.0272888504380928,0.845106993436754,0.746678512643963,-0.127275968335628,1.3773150921855,1.21589053217879,0.752705430766456,-0.39362908418674,-0.730046808099486,1.1331562294307,-0.28241146620539,-0.661710464294168,-0.0211223847149939,-0.661399098477426,-0.314501023749921,-0.007494611098531,0.153009458224249,-0.21207315869354,-0.649728878621984,0.618381874315431,-0.377723874791442,0.354639157654924,0.0667198242227061],"text":["collected","cryogenically","describing","fecal","healthy","metabolome","participants","samples","blood-based","childhood","early","evidence","experiences","integrated","later","point","preceding","proteomics","lipidome","associates","co-morbidities","first-episode","metabolic","psychosis","serum","level","lipidomer","understanding","adolescents","alspac","children","disorder","identification","parents","signature","applications","diagnostic","disorders—the","medicine","metsy","project","research","phenotypic","account","glucose","high","inter-individual","intervention","lifestyle","responses","tolerance","variability","compound","correlations","metabolomics","multiple","peaks","patients","profiling","targeted","avon","longitudinal","atlas","heart","mouse","postnatal","systems","finnish","men","predictive","progression","tool","mass","multi-peak","spectral","platform","metabolite","psychotic","development"],"hovertext":["Term: collected<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: cryogenically<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: describing<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: fecal<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: healthy<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: metabolome<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: participants<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: samples<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: blood-based<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: childhood<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: early<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: evidence<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: experiences<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: integrated<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: later<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: point<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: preceding<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: proteomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: lipidome<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: associates<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: co-morbidities<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: first-episode<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: metabolic<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: psychosis<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: serum<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: level<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: lipidomer<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: understanding<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: adolescents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: alspac<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: children<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: disorder<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: identification<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: parents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: signature<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: applications<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: diagnostic<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: disorders—the<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: medicine<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: metsy<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: project<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: research<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: phenotypic<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: account<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: glucose<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: high<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: inter-individual<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: intervention<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: lifestyle<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: responses<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: tolerance<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: variability<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: compound<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: correlations<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: metabolomics<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: multiple<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: peaks<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: patients<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: profiling<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: targeted<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: avon<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: longitudinal<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: atlas<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: heart<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: mouse<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: postnatal<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: systems<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: finnish<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: men<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: predictive<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: progression<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: tool<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: mass<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: multi-peak<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: spectral<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: platform<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: metabolite<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: psychotic<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: development<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …"],"textfont":{"size":[6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.50544941584095,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.68963511276678,6.68963511276678,6.68963511276678,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.5540948874866,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.7747130133892,7.7747130133892,7.7747130133892,7.7747130133892,7.7747130133892,7.79729215409558,7.79729215409558,7.79729215409558,8.36330791850921,8.36330791850921,8.39404138157446,8.39404138157446,8.39404138157446,8.39404138157446,8.57393433506607,9.04435097450964,9.04435097450964,9.04435097450964,9.04435097450964,9.04435097450964,9.08424340947336,9.08424340947336,9.08424340947336,9.30190863595287,9.30324412776946,9.63548771832759,9.98074916930306],"color":"rgba(55,126,184,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 2","legendgroup":"C 2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.294519818046842,0.5653634675464,0.432708070954528,-0.385571411469732,0.829798998652246,0.893655601764317,0.172456366816755,-0.878077573733185,0.659751160411384,-0.825230919880573,0.280067735970404,0.203176466901239,0.242814553123351,0.579308685068425,-0.395238344870124,0.633652380355086,0.984755392805126,-0.194641788802657,-0.752171454332385,-0.045328160154114,0.115530014631858,-0.98486815752689,0.895082476676961,-0.699451672250817,-0.8871705577359,0.0398608098156354,0.307097508427259,-0.673207227285449,-0.293311507120226,0.142076646298673,-0.55489990455607,0.207355216932055,-0.950179519742224,0.563353089005258,0.23492802897891,0.702240096726026,-0.673600556008015,-0.792158609400932,-0.828854025020693,0.757534234174099,-0.171998516801242,0.0243001738329036,0.682744758645873,-0.839781149779578,0.689897260605226,0.458685306343119,-0.149576699850281,-0.149931992756729,0.347650688720341,-1.14109238877759,0.187275035885568,-1.122900775055,-0.752480999453519,-0.638328917677109,-0.372269548362647,-0.514850604234789,-0.96078733948857,0.610415763334776,-0.533298415950094,-0.136690190101037],"y":[-0.347391881876401,-0.739519146167449,0.225216301261671,-0.432128057245873,1.12153218790073,-0.405285676372014,0.325191523052044,-0.724554059923403,-0.727697943233661,-0.106943638891689,0.108142755432723,-0.548212860272528,-0.48566524966553,0.276331755305378,0.492659711251704,-0.797151260105224,-0.404668998000474,0.293964506565331,-0.802773509201856,-0.381727545098646,-0.730326516595913,0.909761547068821,0.425208297515975,-0.743699114811851,-0.250594713301642,0.957742860032933,-0.00517975517746158,1.17743726049352,1.00263042070705,-0.00496002032619067,0.129157896831519,-0.673629988703989,-0.439141353845709,1.0994073941632,0.941764483440555,-0.700890546452367,-0.653439106207454,0.958367626860625,-0.17019555417087,0.433574135627485,0.39272766954068,0.167328746335714,-0.59226471344182,0.0202299298701275,1.17662636777213,0.675409033706545,1.15335578638887,-0.320992738990216,0.0944788869897734,-0.360648757828503,-0.777520743714749,0.160807438610018,0.483692183574558,0.44773527136621,0.00823691528838982,-0.00546259018975387,-0.705165034676074,-0.503255019946127,0.206711162762613,-0.0528241068690629],"text":["biopsies","deregulation","function","graft","liver","pathway","pre-transplant","purine","survival","transplantation","alcohol","ald","comprehensive","differences","hepatic","intoxication","lipid","nafld","turnover","adipose","areas","between","body","diversity","human","tissue","alzheimer's","amides","amyloid","brain","burden","disease","european","fatty","framework","hippocampal","information","medical","memory","primary","volume","cimt","copenhagen","effect","insulin","metformin","therapy","acids","alteration","amino","assessment","branched","chain","discovery","future","metabolomic","persons","polyols","present","risk"],"hovertext":["Term: biopsies<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: deregulation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: function<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: graft<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: liver<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pathway<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pre-transplant<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: purine<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: survival<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: transplantation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: alcohol<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: ald<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: comprehensive<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: differences<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: hepatic<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: intoxication<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipid<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: nafld<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: turnover<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: adipose<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: areas<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: between<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: body<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: diversity<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: human<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: tissue<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: alzheimer's<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amides<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amyloid<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: brain<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: burden<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: disease<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: european<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: fatty<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: framework<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: hippocampal<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: information<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: medical<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: memory<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: primary<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: volume<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: cimt<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: copenhagen<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: effect<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: insulin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: metformin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: therapy<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: acids<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: alteration<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: amino<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: assessment<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: branched<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: chain<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: discovery<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: future<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: metabolomic<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: persons<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: polyols<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: present<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: risk<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,8.19677691281695],"color":"rgba(77,175,74,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 3","legendgroup":"C 3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.82748493950443,-0.171481091573523,0.791743223640334],"y":[-1.83131017626965,-2.33027197300606,-2.23184554009432],"text":["analysis","data","multi-way"],"hovertext":["Term: analysis<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Cross-organism toxicogenomics with group factor analysis<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: data<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: multi-way<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Graphical multi-way models<br /><br /> Cross-species translation of multi-way biomarkers<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology"],"textfont":{"size":[19.2684559993431,20.2549018723122,22.6771653543307],"color":"rgba(152,78,163,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 4","legendgroup":"C 4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.30811698790946,-1.13250636086174,-1.0324429145657,-0.183387439288784,-1.96395017934151,-0.706173554916687,-1.35686134566016,-2.42432261068695],"y":[0.697072385713528,-0.100770596045931,0.393753312233273,-0.648420812050248,0.951636819765859,0.755174686821774,0.936374905706041,-0.0369937213610495],"text":["impairment","renal","metabolites","reveals","lipidomic","plasma","associated","type<br />1<br />diabetes"],"hovertext":["Term: impairment<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: renal<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: metabolites<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: reveals<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipidomic<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: plasma<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: associated<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: type<br />1<br />diabetes<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[11.7338996139862,11.7338996139862,12.6656296655831,12.8164634648796,13.5005737442335,13.5327992522234,15.4318806572183,19.6199155085839],"color":"rgba(255,127,0,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 5","legendgroup":"C 5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.570970046287077,0.0173909943081911,0.0491319461256738,0.0106295261155299,-0.670680249333806,-0.66040811337573,-0.216050239235666,0.219346641607079,1.29487090840354,0.963910206718145,0.339930691874462,-0.74237150780217,1.05632564503506,0.268221515097798,0.154706155623288,0.538619606062461],"y":[-0.617848687987019,-1.50751272653713,-0.717370716105391,-1.05528322399217,-1.51876616595868,-0.841375943986657,-0.195034903371652,-1.90089781478973,-1.09546219522269,-0.461010370050977,-1.99552192824611,-0.792151116828411,-1.65244096307943,-1.31926697925692,-1.4703014471797,-1.14197055723079],"text":["findings","modeling","stronger","collinear","high-dimensional","two-way","biomarkers","cross-species","biology","computational","multi-source","multivariate","graphical","bayesian","translation","models"],"hovertext":["Term: findings<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: modeling<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: stronger<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: collinear<br /><br /> Two-way analysis of high-dimensional collinear data","Term: high-dimensional<br /><br /> Two-way analysis of high-dimensional collinear data","Term: two-way<br /><br /> Two-way analysis of high-dimensional collinear data","Term: biomarkers<br /><br /> Cross-species translation of multi-way biomarkers","Term: cross-species<br /><br /> Cross-species translation of multi-way biomarkers","Term: biology<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: computational<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: multi-source<br /><br /> Multivariate multi-way analysis of multi-source data","Term: multivariate<br /><br /> Multivariate multi-way analysis of multi-source data","Term: graphical<br /><br /> Graphical multi-way models","Term: bayesian<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: translation<br /><br /> Cross-species translation of multi-way biomarkers<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: models<br /><br /> Graphical multi-way models<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology"],"textfont":{"size":[10.8982491808933,10.8982491808933,10.8982491808933,11.104557149373,11.104557149373,11.104557149373,12.2744429228418,12.2744429228418,12.5437114641505,12.5437114641505,13.0856658439002,13.0856658439002,13.4120300727054,13.674224371609,16.4120163170569,17.208630020291],"color":"rgba(255,255,51,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 6","legendgroup":"C 6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.0404303416625887,-0.90276167193083,-0.495080344151984,-1.25559742353512,-0.246329412102978,0.281022871605774,0.235038938227763,0.434752583265026,-0.308487053894384,0.117095240457969,-0.773223956980688,0.317442570879178],"y":[0.956271181035236,-1.0371723013714,0.123517616952954,-1.03510350843594,-0.961279567495735,-0.114312255454065,0.16773832945451,0.823960769291221,-0.0582324704004614,-0.772714152451435,0.477210941273889,-0.240335154797712],"text":["changes","carbohydrate","crossover","diet","following","low","post‐hoc","randomized","cross-organism","factor","group","toxicogenomics"],"hovertext":["Term: changes<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: carbohydrate<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: crossover<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: diet<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: following<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: low<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: post-hoc<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: randomized<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: cross-organism<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: factor<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: group<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: toxicogenomics<br /><br /> Cross-organism toxicogenomics with group factor analysis"],"textfont":{"size":[7.44951416531748,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,8.74649302849887,8.74649302849887,8.74649302849887,8.74649302849887],"color":"rgba(166,86,40,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 7","legendgroup":"C 7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.29420277718015,1.82656783591943,1.35440186692972,1.93056978128989],"y":[-0.456155795475676,0.00168529865454103,0.970571762963842,1.36972936153111],"text":["imi-rhapsody","study","clinical","type<br />2<br />diabetes"],"hovertext":["Term: imi-rhapsody<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: study<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: clinical<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: type<br />2<br />diabetes<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study"],"textfont":{"size":[14.7100211477783,15.1913643777023,15.4703955062867,16.6909245850854],"color":"rgba(247,129,191,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 8","legendgroup":"C 8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.15151196645164,1.11742082723939,0.98509899500349,-0.0547817286889869,1.59638333182534,0.244751076695828,0.443549679608403,1.17415738992145,2.06678569038303],"y":[1.07597610822092,-0.301015162354749,-0.339966710636806,-0.373581750763994,0.261269151741926,0.990938675632987,0.0356803080498315,-0.603003653188881,1.32749517086348],"text":["clusters","distinct","people","signatures","cross-validation","replication","subtypes","variables","molecular"],"hovertext":["Term: clusters<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: distinct<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: people<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: signatures<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: cross-validation<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: replication<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: subtypes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: variables<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: molecular<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study"],"textfont":{"size":[11.3786268573125,11.3786268573125,11.3786268573125,11.3786268573125,11.4471158713255,11.4471158713255,11.4471158713255,11.4471158713255,12.8242526914013],"color":"rgba(153,153,153,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 9","legendgroup":"C 9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.3059360730594,"r":7.30593607305936,"b":25.5707762557078,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.64887802574045,2.29134110543653],"tickmode":"array","ticktext":["-10","-5","0","5"],"tickvals":[-2.31243834127275,-1.6472311463711,0,1.6472311463711],"categoryorder":"array","categoryarray":["-10","-5","0","5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"<br /><br /><br />Map of my research topics","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.51565132626563,1.56269444544508],"tickmode":"array","ticktext":["-12","-8","-4","0","4"],"tickvals":[-2.49177985264491,-2.0947125472611,-1.44363547517881,0,1.44363547517881],"categoryorder":"array","categoryarray":["-12","-8","-4","0","4"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":-11.7902098622378,"orientation":"h","title":{"text":"Cluster<br>"},"x":0.458383569834937},"annotations":[{"text":"Distance<br />Cluster","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"401c2854404c":{"x":{},"y":{},"label":{},"size":{},"text":{},"colour":{},"type":"scatter"}},"cur_data":"401c2854404c","visdat":{"401c2854404c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Export HTML Widget

* Create a stand-alone html-widget of the figure with the **partial_bundle** function from the **plotly**-package.
* Save the widget with the **saveWidget**-function from the **htmlWidgets**-package.

(Uncomment for saving the files.)


```r
widget <- plotly::partial_bundle( plot.interactive )

# dir.create( "output" )
# 
# htmlwidgets::saveWidget( widget, "output/index.html" )
```

# View the Exported Widget

* View the exported (saved) stand-alone widget with in an **iframe** in a html-document.

```text

<iframe 
  id="igraph" 
  scrolling="no" 
  style="border:none;" 
  seamless="seamless" 
  src="output/index.html" 
  height="800" 
  width="800"
>
widget
</iframe>

```

<iframe 
  id="igraph" 
  scrolling="no" 
  style="border:none;" 
  seamless="seamless" 
  src="output/index.html" 
  height="800" 
  width="800"
>
widget
</iframe>

# Figure with a Dark Layout

## Define the Figure

* Create the same figure with a dark layout, suitable for dark webpages.


```r
plot.dark <-
  plot +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_text( color = "gray" ),
    legend.background = ggplot2::element_rect( color = NA, fill = "black" ),
    legend.key = ggplot2::element_rect( color = "gray",  fill = "gray" ),      
    legend.text = ggplot2::element_text( color = "gray" ),
    legend.title = ggplot2::element_text( color = "gray" ),
    panel.background = ggplot2::element_rect( fill = "black", color  = NA ),
    panel.grid = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect( color = "black", fill = "black" )
  )
```

## Show the (Passive) Figure


```r
plot.dark
```

![](README_files/figure-html/Figure-Dark-Show-1.jpeg)<!-- -->

## Create the Interactive Figure


```r
library( tidyr )

plot.dark.interactive <-
  plotly::ggplotly( 
    p = plot.dark,
    tooltip = c( "label", "Publications" )
  ) %>%
  plotly::layout(
    legend = 
      list(
        orientation = "h",
        title = list( text = "Cluster<br>" ),
        x = quantile( x = data.plot$"Comp.2", probs = 0.67 ),
        y = min( data.plot$"Comp.1" )
      )
  )
```

## View the Interactive Figure


```r
plot.dark.interactive
```

```{=html}
<div id="htmlwidget-4f564cb8bbf247246030" style="width:768px;height:768px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-4f564cb8bbf247246030">{"x":{"data":[{"x":[-0.150074648419563,-1.38654034745896,-1.09031245925481,-0.38076178983802,0.488757490431722,0.344283497726987,-1.34611485873722,-0.772229262222616,-0.927599140335779,0.053229205479022,-1.36466248363153,-0.969134693330977,0.0470666291016302,-0.9746458100657,-0.75435166683227,-0.268307428510787,-1.05745056102231,-1.24594585309468,-0.431631187821845,-0.05618262654833,-0.583637976001124,-0.251429494758762,0.000390890697323121,-0.112784406924314,0.0916618427171636,0.109193381564997,-0.45101819549983,0.195324734320722,-1.3250011645718],"y":[0.642150335530412,0.462039977828633,0.293285433253866,-0.0559472878886048,-0.513330449764955,0.100164685063875,0.150919193548889,0.293074761910417,0.87593686312824,0.34129622435863,-0.0161390118606863,0.193555864474875,-0.652863201715845,0.225679862316242,0.805429958924241,-0.400961217284613,1.11067839264829,0.0809744865853688,-0.816325594276528,0.4270364473794,-0.0551776458289126,-0.646469566817921,-0.548000480360877,-0.17785257178877,0.446610918887365,-0.531390912212026,0.817976331682721,0.765424305940383,1.49776300152942],"text":["adolescent","characterization","dipis","increased","subjects","…","individuals","stratification","trial","albuminuria","gut","microbiota","selected","diabetic","circulating","lipids","retinopathy","abnormalities","pathogenesis","quantitative","review","all-cause","mortality","phosphatidylcholine","species","sphingomyelin","profile","cohort","lipidomics"],"hovertext":["Term: adolescent<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: characterization<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: dipis<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: increased<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: subjects<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: …<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: individuals<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: stratification<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: trial<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: albuminuria<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: gut<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: microbiota<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: selected<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: diabetic<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: circulating<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: lipids<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: retinopathy<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: abnormalities<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: pathogenesis<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: quantitative<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: review<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: all-cause<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: mortality<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: phosphatidylcholine<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: species<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: sphingomyelin<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: profile<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: cohort<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: lipidomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[8.67900209830133,8.67900209830133,8.67900209830133,8.67900209830133,8.67900209830133,8.92414011734841,8.9608783809658,9.00209143430284,9.01741614892878,9.24618494461737,9.24618494461737,9.24618494461737,9.24618494461737,9.27544247860767,9.79436965627137,9.79436965627137,9.79436965627137,10.0227562300786,10.0227562300786,10.0227562300786,10.0227562300786,10.2433622158286,10.2433622158286,10.2433622158286,10.2433622158286,10.2433622158286,10.6006891089719,10.6258578201705,10.7115110536004],"color":"rgba(228,26,28,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"Cluster (C) 1","legendgroup":"Cluster (C) 1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.455952976989565,-0.0914420445179623,1.19255974407214,-0.351830760641961,0.278417099107983,0.0814322059312779,0.953386208348902,-0.158126916895626,0.915672631034952,1.14327500111828,0.84017588946442,0.883770859366608,0.873055970793915,-0.0946013647743046,0.983300264956754,0.884054036842984,-0.0041442075424373,0.00819250478814246,0.81377943577909,-0.709789036489187,-0.0459111860907659,1.04500249056602,0.91989578800732,0.11129521187851,-0.14535651327586,0.19879077624229,-0.174968025948133,0.935964953318957,0.508849119532822,-0.664501974128218,0.86153489653763,-0.196309062887015,0.514657967489181,-0.608761955287339,0.990570496533035,-0.0308700442566481,0.251086932954291,0.310011202816764,0.514484772805968,0.341905444742093,-0.182063985268673,-0.117987005624076,-0.203700742193756,0.984718055687918,1.0096046205052,0.201146989818735,1.27413498213522,1.29590305615224,0.093638859912826,1.18051669210203,0.0303242111020519,0.148368403073473,-0.130054202701207,-0.0573868777856787,-0.406174477028902,-0.466025868269857,0.680545731608514,0.130416820628552,0.984436004294066,-0.222178007604571,1.24979333911679,0.51806868618942,1.33709252270733,0.649960732263073,-0.0881455506304955,0.52377513068591,-0.123157855149458,-0.36407300338368,0.810286249758349,1.25365173749407,0.76456218937193,0.0671749844007651,-0.394376788761832,-0.19042607537321,0.746088653329395,0.338665598504874,-0.158850146825552,1.28260838228486,0.794263171336121],"y":[0.0394683662416728,0.170914592741647,-0.338194532730958,0.549671692828307,-0.526253954918465,0.973093946518967,0.931922772022554,-0.0488994190854637,0.592145146212269,-0.0212846441504465,-0.0337493891206489,0.722250355283458,-0.470042159439336,-0.454532998272489,0.040823245762576,-0.33721615386502,1.02643433061122,-0.27624365327255,0.714880899312085,0.111349738116671,-0.629664760169278,-0.199854255274574,-0.443116018169264,0.666117211717476,0.587316540914883,0.383396101206036,0.0726082721012252,-0.659120685399084,1.22512157066735,0.0399851225410614,-0.629310487853267,0.796327045569292,0.126704024998567,0.110036518341026,0.315092813605538,-0.109644779838261,-0.671672679852333,1.03194707176125,1.01145535018704,0.152361221243057,-0.279103420111921,-0.146460014298197,0.346076682498266,-0.458295850057555,0.907027617588239,1.00125291992684,0.697610043014931,0.97152085185756,-0.422887498487188,0.516654260016577,0.248777441240447,0.518783660165565,0.0404868387098061,0.619615693152194,-1.15684288316941,0.320760071784501,-0.801018136046584,0.756791157748491,0.626903708193392,-0.30969462300843,-0.555475463340074,1.38224961404315,-0.625014228126146,0.257069645138346,-0.0545102821980789,0.695289294389896,-0.0218703074770881,1.09354096894049,0.238773094591585,0.95687080929884,-0.743378531086359,0.213530196444031,-0.689176584814715,-1.05820693516828,0.394804087712389,0.138411456398656,0.896703632281245,1.37151776068119,0.0777791028449757],"text":["collected","cryogenically","describing","fecal","healthy","metabolome","participants","samples","blood-based","childhood","early","evidence","experiences","integrated","later","point","preceding","proteomics","lipidome","associates","co-morbidities","first-episode","metabolic","psychosis","serum","level","lipidomer","understanding","adolescents","alspac","children","disorder","identification","parents","signature","applications","diagnostic","disorders—the","medicine","metsy","project","research","phenotypic","account","glucose","high","inter-individual","intervention","lifestyle","responses","tolerance","variability","compound","correlations","metabolomics","multiple","peaks","patients","profiling","targeted","avon","longitudinal","atlas","heart","mouse","postnatal","systems","finnish","men","predictive","progression","tool","mass","multi-peak","spectral","platform","metabolite","psychotic","development"],"hovertext":["Term: collected<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: cryogenically<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: describing<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: fecal<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: healthy<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: metabolome<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: participants<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: samples<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: blood-based<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: childhood<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: early<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: evidence<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: experiences<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: integrated<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: later<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: point<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: preceding<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: proteomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: lipidome<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: associates<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: co-morbidities<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: first-episode<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: metabolic<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: psychosis<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: serum<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: level<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: lipidomer<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: understanding<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: adolescents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: alspac<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: children<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: disorder<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: identification<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: parents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: signature<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: applications<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: diagnostic<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: disorders—the<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: medicine<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: metsy<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: project<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: research<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: phenotypic<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: account<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: glucose<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: high<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: inter-individual<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: intervention<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: lifestyle<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: responses<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: tolerance<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: variability<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: compound<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: correlations<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: metabolomics<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: multiple<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: peaks<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: patients<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: profiling<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: targeted<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: avon<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: longitudinal<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: atlas<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: heart<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: mouse<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: postnatal<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: systems<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: finnish<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: men<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: predictive<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: progression<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: tool<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: mass<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: multi-peak<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: spectral<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: platform<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: metabolite<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: psychotic<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: development<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …"],"textfont":{"size":[6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.10014971150858,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.28033846598011,6.50544941584095,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.511764177495,6.68963511276678,6.68963511276678,6.68963511276678,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.16262442080878,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.2121917950828,7.5540948874866,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.56532227254429,7.7747130133892,7.7747130133892,7.7747130133892,7.7747130133892,7.7747130133892,7.79729215409558,7.79729215409558,7.79729215409558,8.36330791850921,8.36330791850921,8.39404138157446,8.39404138157446,8.39404138157446,8.39404138157446,8.57393433506607,9.04435097450964,9.04435097450964,9.04435097450964,9.04435097450964,9.04435097450964,9.08424340947336,9.08424340947336,9.08424340947336,9.30190863595287,9.30324412776946,9.63548771832759,9.98074916930306],"color":"rgba(55,126,184,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 2","legendgroup":"C 2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.841864490756985,-0.732035838192109,-0.0597083267138937,0.0256841116270384,-0.853881016498212,-0.681067632635836,0.0337743250338695,0.737182766540196,0.915335218297537,-0.162797825069878,-0.271327757123474,0.996904604755726,-0.590870303880039,0.131985968131956,0.353554091103759,0.251862317942467,0.608110874578741,-0.971508005946491,0.707013344545301,-0.0074885621317127,-1.02768048072461,-1.06563052149717,0.484728222375324,-0.890422685875038,-0.326684633607457,0.581773397640844,-0.522459231461827,-0.427034941181038,-1.01541387024844,0.269924855646249,0.32837906418791,-0.907806537258867,-0.965779620833192,0.546207443666216,0.811510132946577,-0.752872449025009,-0.399386268253867,0.581401930045839,-0.876805565464739,0.341361671075728,-0.473769670497123,-0.0560338329757946,-0.777164953784217,0.764226170698981,-0.39813255427884,-0.495402859081646,0.213383298343847,0.287738083043541,-0.263162274731849,-0.152432550563072,0.46505649026617,-1.15795142331958,0.0312952907955602,-0.915569916720097,0.734762044270928,-1.07273652626068,-0.207343099991386,0.215393370596166,0.0543336343219832,-0.78283102534897],"y":[-0.120970643487535,0.592947483632006,-0.608053053159974,-0.857739906188808,-0.601492490050159,-0.112792793092094,0.628960583928265,-0.0440137734531981,0.662670935607739,1.07847235781256,-0.803759946515115,-0.417979828109296,-0.141259254914971,-0.63799887479216,0.161080224412231,0.983542025993316,-0.416238995761873,-0.415769835029991,-0.341914329705091,1.11594857597234,0.470046557909565,-0.573179272511287,0.305136911994533,0.690993745303469,-0.765526380897744,-0.651069818106724,0.730765077032036,-0.666085358962529,-0.640097154557818,-0.449102715376877,0.549050289884097,1.06046930442367,0.609061759643144,-0.0234198125750954,0.560216115534193,0.0407083097173636,-0.780289417386168,1.11328694338713,0.851128217976428,-0.661904728759193,-0.358414972454422,-0.281713819784911,-0.236005072669924,-0.662917801366986,0.980123724600433,-0.512397784462066,0.203139043225108,-0.468251939913361,0.893778134086997,-0.154399394573747,0.660573899229885,0.569007182909073,0.76307949232486,0.295537704551489,0.665977284261854,-0.315169083004921,0.798525494472952,0.200139000030131,-0.801158187029986,0.418933571911642],"text":["biopsies","deregulation","function","graft","liver","pathway","pre-transplant","purine","survival","transplantation","alcohol","ald","comprehensive","differences","hepatic","intoxication","lipid","nafld","turnover","adipose","areas","between","body","diversity","human","tissue","alzheimer's","amides","amyloid","brain","burden","disease","european","fatty","framework","hippocampal","information","medical","memory","primary","volume","cimt","copenhagen","effect","insulin","metformin","therapy","acids","alteration","amino","assessment","branched","chain","discovery","future","metabolomic","persons","polyols","present","risk"],"hovertext":["Term: biopsies<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: deregulation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: function<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: graft<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: liver<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pathway<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pre-transplant<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: purine<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: survival<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: transplantation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: alcohol<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: ald<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: comprehensive<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: differences<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: hepatic<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: intoxication<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipid<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: nafld<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: turnover<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: adipose<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: areas<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: between<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: body<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: diversity<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: human<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: tissue<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: alzheimer's<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amides<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amyloid<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: brain<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: burden<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: disease<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: european<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: fatty<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: framework<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: hippocampal<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: information<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: medical<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: memory<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: primary<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: volume<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: cimt<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: copenhagen<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: effect<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: insulin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: metformin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: therapy<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: acids<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: alteration<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: amino<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: assessment<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: branched<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: chain<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: discovery<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: future<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: metabolomic<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: persons<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: polyols<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: present<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: risk<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,4.67686510450572,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.2796184295226,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,5.64600560912639,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.23092615454284,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,6.86831884568736,8.19677691281695],"color":"rgba(77,175,74,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 3","legendgroup":"C 3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.2092367250249,-0.724743831481473,0.767050785563152],"y":[-2.78406628358879,-3.13266602737047,-1.99747139923939],"text":["analysis","data","multi-way"],"hovertext":["Term: analysis<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Cross-organism toxicogenomics with group factor analysis<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: data<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: multi-way<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Graphical multi-way models<br /><br /> Cross-species translation of multi-way biomarkers<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology"],"textfont":{"size":[19.2684559993431,20.2549018723122,22.6771653543307],"color":"rgba(152,78,163,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 4","legendgroup":"C 4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.294507666587163,-1.18263095254384,-1.97079391569053,-0.459879391417493,-0.965155353593897,-0.58591306807176,-2.43001276133976,-2.21212427035295],"y":[-0.196870341993798,-0.151637274868716,0.102180936544571,-0.478130168235297,-0.735018195453144,1.26566280719511,0.0668178328318822,0.721771191944591],"text":["impairment","renal","metabolites","reveals","lipidomic","plasma","associated","type<br />1<br />diabetes"],"hovertext":["Term: impairment<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: renal<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: metabolites<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: reveals<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipidomic<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: plasma<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: associated<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: type<br />1<br />diabetes<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort"],"textfont":{"size":[11.7338996139862,11.7338996139862,12.6656296655831,12.8164634648796,13.5005737442335,13.5327992522234,15.4318806572183,19.6199155085839],"color":"rgba(255,127,0,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 5","legendgroup":"C 5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.126008125440824,-0.240611684089852,0.923927067266452,-1.15521779445974,0.525618022083812,-0.775732165892101,-0.0918036917713847,-0.0673088295874562,1.30751728968113,-0.407690337361994,0.430483719917822,0.781918280876118,1.13144814440787,0.19919449077654,0.441081444474847,0.931852865736623],"y":[-1.12942560547515,-0.484016318521638,-1.39347467636241,0.148106563588646,-1.00356197754476,-0.377929355731192,-1.98538421689527,-1.50262399612921,-1.15274023741619,-0.433798457608979,-0.907421848799078,-0.962993242374598,-1.85728145786489,-1.87240323252072,-2.24807973087259,-1.88350688952514],"text":["findings","modeling","stronger","collinear","high-dimensional","two-way","biomarkers","cross-species","biology","computational","multi-source","multivariate","graphical","bayesian","translation","models"],"hovertext":["Term: findings<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: modeling<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: stronger<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: collinear<br /><br /> Two-way analysis of high-dimensional collinear data","Term: high-dimensional<br /><br /> Two-way analysis of high-dimensional collinear data","Term: two-way<br /><br /> Two-way analysis of high-dimensional collinear data","Term: biomarkers<br /><br /> Cross-species translation of multi-way biomarkers","Term: cross-species<br /><br /> Cross-species translation of multi-way biomarkers","Term: biology<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: computational<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: multi-source<br /><br /> Multivariate multi-way analysis of multi-source data","Term: multivariate<br /><br /> Multivariate multi-way analysis of multi-source data","Term: graphical<br /><br /> Graphical multi-way models","Term: bayesian<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: translation<br /><br /> Cross-species translation of multi-way biomarkers<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology","Term: models<br /><br /> Graphical multi-way models<br /><br /> Bayesian Multi-Way Models for Data Translation in Computational Biology"],"textfont":{"size":[10.8982491808933,10.8982491808933,10.8982491808933,11.104557149373,11.104557149373,11.104557149373,12.2744429228418,12.2744429228418,12.5437114641505,12.5437114641505,13.0856658439002,13.0856658439002,13.4120300727054,13.674224371609,16.4120163170569,17.208630020291],"color":"rgba(255,255,51,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 6","legendgroup":"C 6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.974383122938004,0.558129577658792,0.399599936086406,-0.455520074587594,-0.327247345127444,0.180566436372419,-0.773882047511916,-0.169986802857648,-1.33107160351141,-0.909215662562234,-0.000387042229009482,0.606332466538118],"y":[0.196652316640511,-0.50825917876959,-0.20381499470845,-0.794654693619163,0.536970166790484,0.487530996392666,0.606439895763247,0.53390883591324,0.332354463651946,0.319158014390788,-0.821456572025844,0.426292537260971],"text":["changes","carbohydrate","crossover","diet","following","low","post‐hoc","randomized","cross-organism","factor","group","toxicogenomics"],"hovertext":["Term: changes<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: carbohydrate<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: crossover<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: diet<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: following<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: low<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: post-hoc<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: randomized<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: cross-organism<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: factor<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: group<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: toxicogenomics<br /><br /> Cross-organism toxicogenomics with group factor analysis"],"textfont":{"size":[7.44951416531748,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,7.89774846019875,8.74649302849887,8.74649302849887,8.74649302849887,8.74649302849887],"color":"rgba(166,86,40,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 7","legendgroup":"C 7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.14351003393091,2.22694054972367,1.78651013600279,2.19061304425959],"y":[1.20444004948975,1.50667199553113,1.03410528468684,0.187986997921734],"text":["imi-rhapsody","study","clinical","type<br />2<br />diabetes"],"hovertext":["Term: imi-rhapsody<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: study<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: clinical<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: type<br />2<br />diabetes<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study"],"textfont":{"size":[14.7100211477783,15.1913643777023,15.4703955062867,16.6909245850854],"color":"rgba(247,129,191,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 8","legendgroup":"C 8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.739379396862922,1.16459145417028,0.503257226865379,0.950639313287674,1.6849655155254,1.56548468770212,0.668610322770981,0.236651787550507,0.302740471445931],"y":[0.62895554864924,-0.0753722875178269,0.915335584917624,-0.666210920529853,-0.0700760171317422,0.259816937191699,0.522577645765904,-0.584435050332632,0.732175780100885],"text":["clusters","distinct","people","signatures","cross-validation","replication","subtypes","variables","molecular"],"hovertext":["Term: clusters<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: distinct<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: people<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: signatures<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: cross-validation<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: replication<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: subtypes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: variables<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: molecular<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study"],"textfont":{"size":[11.3786268573125,11.3786268573125,11.3786268573125,11.3786268573125,11.4471158713255,11.4471158713255,11.4471158713255,11.4471158713255,12.8242526914013],"color":"rgba(153,153,153,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"C 9","legendgroup":"C 9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.3059360730594,"r":7.30593607305936,"b":25.5707762557078,"l":10.958904109589},"plot_bgcolor":"rgba(0,0,0,1)","paper_bgcolor":"rgba(0,0,0,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.66286042689293,2.45978821527684],"tickmode":"array","ticktext":["-10","-5","0","5","10"],"tickvals":[-2.31243834127275,-1.6472311463711,0,1.6472311463711,2.31243834127275],"categoryorder":"array","categoryarray":["-10","-5","0","5","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"<br /><br /><br />Map of my research topics","font":{"color":"rgba(190,190,190,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-3.36463292851555,1.73863889667621],"tickmode":"array","ticktext":["-20","-10","0"],"tickvals":[-2.99822295029797,-2.31243834127275,-4.44089209850063e-16],"categoryorder":"array","categoryarray":["-20","-10","0"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(190,190,190,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(0,0,0,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(190,190,190,1)","family":"","size":11.689497716895},"y":-11.7902098622378,"orientation":"h","title":{"text":"Cluster<br>"},"x":0.458383569834937},"annotations":[{"text":"Distance<br />Cluster","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(190,190,190,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"401c2b627b7":{"x":{},"y":{},"label":{},"size":{},"text":{},"colour":{},"type":"scatter"}},"cur_data":"401c2b627b7","visdat":{"401c2b627b7":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Export HTLM Widget

(Uncomment for saving the files.)


```r
widget <- plotly::partial_bundle( plot.dark.interactive )

# dir.create( "output-dark" )
# 
# htmlwidgets::saveWidget( widget, "output-dark/index.html" )
```

## View the Exported Widget

```text

<iframe 
  id="igraph" 
  scrolling="no" 
  style="border:none;" 
  seamless="seamless" 
  src="output-dark/index.html" 
  height="800" 
  width="800"
>
widget
</iframe>

```

<iframe 
  id="igraph" 
  scrolling="no" 
  style="border:none;" 
  seamless="seamless" 
  src="output-dark/index.html" 
  height="800" 
  width="800"
>
widget
</iframe>

# SessionInfo


```r
utils::sessionInfo()
```

```
## R version 4.0.4 (2021-02-15)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19042)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] tidyr_1.1.2      ggfortify_0.4.12 ggplot2_3.3.5   
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.1   xfun_0.27          purrr_0.3.4        colorspace_2.0-0  
##  [5] vctrs_0.3.8        generics_0.1.0     htmltools_0.5.1.1  viridisLite_0.4.0 
##  [9] yaml_2.2.1         utf8_1.1.4         plotly_4.9.4.1     rlang_0.4.11      
## [13] jquerylib_0.1.4    pillar_1.6.2       glue_1.4.2         withr_2.4.2       
## [17] bit64_4.0.5        RColorBrewer_1.1-2 lifecycle_1.0.0    stringr_1.4.0     
## [21] munsell_0.5.0      gtable_0.3.0       htmlwidgets_1.5.4  evaluate_0.14     
## [25] labeling_0.4.2     knitr_1.34         tzdb_0.1.2         crosstalk_1.1.1   
## [29] curl_4.3           parallel_4.0.4     fansi_0.4.2        highr_0.9         
## [33] readr_2.0.1        scales_1.1.1       vroom_1.5.5        jsonlite_1.7.2    
## [37] farver_2.1.0       bit_4.0.4          gridExtra_2.3      hms_1.1.0         
## [41] digest_0.6.27      stringi_1.5.3      dplyr_1.0.4        grid_4.0.4        
## [45] cli_3.0.1          tools_4.0.4        magrittr_2.0.1     lazyeval_0.2.2    
## [49] tibble_3.0.6       cluster_2.1.2      crayon_1.4.1       pkgconfig_2.0.3   
## [53] ellipsis_0.3.2     data.table_1.13.6  rmarkdown_2.11     httr_1.4.2        
## [57] rstudioapi_0.13    R6_2.5.1           compiler_4.0.4
```

# Appendix

* Copy README.html to index.html to view this script on Github Pages.


```r
if ( file.exists( "README.html" ) ) {
  
  system( command = "rm index.html" )
  system( command = "cp README.html index.html" )
  
}
```

```
## [1] 0
```
