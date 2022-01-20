---
title: "Scholar WordGraph"
author: Tommi Suvitaival, Steno Diabetes Center Copenhagen, tommi.raimo.leo.suvitaival@regionh.dk
date: "2022-01-20"
output: 
  html_document:
    keep_md: true
    fig_width: 8
    fig_height: 8
    dev: jpeg
    toc: yes
always_allow_html: true
---

# Load Data


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
## Rows: 97 Columns: 1
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
# Format Data into Data Frame


```r
data <- unlist( data.loaded )
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```r
data <- data[ 1:( length( data ) - 4 ) ]

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

# Convert Titles to Lower Case


```r
data$"Title.lower" <-
  tolower( data$"Title" )

# Neutralize plurals and verbs.

# data$"Title.lower" <-
#   stringr::str_replace_all(
#     string = data$"Title.lower",
#     pattern = "(s\\s)",
#     replacement = " "
#   )
# 
# data$"Title.lower" <-
#   stringr::str_replace_all(
#     string = data$"Title.lower",
#     pattern = "(s\\:)",
#     replacement = ""
#   )
# 
# data$"Title.lower" <-
#   stringr::str_replace_all(
#     string = data$"Title.lower",
#     pattern = "(s$)",
#     replacement = ""
#   )

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
    # pattern = "type 2 diabete",
    pattern = "type 2 diabetes",
    replacement = "type_2_diabetes"
  )
```

# Omit Punctuation from Titles


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
```
# Extract Unique Words


```r
words.unique <-
  sort(
    unique(
      unlist( words ) )
  )
```

# Define List of Blocked Words


```r
blocklist <-
  c(
    "in",
    "of",
    "and",
    "the",
    "with",
    "for",
    "from",
    "to",
    "...",
    "an",
    "at",
    "on",
    letters,
    "after",
    "are",
    "as",
    "based",
    "by",
    "do",
    "is",
    "not",
    "without",
    "through",
    "during"
  )

# words.unique <- words.unique[ !( words.unique %in% blocklist ) ]

blocklist <- 
  c(
    blocklist,
    words.unique[ grepl( x = words.unique, pattern = "^[0-9]+$" ) ]
  )
```


```r
words <-
  lapply(
    X = words,
    FUN = function( x ) {
      x[ !( x %in% blocklist ) ]
    }
  )
```

# Re-Extract Unique Words


```r
words.unique <-
  sort(
    unique(
      unlist( words ) )
  )
```

# Create the Words-by-Articles Matrix


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

## Log-Transform Occurrences


```r
wba.log1px <- log10( 1 + wba )
```

## Compute PCA


```r
wba.norm <- scale( wba.log1px )
# wba.norm <- scale( wba )

result.pca <-
  princomp(
    x = wba.norm
  )
```

# Plot PCA


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
    ,
    # loadings = TRUE,
    # loadings.label = TRUE
  )

print( plot )
```

![](README_files/figure-html/unnamed-chunk-12-1.jpeg)<!-- -->

# Cluster Words


```r
result.clustering <-
  cluster::pam(
    x = wba.norm,
    k = 8
  )

# result.clustering <-
#   cluster::pam(
#     x = result.pca$"scores"[ , 1:2 ],
#     k = 8
#   )
```

# Prepare Data for Plot


```r
jitter <- 1

data.plot <-
  data.frame(
    Word = rownames( result.pca$"scores" ),
    result.pca$"scores"
  )

# data.plot$"Comp.1.jittered" <- 
#   data.plot$"Comp.1" + 
#   rnorm(
#     n = nrow( data.plot ),
#     sd = jitter
#   )
# 
# data.plot$"Comp.2.jittered" <- 
#   data.plot$"Comp.2" + 
#   rnorm(
#     n = nrow( data.plot ),
#     sd = jitter
#   )

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

data.plot$"Cluster" <- 
  as.factor( result.clustering$"clustering" )
  # as.factor(
  #   paste(
  #     "C",
  #     result.clustering$"clustering"
  #   )
  # )

data.plot$"Publications" <-
  # tmp <-
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
      # names( which( x ) )
    }
  )
```

# Define the Figure


```r
palette <- RColorBrewer::brewer.pal( n = 9, "Set1" )[ -6 ]

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
  # ggplot2::geom_point() +
  ggplot2::geom_text(
    position =
      ggplot2::position_jitter(
        height = jitter,
        width = jitter
      )
    # ,
    # show_guide = FALSE
  ) +
  ggplot2::scale_x_continuous( trans = "pseudo_log" ) +
  ggplot2::scale_y_continuous( trans = "pseudo_log" ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
    # ,
    # legend.position = "none"
  ) +
  ggplot2::xlab( label = "" ) +
  ggplot2::ylab( label = "" ) +
  ggplot2::scale_color_manual( values = palette )
#+
  # ggplot2::scale_color_brewer( palette = "Set1" )
# +
  # ggplot2::theme_classic()
```

# Create the Interactive Figure


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
        title = list( text = "Cluster" ),
        x = quantile( x = data.plot$"Comp.2", probs = 0.67 ),
        y = min( data.plot$"Comp.1" )
      )
  )
```

# View the Interactive Figure


```r
plot.interactive
```

```{=html}
<div id="htmlwidget-9aedecb1c2e97a636317" style="width:768px;height:768px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-9aedecb1c2e97a636317">{"x":{"data":[{"x":[-1.53130981150473,0.413667047955432,0.979536006984993,0.0111333456936415,0.047831449949875,0.892943559443668,0.877434952476754,0.307816356247052,-0.46133355580853,-0.703173329761491,-0.255972265905008,-0.592575909942053,0.907455357549473,-0.744883919773862,-0.94973842444481],"y":[1.13197276497544,0.122100049674344,1.18026544178137,0.297244517020794,-0.737045439647051,0.777975007027549,0.481148226896761,0.206217591429589,0.736947531125037,-0.170923494023743,0.653634931670961,0.00808210302872647,-0.595878466253641,0.991783057118294,0.486029143223999],"text":["…","avon","blood-based","changes","childhood","development","early","evidence","experiences","integrated","later","longitudinal","point","preceding","proteomics"],"hovertext":["Term: …<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: avon<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: blood-based<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: changes<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: childhood<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: development<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: early<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: evidence<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: experiences<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: integrated<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: later<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: longitudinal<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: point<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: preceding<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …","Term: proteomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …"],"textfont":{"size":[9.99470877539863,9.48745738512215,6.96744524536321,7.81788257593737,6.96744524536321,10.8484656232097,6.96744524536321,6.96744524536321,6.96744524536321,6.96744524536321,6.96744524536321,9.48745738512215,6.96744524536321,6.96744524536321,6.96744524536321],"color":"rgba(228,26,28,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.13021870253969,0.299042967028361,-0.804292213847913,-0.628201256283399,0.867918128556798,-1.22770229201897,-1.21468784602127,0.386775830704014,0.276068352508736,0.122982706289184,1.08942401779483,-0.905298182020017,-0.33448538801502,0.448357050882345,0.578667494918839,0.104595430133364,-0.327329373663344,1.14026509826106,-0.0353806519192239,-0.409772637419957,0.0690737408112412,0.369381854656529,-0.736665636078295,-0.991066121087184,0.561471556905174,-1.46286527814881,1.07464941558998,0.359004901847601,0.769548292381381,-0.0293816566264558,1.40181832051882,-0.388322817223647,-0.87136398848567,0.169801551786682,-0.12128042823663,0.4501758571252,1.09913726271705,-0.614563793456959,0.709763803188929,-0.0549828449640883,-0.17242094739418,-0.875791667737839,-0.873400907617617,-0.504098689705839,-0.151386488103509,0.374702495312266,0.0622954383822917,0.86433200144181,0.428626001451402,0.390917517059687,0.904169653186267,-0.372089192784237,-0.73856255750041,0.162457479707195,-0.476164354691562,-1.32438828550412,0.678164519354389,0.747961833971082,-0.359614739405044,-0.0879142610922058,2.10506204712603,-0.543687981980697,0.363364675770673,-0.847964942329115,-0.669016336102815,0.622524520908561,0.875039545807211,-2.06414589691081,-1.64740470100732,-1.15441150548946,1.10083372731207,1.3088903578941,-0.63194913033613,-1.23155333422781,-1.86470299583295,-0.558977697164375,-0.447359841378906,-0.129963143136835,-0.287061466240724,1.44482179415622,0.227292825079752,1.01057517235094,-0.550614613459915,1.0986685710807,0.0737307404957572,0.600862591271493,1.34879032097528,-0.0725989463427452,0.192233873275313,-0.355719569195767,-0.862184577327109,-0.0162044481540249,0.426490806446842,0.452829600437343,0.367952820259523,-0.079200145674408,-0.805887163270819,0.134103795466167,-0.302142840359672,0.54215123346189,0.567565634726924,-0.0467412585181519,0.516197291931309,0.759878190049174,-0.953844150418723,-1.01689671602461,-1.06894315271716,-0.774259103889421,0.996822473403226,-0.0814388231614346,-1.0292366218603,-1.57263137294113,-0.611566553714061,-0.135797410328646,1.17763662863037,-0.909326008872599,0.779730317913727,-0.029560258497954,0.121613426567403,1.60942633693316,0.444079888603091,-1.14711364988315,1.65273564196767,-0.695610410736702,-0.206826259255892,-0.425337594213017,0.173101798911783,1.07866090933764,0.541988182301051,-0.527082386039532,-0.124071294543804,-3.00311630469525,-0.0124230766788438,0.587027435001146],"y":[-0.449538343819105,-0.0296109686878004,0.654659609267418,-0.0687330990676722,0.63608170094931,-0.79912175680705,-0.81574299127094,0.0918391750104116,0.950296777273299,0.608419289319547,-3.08549150431518,0.408818716140393,-0.896500044529555,-0.618088068828368,-0.341446917255099,-0.315838774972258,-0.847634634511616,-1.04568460981971,-0.112631582697924,-0.310649517485169,0.0548809651620311,-0.795007517588554,-0.480015755316331,-0.634170981784971,0.798606547695534,-0.190447127066107,1.36888823545817,1.14653781626871,-0.0890122953955167,0.1541836287749,-0.69788255904154,-0.236743247442542,-0.635175258218362,0.0795228226774946,0.00173239175088435,-1.17529517158544,0.159139726641919,-0.268784119155295,-1.60096869443508,0.813727975463752,-1.0168844157495,0.65312585685207,0.4225877136651,1.23344808331722,1.2395152441462,0.763065989086334,0.748571309895204,-0.456756152232665,-0.667844381421375,0.82648701849955,0.0376934856954879,-0.521048040002622,0.484380530631872,-0.190475864892346,-1.51989334032081,0.0426537359579593,1.24077278586787,-1.98544608185352,0.663793992673908,-0.405201219683201,2.03648330729935,-1.36479665748492,0.40029405414863,0.584385155403004,-0.887687266856789,0.138024222000771,-0.753817448445021,-0.796391674894626,-0.519157447937431,-0.643857968844511,-0.147320990730975,-0.55650407238302,0.436609675719953,1.06612653158449,0.561185199834656,-0.911881750011114,0.344655373758973,-0.682541474065794,-0.731794587105593,-1.24336706917318,-0.614053736531628,1.01665402874091,-0.518677577713947,0.81460239297462,-0.184294808936272,-1.24835909609855,-2.13482620079763,0.381784620388748,-1.46629477371634,1.06946422885203,0.489205516176051,1.36812749073738,-0.975795456980018,0.588919462287186,-0.795014615197895,-0.306374348106306,-0.0859946375985124,0.271651083264462,0.260555116708701,-0.158502649277422,-0.263818228178886,1.14507214915891,0.530831045814691,0.885952886898097,-0.471006684858733,-0.299556944286698,0.199246961750053,0.361431181992436,0.360116741019632,0.632466415789424,-1.05363681516635,0.336345149900684,-0.404286838524424,1.15943789835119,-0.0890502565304359,-0.829424682410877,-0.835822976008955,-1.34911346003644,1.25002795549435,-0.55141204712497,2.21647800115074,0.76079612629293,1.2856861188592,-0.0436827513678987,-0.152452234941586,-0.421220156407054,-0.546031301602695,-1.27700588574775,0.214463034852831,-0.921517135379438,-1.72592337651462,-0.758483424543204,-0.584846832841991,1.22897397853001],"text":["abnormalities","acids","adipose","adolescent","adolescents","albuminuria","all-cause","alspac","alteration","amino","analysis","areas","assessment","associates","atlas","bayesian","between","biomarkers","body","branched","carbohydrate","chain","characterization","children","cimt","circulating","clinical","clusters","co-morbidities","cohort","collinear","compound","copenhagen","correlations","cross-organism","cross-species","cross-validation","crossover","data","diabetic","diet","dipis","discovery","disorder","distinct","diversity","effect","factor","findings","finnish","first-episode","following","future","graphical","group","gut","heart","high-dimensional","human","identification","imi-rhapsody","impairment","increased","insulin","level","lipidome","lipidomer","lipidomic","lipids","low","mass","men","metabolic","metabolite","metabolites","metabolomic","metabolomics","metformin","microbiota","modeling","models","molecular","mortality","mouse","multi-peak","multi-source","multi-way","multiple","multivariate","parents","pathogenesis","patients","peaks","people","persons","phosphatidylcholine","polyols","post‐hoc","postnatal","predictive","present","profile","profiling","progression","psychosis","quantitative","randomized","renal","replication","retinopathy","review","selected","serum","signature","signatures","species","spectral","sphingomyelin","stratification","stronger","study","subjects","subtypes","targeted","therapy","tissue","tool","toxicogenomics","translation","trial","two-way","type<br />1<br />diabetes","understanding","variables"],"hovertext":["Term: abnormalities<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: acids<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: adipose<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: adolescent<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: adolescents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: albuminuria<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: all-cause<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: alspac<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: alteration<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: amino<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: analysis<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Cross-organism toxicogenomics with group factor analysis<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: areas<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: assessment<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: associates<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: atlas<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: bayesian<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: between<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: biomarkers<br /><br /> Cross-species translation of multi-way biomarkers","Term: body<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: branched<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: carbohydrate<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: chain<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: characterization<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: children<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: cimt<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: circulating<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: clinical<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: clusters<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: co-morbidities<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: cohort<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: collinear<br /><br /> Two-way analysis of high-dimensional collinear data","Term: compound<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: copenhagen<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: correlations<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: cross-organism<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: cross-species<br /><br /> Cross-species translation of multi-way biomarkers","Term: cross-validation<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: crossover<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: data<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Two-way analysis of high-dimensional collinear data<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: diabetic<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: diet<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: dipis<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: discovery<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: disorder<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: distinct<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: diversity<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: effect<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: factor<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: findings<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: finnish<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: first-episode<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: following<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: future<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: graphical<br /><br /> Graphical multi-way models","Term: group<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: gut<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: heart<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: high-dimensional<br /><br /> Two-way analysis of high-dimensional collinear data","Term: human<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: identification<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: imi-rhapsody<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: impairment<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: increased<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: insulin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: level<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: lipidome<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: lipidomer<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: lipidomic<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: lipids<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: low<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: mass<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: men<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: metabolic<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: metabolite<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: metabolites<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: metabolomic<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: metabolomics<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: metformin<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: microbiota<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: modeling<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: models<br /><br /> Graphical multi-way models","Term: molecular<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: mortality<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: mouse<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: multi-peak<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: multi-source<br /><br /> Multivariate multi-way analysis of multi-source data","Term: multi-way<br /><br /> Multivariate multi-way analysis of multi-source data<br /><br /> Graphical multi-way models<br /><br /> Cross-species translation of multi-way biomarkers","Term: multiple<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: multivariate<br /><br /> Multivariate multi-way analysis of multi-source data","Term: parents<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: pathogenesis<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: patients<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: peaks<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: people<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: persons<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: phosphatidylcholine<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: polyols<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: post-hoc<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: postnatal<br /><br /> Molecular Atlas of Postnatal Mouse Heart Development","Term: predictive<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: present<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: profile<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: profiling<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: progression<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: psychosis<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: quantitative<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: randomized<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: renal<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …","Term: replication<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: retinopathy<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes","Term: review<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review","Term: selected<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: serum<br /><br /> Serum metabolite profile associates with the development of metabolic co-morbidities in first-episode psychosis","Term: signature<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort","Term: signatures<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: species<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: spectral<br /><br /> Stronger findings from mass spectral data through multi-peak modeling","Term: sphingomyelin<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes","Term: stratification<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria","Term: stronger<br /><br /> Stronger findings from mass spectral data through multi-peak modeling<br /><br /> Stronger findings for metabolomics through Bayesian modeling of multiple peaks and compound correlations","Term: study<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: subjects<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: subtypes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study","Term: targeted<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients","Term: therapy<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial","Term: tissue<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas","Term: tool<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men","Term: toxicogenomics<br /><br /> Cross-organism toxicogenomics with group factor analysis","Term: translation<br /><br /> Cross-species translation of multi-way biomarkers","Term: trial<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial","Term: two-way<br /><br /> Two-way analysis of high-dimensional collinear data","Term: type<br />1<br />diabetes<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Lipidomic Abnormalities During the Pathogenesis of Type 1 Diabetes: a Quantitative Review<br /><br /> Changes in the lipidome in type 1 diabetes following low carbohydrate diet: Post-hoc analysis of a randomized crossover trial<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: understanding<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR","Term: variables<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study"],"textfont":{"size":[10.834091285832,7.41340652642843,5.76412010584984,10.0741811762057,8.20530765357092,10.8002771272787,10.8896061105856,8.20530765357092,7.41340652642843,7.41340652642843,22.6771653543307,5.76412010584984,7.41340652642843,7.56031083578674,9.07930083895269,9.33156346343536,5.76412010584984,11.6680003215845,5.76412010584984,7.41340652642843,8.86031488892488,7.41340652642843,10.0741811762057,8.20530765357092,8.08385701377704,10.850582375681,16.5851461090282,12.2533261350314,7.56031083578674,12.4081167515685,13.70228936948,9.33156346343536,8.08385701377704,9.33156346343536,11.6051856373019,11.6680003215845,12.3272440709231,8.86031488892488,20.6905833920712,10.9406684352733,8.86031488892488,10.0741811762057,7.41340652642843,8.20530765357092,12.2533261350314,5.76412010584984,8.08385701377704,11.6051856373019,13.2207595112712,9.94584026401473,7.56031083578674,8.86031488892488,7.41340652642843,12.5407586654779,11.6051856373019,10.8002771272787,9.07930083895269,13.70228936948,5.76412010584984,8.20530765357092,15.8041798313968,12.3857022172263,10.0741811762057,8.08385701377704,7.76275119884904,9.17177062286652,7.76275119884904,14.330554907375,10.850582375681,8.86031488892488,11.3110029381096,9.94584026401473,7.56031083578674,11.3570310573125,14.3268804400379,7.41340652642843,9.33156346343536,8.08385701377704,10.8002771272787,13.2207595112712,12.5407586654779,13.759036667372,10.8896061105856,9.07930083895269,11.3110029381096,14.866365290156,20.2396570689893,9.33156346343536,14.866365290156,8.20530765357092,10.834091285832,8.66127674619946,9.33156346343536,12.2533261350314,7.41340652642843,10.8896061105856,7.41340652642843,8.86031488892488,9.07930083895269,9.94584026401473,7.41340652642843,13.144283097115,8.66127674619946,9.94584026401473,7.56031083578674,10.834091285832,8.86031488892488,12.3857022172263,12.3272440709231,10.850582375681,10.834091285832,10.8002771272787,7.56031083578674,8.20530765357092,12.2533261350314,10.8896061105856,11.3110029381096,10.8896061105856,11.2589235379919,13.2207595112712,16.2897069347355,10.0741811762057,12.3272440709231,8.66127674619946,8.08385701377704,5.76412010584984,9.94584026401473,11.6051856373019,11.6680003215845,9.62428245899456,13.70228936948,21.2186861670478,7.76275119884904,12.3272440709231],"color":"rgba(55,126,184,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.335390497912848,0.551943823212022,-0.197449460643148,-0.52019069647968,-0.0240196893166659,1.17187682630607,0.0442176479671242,-0.702204385324186,-1.33610565792967,0.308125976455593,0.805051414222112,0.0134401819493713],"y":[0.593122313448871,0.371997012940878,-0.512882081607645,0.531738577351764,0.824491504298235,0.411843544522012,0.344181334318305,-0.64634540565527,-0.109204070902382,0.479418568031574,1.65682644899745,1.21422392613971],"text":["account","glucose","high","individuals","inter-individual","intervention","lifestyle","responses","risk","tolerance","type<br />2<br />diabetes","variability"],"hovertext":["Term: account<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: glucose<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: high<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: individuals<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: inter-individual<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: intervention<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: lifestyle<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: responses<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: risk<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: tolerance<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes","Term: type<br />2<br />diabetes<br /><br /> Lipidome as a predictive tool in progression to type 2 diabetes in Finnish men<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Replication and cross-validation of type 2 diabetes subtypes based on clinical variables: an IMI-RHAPSODY study<br /><br /> Distinct molecular signatures of clinical clusters in people with type 2 diabetes: an IMI-RHAPSODY study","Term: variability<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes"],"textfont":{"size":[8.23996450129806,8.23996450129806,8.23996450129806,10.0924606348041,8.23996450129806,8.23996450129806,8.23996450129806,8.23996450129806,9.73035834686988,8.23996450129806,17.9572626227244,8.23996450129806],"color":"rgba(77,175,74,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.810521538501879,-0.689309221559862,0.880552130883834,0.804943691605976,0.415877859706333,0.683290450500598,-0.836601375469664,-0.867936276000194,0.806264052300057,0.227683486327896,-0.774608000211993,-0.0899878385082873],"y":[0.641241269680761,0.673320290661506,-0.356003305380144,0.936757896124474,0.424064773629509,0.567697674530647,0.0305214212565643,0.49994451096728,-0.737790251587428,-0.439559805844059,0.442308315401762,1.06950595139628],"text":["alcohol","ald","comprehensive","differences","hepatic","intoxication","lipid","lipidomics","nafld","phenotypic","reveals","turnover"],"hovertext":["Term: alcohol<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: ald<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: comprehensive<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: differences<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: hepatic<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: intoxication<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipid<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: lipidomics<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: nafld<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: phenotypic<br /><br /> Phenotypic Responses to a Lifestyle Intervention Do Not Account for Inter-Individual Variability in Glucose Tolerance for Individuals at High Risk of Type 2 Diabetes<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: reveals<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Lipidomics of human adipose tissue reveals diversity between body areas<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication","Term: turnover<br /><br /> Comprehensive lipidomics reveals phenotypic differences in hepatic lipid turnover in ALD and NAFLD during alcohol intoxication"],"textfont":{"size":[5.00498499093759,5.00498499093759,5.00498499093759,5.00498499093759,5.00498499093759,5.00498499093759,5.00498499093759,12.2191467805628,5.00498499093759,8.06978828172275,13.4742612165474,5.00498499093759],"color":"rgba(152,78,163,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.305463576662653,-0.406900803119577,0.581187095466369,-0.145850619327909,-0.501661595915145,0.597627624842637,-0.260644656632728,-0.118427544858939,-1.07086799017031,-0.528981459353752,-0.846566182587928,0.342751594233625,-0.572587111991531,-1.8147481269723,0.494711586247497,0.303936725921296],"y":[-0.0421058911295894,0.145090257429862,-0.67602957258246,-0.197434840577311,-0.0624965032028321,0.41616484304645,0.00244919690989816,-0.396957249699748,-0.623177932428783,0.751579817531043,-0.125304402636952,1.06382646659144,0.7266816181665,-0.165547430011868,-0.836816883965051,0.43077662229293],"text":["alzheimer's","amides","amyloid","brain","burden","disease","european","fatty","framework","hippocampal","information","medical","memory","plasma","primary","volume"],"hovertext":["Term: alzheimer's<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amides<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: amyloid<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: brain<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: burden<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: disease<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: european<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: fatty<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: framework<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: hippocampal<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: information<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: medical<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: memory<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: plasma<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Effect of metformin on plasma metabolite profile in the Copenhagen Insulin and Metformin Therapy (CIMT) trial<br /><br /> Gut microbiota profile and selected plasma metabolites in type 1 diabetes without and with stratification by albuminuria<br /><br /> Characterization of plasma lipidomics in adolescent subjects with increased risk for type 1 diabetes in the DiPiS cohort","Term: primary<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …","Term: volume<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …"],"textfont":{"size":[6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,6.7897961314756,16.3622306897015,6.7897961314756,6.7897961314756],"color":"rgba(255,127,0,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.52452010091298,0.327945116794637,0.836163721400282,-0.587626409829625,1.13316298451178,0.438289330688342,0.615243115815303,0.362896626205865,0.0583597364639556,-0.392598532834799],"y":[0.958553032486231,-0.725166951303838,0.0874812160641861,0.280426910149397,0.282123892104716,0.550141239201098,0.413291050589175,1.26624837783684,-0.476028710135727,0.893984366771667],"text":["applications","diagnostic","disorders—the","medicine","metsy","platform","project","psychotic","research","systems"],"hovertext":["Term: applications<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: diagnostic<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: disorders—the<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: medicine<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: metsy<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: platform<br /><br /> Targeted Clinical Metabolite Profiling Platform for the Stratification of Diabetic Patients<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: project<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: psychotic<br /><br /> Identification of a plasma signature of psychotic disorder in children and adolescents from the Avon Longitudinal Study of Parents and Children (ALSPAC) cohort<br /><br /> Integrated lipidomics and proteomics point to early blood-based changes in childhood preceding later development of psychotic experiences: evidence from the Avon Longitudinal …<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: research<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project","Term: systems<br /><br /> Platform for systems medicine research and diagnostic applications in psychotic disorders—The METSY project<br /><br /> Understanding the Lipidome at the Systems Level with lipidomeR"],"textfont":{"size":[7.86470265659629,7.86470265659629,7.86470265659629,7.86470265659629,7.86470265659629,10.0798691385651,7.86470265659629,10.5813026397026,7.86470265659629,9.34944378435302],"color":"rgba(166,86,40,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"6","legendgroup":"6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.15979373321005,0.889910564451852,0.617332582987287,-0.34686420329716,-0.869014468815735,-0.266128664210996,-0.339460254952869,0.558962449591049,-0.696762996100447,-0.209864272576383,-1.03237420351114],"y":[-0.241970117759512,-0.00280086805708245,-0.0879769382778509,-0.0980968602713151,0.862524236853798,-0.373626575503866,-0.455120412331442,-0.0620954024385615,-0.333757681936394,0.144275004811217,-0.639045586973797],"text":["associated","biopsies","deregulation","function","graft","liver","pathway","pre-transplant","purine","survival","transplantation"],"hovertext":["Term: associated<br /><br /> Primary fatty amides in plasma associated with brain amyloid burden, hippocampal volume, and memory in the European Medical Information Framework for Alzheimer's Disease …<br /><br /> Lipidomic analysis reveals sphingomyelin and phosphatidylcholine species associated with renal impairment and all-cause mortality in type 1 diabetes<br /><br /> Metabolomic assessment reveals alteration in polyols and branched chain amino acids associated with present and future renal impairment in a discovery cohort of 637 persons …<br /><br /> Circulating metabolites and lipids are associated to diabetic retinopathy in individuals With type 1 diabetes<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: biopsies<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: deregulation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: function<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: graft<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: liver<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pathway<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: pre-transplant<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: purine<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: survival<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation","Term: transplantation<br /><br /> Deregulation of the Purine Pathway in Pre-Transplant Liver Biopsies Is Associated with Graft Function and Survival after Transplantation"],"textfont":{"size":[16.5391435996719,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512],"color":"rgba(247,129,191,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"7","legendgroup":"7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.586822001566251,-0.266196875530223,-0.142962949979047,0.803173336037507,0.325830872007718,-0.0476309514370958,1.16612480145421,-0.100992100010554],"y":[-0.430605693622437,-0.228678649659213,-0.276757240197566,0.312057557613644,-0.497996647744384,0.670847526849303,0.0627604826400608,0.971045444351676],"text":["collected","cryogenically","describing","fecal","healthy","metabolome","participants","samples"],"hovertext":["Term: collected<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: cryogenically<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: describing<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: fecal<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: healthy<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: metabolome<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: participants<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants","Term: samples<br /><br /> Describing the fecal metabolome in cryogenically collected samples from healthy participants"],"textfont":{"size":6.96804895201759,"color":"rgba(153,153,153,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"8","legendgroup":"8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.3059360730594,"r":7.30593607305936,"b":10.958904109589,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-3.25852522228631,2.3604709647171],"tickmode":"array","ticktext":["-20","-10","0","10"],"tickvals":[-2.99822295029797,-2.31243834127275,-4.44089209850063e-16,2.31243834127275],"categoryorder":"array","categoryarray":["-20","-10","0","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-3.35058997958847,2.48157647642404],"tickmode":"array","ticktext":["-20","-10","0","10"],"tickvals":[-2.99822295029797,-2.31243834127275,0,2.31243834127275],"categoryorder":"array","categoryarray":["-20","-10","0","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":-10.0231455571157,"orientation":"h","title":{"text":"Cluster"},"x":0.448834434430711},"annotations":[{"text":"Cluster<br />Distance","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"3ef43b626073":{"x":{},"y":{},"label":{},"size":{},"text":{},"colour":{},"type":"scatter"}},"cur_data":"3ef43b626073","visdat":{"3ef43b626073":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

# Export HTLM Widget


```r
widget <- plotly::partial_bundle( plot.interactive )

# dir.create( "output" )

# htmlwidgets::saveWidget( widget, "output/index.html" )
```

# View the Exported Widget

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
