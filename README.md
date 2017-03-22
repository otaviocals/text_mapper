# Word Correlation Map Generator
### Written by Otavio Cals

### Introduction

  The Word Correlation Map Generator is a tool I developed in R for analizing word correlations in a text dataset.
  
  This program takes as input a word, looks for the highest correlated words in the text dataset and then looks for the highest correlated words to those words and so on, each time creating a new correlation layer.
  
  User discretion is advised since the total of queries to the dataset in any given layers is $5^n$, for n being the number of layers. Therefore, the number of queries grows exponentially with the number of layers.

### Usage

After sourcing both text_mapping.R and map_visualizer.R, run text_mapping("word","dataset"), where "word" is the word to be analized and "dataset" is the text dataset to be used.

One can fine tune the following settings:

  * lines (default=0): The number of lines to be read from the dataset. Set to 0 to read all the lines of the set.
  * depth (default=1): The number of correlation layers
  * synthesize (default=TRUE): Wheter to synthesized repeated words in the correlation layers or not. Synthesized words do not generate branches on further layers.
  * filter (default=TRUE): Filter common words from a chosen language.
  * language (default="english"): The language to whose common words will be filtered if filter=TRUE.

After generating the data table with the correlation values, one can run map_visualizer("data_table","mode") to visualize the data. The visualization modes are:

  * "pie" (default): A pie chart with the values of each word. If this mode is selected, one can also set a "min" value for a word to be exhibited on the chart. Any value lower than the minimum will be concatenated under the "Others" category
  * "tree": A tree structure from the googleVis package.
  * "org": An organization chart from the googleVis package.

### Examples

Looking for correlations of the word "science" in a dataset of colleted twitter posts. The total amount of lines (each line is a tweet in this set) is 50000. The number of correlation layers is 4.

```
textmap <- text_mapping("science","Datasets/en_US.twitter.txt",lines = 50000, depth = 4)
```

After done running, we will generate visualizations for the results:


Pie Chart

```
map_visualizer(textmap,"pie",min=0.05)
```


Tree Structure

```
map_visualizer(textmap,"tree")
```


Organization Chart

```
map_visualizer(textmap,"org")
```