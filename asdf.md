---
title: "Logue Lab - Song Matching Experiment - Data Handling and Analysis"
output:   
  html_document:
    keep_md: true
    theme: journal
    toc: true
    toc_depth: 4
    toc_float: 
      collapsed: false
    code_folding: hide
---


```r
library(lme4)
library(plyr)
library(ggplot2)
library(afex)
library(emmeans)
library(ggthemes)
library(tidyverse)
library(kableExtra)
library(Hmisc)
library(binom)
library(Rmisc)
library(magick)
library(webshot)
library(magrittr)
library(multcomp)
library(data.table)
library(here)

theme_alan <- function(base_size = 12 , base_family = "")
{
  half_line <- base_size/2
  colors <- ggthemes_data$few
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", 
                        colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", 
                        size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                        angle = 0, margin = margin(), debug = FALSE),
    
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL, 
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0), 
    axis.ticks = element_line(colour = "grey20"), 
    axis.ticks.length = unit(half_line/2, "pt"),
    axis.title.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),
    
    legend.background = element_rect(colour = NA),
    legend.spacing = unit(0.4, "cm"), 
    legend.spacing.x = NULL, 
    legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = "white", colour = NA), 
    legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)), 
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),
    
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_line(colour = "grey92", size = 0.25),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    
    strip.background = element_rect(fill = "NA", colour = "NA"),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = 0, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
    strip.placement.x = NULL, 
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0, vjust = 1, margin = margin(b = half_line * 1.2)),
    plot.subtitle = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)), 
    plot.margin = margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE)
}
```
# Introduction

The approach I'm taking here is to lay out all data wrangling and analyses using best practices. In future, this should get around issues of

a) file naming conventions
b) folder structure
c) "showing your work"

etc

That seem to plague multi-year and multi-student projects. Thus, this document will cover everything from experiment design to data analysis in order, with as many notes as I can put in as possible.

## Project Workflow

As with all things, its a great idea to lay out a project workflow. You can click on GO beneath to each of these sections to take you to the appropriate part of the document. You can also click through the table of contents for the same purpose.



# Experimental Design {#ExperimentDesign}

The first part of this project at least was collecting songs from birds. 

## Creating Stimlists
For most experiments, it is really beneficial to use the approach of creating Stimlists that are exhaustive, and that take the same format that our eventual data analysis will be happiest with - this keeps a through-line to our entire experiment where we always know what our columns mean, and can always relate data at any point to data from any other point and see how we got there, which is incredibly beneficial.

### Creating our Birdsong Stimuli
I am unsure what the original set of experimental stimuli was for this experiment, so we are reconstructing an idea of how to put this experiment together from scratch. But, for the sake of argument, **let's assume that we are creating our experiment based on 40 unique birdsongs**. These songs, without any manipulation, are our "Control" songs, but we can also either speed up ("Fast") or slow down ("Slow") these stimuli, giving us a total of 120 stims.

For the purposes of putting together this experiment, I've simply taken 60 songs from "DDLb dawn", copied them (for a total of 120 songs) and renamed them accordingly in the below code chunk (when you run this chunk the OldNames and NewNames vectors will already be the same, because the files were already renamed, but you can see how this went (the files that this was done with can be seen in the "Source" subfolder))


```r
#Empty files in the destination directory
file.remove(dir(here("Stimuli/Fake Stimuli"), pattern = ".aif", full.names = TRUE))

#Copy files from source to destination
SourceNames <- dir(here("Stimuli/Fake Stimuli/Source"),  full.names = TRUE, pattern = ".aif") 
file.copy(from = SourceNames,
          to = here("Stimuli/Fake Stimuli"),
          overwrite = TRUE)

#get the names of the files in the directory
OldNames <- dir(here("Stimuli/Fake Stimuli"),  full.names = TRUE, pattern = ".aif") 

#Create the combinations of Song number and Song Type, which will become the new filenames

BirdSongs <- paste(here("Stimuli/Fake Stimuli"), "/",  #File Path
                  rep("Song ", 120),   #Just the word "Song" 60 times
                  rep(1:40, 3), "-", # The numbers 1:20 3 times in a row
                  rep(c("Control", "Fast", "Slow"), each = 40), #"Control" 20 times, then "Fast" 20 times, etc.
                  ".aif", sep = "" )

#use file.rename to apply our new names
file.rename(OldNames, BirdSongs)

# Clean up our now not needed OldNames vector
rm(OldNames, SourceNames)
```

So now we have our set of songs, from which we want to construct our lists of stimuli (StimLists) for use in each playback experiment. 

In the real world, however, we'd likely be choosing this subset of songs for our experiment from a larger database of birdsongs from the species that we are studying- We might for example have say 1000 total songs from 50 birds that fall into 30 total song types (this is totally made up) - in such a case we would want a script that would choose randomly from this list in such a way that each Song Type was only used once. See [Appendix 1- Choosing Song Stimuli I](#ChoosingSongs) for an example of this kind of script.

In another case, there might actually be less than 20 overall Song Types, even though we need 20 individual songs for our Experiment for some reason. In this case, the selection procedure for each Stimlist would need to be a bit more careful. See [Appendix 2- Choosing Song Stimuli II](#ChoosingSongs2) for an example of this

### Creating Bird Band Permutations

For simplicity, we will assume here that we're going to be testing 30 birds, and we're going to test each of them in both types of trials (Dawn and A), so we'll have a total of 60 stimlists.

Lets set up the identities of our target birds. Obviously in the field this is done differently - but here we'll simply assume that each bird has two colored bands chosen from the following colors, but that no bird has two bands of the same color :
black (B), white (W), orange (O), red (R), green (G), yellow (Y). We can easily set up the permutations of these colors (and exclude two bands of the same color) by doing the following:

1. Creating a vector of our 6 color abbreviations
2. Obtaining the permutations of these colors using [expand.grid](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid)
3. Removing permutations where both bands are the same color using the [subset](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/subset) function



```r
#1- Creating a vector of our color abbreviations
colors <- c("B", "W", "O", "R", "G", "Y") #Make a vector of our color abbreviations

#2- Obtaining all permutations of these colors
BirdID <- expand.grid(colors, colors) #Give us all permutations of our colors

#3- Removing permutations where both bands are the same color
BirdID <- subset(BirdID, Var1 != Var2) 
BirdID$BirdID <- paste(BirdID$Var1, BirdID$Var2, sep="") #Make a column which is the combination of these colors

#clean up our colors vector
rm(colors)
```

This gives us a total of 30 possible Bird IDs, from which we can easily derive our total number of 60 "participants"

In the real world, we have a finite set of colors for banding, and we are ultimately looking for maximal distinctiveness in the bands we apply to our birds. To use R to set up a banding scheme in the future, consider [Appendix 3- Maximally Distinct Banding Patterns](#MaxDistinct)).

### Creating our Experimental permutations (Participants)

In this case we have an easy job - each of our 30 birds is going to be tested at both Dawn and in the Afternoon. We'll call these "Participants", mostly because we then have language that can apply to other experiments and be generalised a little bit more easily by novices who might be working with people, cockroaches, or anything other than birds!


```r
#1- Creating a vector of Trial Types
TrialType <- c("Dawn", "Afternoon")

#Obtaining all permutations of BirdID and Trial Type
Participants <- expand.grid(BirdID$BirdID, TrialType)

#Renaming the columns of our "Experiments" dataframe
colnames(Participants) <- c("BirdID", "TrialType")

#Cleaning up now unnecessary vectors/dataframes
rm(BirdID, TrialType)
```


So now we have our list of Participants (crossings of Bird ID and Trial Type, such that each of our 30 birds is going to be tested at both times of day)

So, that's still not very much - what we need to do next is to choose which stimuli will be used for each Participant (each bird at each time)

### Assigning Stimuli

There are a few ways that we could assign stimuli in a case like this one - but generally we want to do so randomly - for each of our 60 participants we want to choose 10 of our 40 songs, then present each of those songs in their Control, Slow, and Fast format.

Actually, its a bit more complicated than that, because we don't want the same songs to appear in both the Afternoon and Dawn Trial Types. Here I assume we want completely separate songs for both types.

Because we're ultimately going to create 60 stimlists, we'll eventually have to write some sort of [function](https://www.tutorialspoint.com/r/r_functions.htm#:~:text=R%20has%20a%20large%20number,function%20to%20accomplish%20the%20actions.), or at least a [for loop](https://www.datamentor.io/r-programming/for-loop/) that can put together and export our stimlists. But for now, we're going to focus on a single case: Bird YB at Dawn.
