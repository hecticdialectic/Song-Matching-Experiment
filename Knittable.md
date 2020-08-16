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
library(sound)
library(audio)
library(seewave)
library(phonTools)
library(tuneR)
library(warbleR)
library(Rraven)
library(gdata) #for interleave
library(infer) #for rep_sample_n

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
I am unsure what the original set of experimental stimuli was for this experiment, so we are reconstructing an idea of how to put this experiment together from scratch. But, for the sake of argument, **let's assume that we are creating our experiment based on 20 unique birdsongs**. These songs, without any manipulation, are our "Control" songs, but we can also either speed up ("Fast") or slow down ("Slow") these stimuli, giving us a total of 60 stimuli.

For the purposes of putting together this experiment, I've taken 60 random songs in ".wav" format and renamed them accordingly in the below code chunk by doing the following:

1. Ensuring that the destination folder (/Fake Stimuli) is empty by deleting all ".wav" files from the folder

```r
set.seed(49) #I'll come back to why we did this here later! For now you can ignore it
file.remove(dir(here("Simulated/Stimuli-Shared/Source Files"), pattern = ".wav", full.names = TRUE))
```

2. Copying all ".wav" files from the source folder (/Source) to the destinate folder (/Fake Stimuli)

```r
OriginalNames <- dir(here("Simulated/Stimuli-Shared/Source Files/Original"),  full.names = TRUE, pattern = ".wav") 
file.copy(from = OriginalNames,
          to = here("Simulated/Stimuli-Shared/Source Files"),
          overwrite = TRUE)
```

3. Retrieving the names of all files in the destination folder

```r
OldNames <- dir(here("Simulated/Stimuli-Shared/Source Files"),  full.names = TRUE, pattern = ".wav") 
```

4. Creating a list of filenames for the new files

```r
BirdSongs <- paste(here("Simulated/Stimuli-Shared/Source Files"), "/",  #File Path
                  rep("Song ", 20),   #Just the word "Song" 20 times
                  rep(1:20, 3), "-", # The numbers 1:20 3 times in a row
                  rep(c("Control", "Fast", "Slow"), each = 20), #"Control" 20 times, then "Fast" 20 times, etc.
                  ".wav", sep = "" )
```

5. Renaming the files in the directory to the new names

```r
file.rename(OldNames, BirdSongs)
rm(OriginalNames, OldNames)
```


So now we have our set of songs, from which we want to construct our lists of stimuli (StimLists) for use in each playback experiment. 

In the real world, however, we'd likely be choosing this subset of songs for our experiment from a larger database of birdsongs from the species that we are studying- We might for example have say 1000 total songs from 50 birds that fall into 30 total song types (this is totally made up) - in such a case we would want a script that would choose randomly from this list in such a way that each Song Type was only used once. See [Appendix 1- Choosing Song Stimuli I](#ChoosingSongs) for an example of this kind of script.

In another case, there might actually be less than 20 overall Song Types, even though we need 20 individual songs for our Experiment for some reason. In this case, the selection procedure for each Stimlist would need to be a bit more careful. See [Appendix 2- Choosing Song Stimuli II](#ChoosingSongs2) for an example of this

### Creating Bird Band Permutations

For simplicity, we will assume here that we're going to be testing 30 birds, and we're going to test each of them in both types of trials (Dawn and A), so we'll have a total of 60 stimlists.

Lets set up the identities of our target birds. Obviously in the field this is done differently - but here we'll simply assume that each bird has two colored bands chosen from the following colors, but that no bird has two bands of the same color :
black (B), white (W), orange (O), red (R), green (G), yellow (Y). We can easily set up the permutations of these colors (and exclude two bands of the same color) by doing the following:

1. Creating a vector of our 6 color abbreviations

```r
colors <- c("B", "W", "O", "R", "G", "Y")
```

2. Obtaining the permutations of these colors using [expand.grid](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid)

```r
BirdID <- expand.grid(colors, colors)
```

3. Removing permutations where both bands are the same color using the [subset](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/subset) function

```r
BirdID <- subset(BirdID, Var1 != Var2) 
BirdID$BirdID <- paste(BirdID$Var1, BirdID$Var2, sep="")
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

There are a few ways that we could assign stimuli in a case like this one - but generally we want to do so randomly - for each of our 60 participants we want to choose 3 of our 20 songs, then present one of those songs in its "Control" form, one in its "Slow" form, and one in its "Fast" form

Actually, its a bit more complicated than that, because we don't want the same songs to appear in both the Afternoon and Dawn Trial Types. Here I assume we want completely separate songs for both types.

Because we're ultimately going to create 60 stimlists, we'll eventually have to write some sort of [function](https://www.tutorialspoint.com/r/r_functions.htm#:~:text=R%20has%20a%20large%20number,function%20to%20accomplish%20the%20actions.), or at least a [for loop](https://www.datamentor.io/r-programming/for-loop/) that can put together and export our stimlists. But for now, we're going to focus on a single case: Bird YB at Dawn. After going through this piece by piece using standard R assignment, we'll write the same code with pipes for the Afternoon trials.

#### The Single Case

We will assign stimuli for this Participant in the following fashion

1. Create our list of "Base Songs" - which are the unmodified songs (i.e. the same as our control songs) using [enframe](https://tibble.tidyverse.org/reference/enframe.html)
    *Getting rid of the parts we don't want with str.replace (file paths and extensions)
    *Separate the "Song" column back into its components: Song Number and Song Type
    *Subset so there are only "Control" songs in the list

```r
BaseSongs <- as.data.frame(dir(here("Simulated/Stimuli-Shared/Source Files"), pattern = ".wav"))

# Rename the column of this data frame to something meaningful (but a placeholder)
colnames(BaseSongs) <- "Songdata"

# Remove the file extensions
BaseSongs$Songdata <- str_replace_all(BaseSongs$Songdata, ".wav", "")

#Separate into two columns - SongNumber and SongType
BaseSongs <- separate(data = BaseSongs, 
                      col = Songdata, 
                      into = c("SongNum", "SongType"), sep = "-", remove = FALSE)

#Subset so we only have Control songs (our Base Songs)
BaseSongs <- subset(BaseSongs, SongType == "Control")
```
    
2. We will sample 6 Songs from our list of Base Songs using the [sample](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample) function

```r
ChosenSongs <- factor(sample(BaseSongs$SongNum, 6)) #Note we include factor() here to drop unused levels of the factor
```
    
3. Assign song speeds for both Trial Types - for each, one of the songs will be presented in its Control format, one in its Fast Format, and one in its Slow format. Because we need to sample multiple times, we'll do this using the [replicate](https://www.rdocumentation.org/packages/Rpdb/versions/2.2/topics/replicate) function.

```r
Speeds <- c("Control", "Fast", "Slow")
SpeedOrder <- as.vector(replicate(2, sample(Speeds, 3))) #unfortunately the replicate function needs us to add "as.vector" to output data in a sensible format (i.e. not as a list of lists)
ChosenSongs <- paste(ChosenSongs, SpeedOrder, sep = "-") #Add our Speeds to the songs

rm(Speeds, SpeedOrder)
```
    
4. Assign half of these songs to Dawn and half to Afternoon, and save these stimuli to the appropriate folder

```r
DawnSongs <- ChosenSongs[1:3]
DawnSongsS <- here("Simulated/Stimuli-Shared/Source Files", 
                   paste(DawnSongs, ".wav", sep = "") 
                   )
DawnSongsD <- here("Simulated/Single/Stimuli/Dawn", 
                   paste(DawnSongs, ".wav", sep = "") 
                   )

#Make sure destination folder is empty
file.remove(dir(here("Simulated/Single/Stimuli/Dawn"), pattern = ".wav", full.names = TRUE))

#Copy the chosen files
file.copy(from = DawnSongsS,
          to = DawnSongsD,
          overwrite = TRUE)
```
    
6. Set up the Dawn songs. For Dawn trials, songs are played in 10 blocks, where the order of presentation within the blocks is randomised. Thus we do this by sampling 10 times, using replicate

```r
DawnSongsList <- as.data.frame(as.vector(replicate(10, sample(DawnSongs, 3))))
colnames(DawnSongsList) <- "SongInfo"
DawnSongsList <- separate(data = DawnSongsList,
                           col= SongInfo,
                           into = c("SongNum", "SongSpeed"), sep = "-", remove = FALSE)
```
    
7. Reading in Sound files using the [load.wave](https://cran.r-project.org/web/packages/audio/audio.pdf) function from the "audio" package


```r
Dawn1 <- load.wave(  here("Simulated/Single/Stimuli/Dawn", paste(DawnSongs[1], ".wav", sep = ""))  )
Dawn2 <- load.wave(  here("Simulated/Single/Stimuli/Dawn", paste(DawnSongs[2], ".wav", sep = ""))  )
Dawn3 <- load.wave(  here("Simulated/Single/Stimuli/Dawn", paste(DawnSongs[3], ".wav", sep = ""))  )
```

8. Inserting 10 seconds of silence at the start of each file using the [addsilw](https://www.rdocumentation.org/packages/seewave/versions/2.1.6/topics/addsilw) function of the 'seewave" package

```r
Dawn1silent <- addsilw(Dawn1, 
                       at = "start",
                       d = 10,
                       output = "audioSample")

Dawn2silent <- addsilw(Dawn2, 
                       at = "start",
                       d = 10,
                       output = "audioSample")

Dawn3silent <- addsilw(Dawn3, 
                       at = "start",
                       d = 10,
                       output = "audioSample")
```

9. Saving our files that have 10 seconds of silence added to them using [save.wave](https://cran.r-project.org/web/packages/audio/audio.pdf) from the "audio" package

Strictly speaking, we don't need to save these files - we're just going to append them into stimtracks - but its good practice to save things at each of our steps anyways so we know exactly what was used where

```r
file.remove(dir(here("Simulated/Single/Stimuli/Dawn"), pattern = "Silent.wav", full.names = TRUE))
save.wave(Dawn1silent, 
           where = here("Simulated/Single/Stimuli/Dawn", 
                  paste(DawnSongs[1],"-Dawn1-","Silent", ".wav", sep = "")))
save.wave(Dawn2silent, 
           where = here("Simulated/Single/Stimuli/Dawn", 
                  paste(DawnSongs[2],"-Dawn2-","Silent", ".wav", sep = "")))
save.wave(Dawn3silent, 
           where = here("Simulated/Single/Stimuli/Dawn", 
                  paste(DawnSongs[3],"-Dawn3-","Silent", ".wav", sep = "")))
```

10. Concatenating files into a stimtrack

Here we do this the most ugly way possible (to demonstrate the value of using lapply or writing a function/for loop)

MAKE A NOTE HERE ABOUT CACHEING!

```r
DawnSongsList$SongSel <- mapvalues(DawnSongsList$SongInfo,
                                 from = DawnSongs,
                                 to = c("Dawn1", "Dawn2", "Dawn3"))

DawnSongsList$SongPath <- here("Simulated/Single/Stimuli/Dawn", 
                               paste(
                                 DawnSongsList$SongInfo,  "-", DawnSongsList$SongSel, "-Silent.wav", sep = ""
                               ))

write.csv(DawnSongsList, here("/Simulated/Single/Stimlists", "DawnStims.csv"))
                               
DawnSongsTrack <- appendSample(DawnSongsList$SongPath[1],DawnSongsList$SongPath[2], DawnSongsList$SongPath[3],
                      DawnSongsList$SongPath[4],DawnSongsList$SongPath[5],DawnSongsList$SongPath[6],
                      DawnSongsList$SongPath[7],DawnSongsList$SongPath[8],DawnSongsList$SongPath[9],
                      DawnSongsList$SongPath[10],DawnSongsList$SongPath[11],DawnSongsList$SongPath[12],
                      DawnSongsList$SongPath[13],DawnSongsList$SongPath[14],DawnSongsList$SongPath[15],
                      DawnSongsList$SongPath[16],DawnSongsList$SongPath[17],DawnSongsList$SongPath[18],
                      DawnSongsList$SongPath[19],DawnSongsList$SongPath[20],DawnSongsList$SongPath[21],
                      DawnSongsList$SongPath[22],DawnSongsList$SongPath[23],DawnSongsList$SongPath[24],
                      DawnSongsList$SongPath[25],DawnSongsList$SongPath[26],DawnSongsList$SongPath[27],
                      DawnSongsList$SongPath[28],DawnSongsList$SongPath[29],DawnSongsList$SongPath[30])

saveSample(DawnSongsTrack, 
            here("Simulated/Single/Stimuli/Dawn", 
                  paste("Stimtrack-Dawn", ".wav", sep = "")),
           overwrite = TRUE)
```

11. Using [autodetec](https://marce10.github.io/warbleR/reference/autodetec.html) from the warbleR package to attempt to extract signals from the stimtrack files.


```r
setwd(here("Simulated/Single/Stimuli/Dawn")) #A really annoying case where I have to use setwd because autodetec is weird

Dawn.ad <- autodetec(flist = "./Stimtrack-Dawn.wav", 
                     threshold = 0, mindur = 0.5, 
                     envt = "abs", ssmooth = 900,   
                     output = "data.frame")


head(Dawn.ad) %>% 
  data.frame() %>% 
      mutate_if(is.numeric, round, digits = 3) %>% 
            knitr::kable(caption = "Autodetection of Signals in Stimtrack", ) %>%
              kable_styling(full_width= F)
```
This seems to do an **okay** job, but requires a lot of fiddling - not ideal, and it probably won't work on files that have a lot more noise in them without significantly more experience using warbleR, but we can at least check how good of a job this did selecting our songs/signals out of the track. We could, of course, do this by checking the selections in Raven, but for now lets keep things in R. We can compare to where the signals are, because we know exactly where the signals are - after all we put them there.

To do this we'll start by extracting the durations of our sound files using the [duration](https://rdrr.io/cran/seewave/man/duration.html) function from the "seewave" package.

12. Extracting the duration of sound files 

```r
Durations <- c(duration(Dawn1), duration(Dawn2), duration(Dawn3))
```

13. Inserting columns with duration information into our dataframes using [mapvalues](https://www.rdocumentation.org/packages/plyr/versions/1.8.6/topics/mapvalues) from the "plyr" package


```r
DawnSongsList$Duration <- as.numeric(as.character(mapvalues(DawnSongsList$SongSel,
                                 from = c("Dawn1", "Dawn2", "Dawn3"),
                                 to = Durations)))
```

14. Reconstructing start times and end times. 
    * First we figure out the cumulative length of the appended sound files using [cumsum](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cumsum) from base R. This is actually the End Time in seconds for each song clip in the stimtrack 
    * Then reconstruct the start time by subtracting the duration
    

```r
## Adding up the cumulative length
DawnSongsList$`End Time (s)` <- cumsum(DawnSongsList$Duration +10)
DawnSongsList$`Begin Time (s)` <- DawnSongsList$`End Time (s)` - DawnSongsList$Duration
```

15. Comparing to the values obtained from autodetec


```r
## Adding up the cumulative length
DawnSongsList$`Begin Time (s) 2`<- Dawn.ad$start
DawnSongsList$`End Time (s) 2`<- Dawn.ad$end

## Computing differences
DawnSongsList$StartDiff <- DawnSongsList$`Begin Time (s)` - DawnSongsList$`Begin Time (s) 2`
DawnSongsList$EndDiff <- DawnSongsList$`End Time (s)` - DawnSongsList$`End Time (s) 2`
```

So actually it looks like our autodetec did pretty well - on average the difference between the begin/end times we input manually and those picked up by autodetec is only 2.9582 seconds, with a minimum difference of 0.0102 seconds and a maximum difference of 6.1834 seconds.

If you're interested in how well autodetec does in less ideal circumstances, see [Appendix 4- Autodetection of Signals I I](#AutodetectionI).

16. Saving our stimtracks as maximally informative stimtrack csv files (in the same format as those used/generated by Raven) so that they can be read into Raven as the starting point for annotation.

We'll be making use of the [interleave](https://rdrr.io/cran/gdata/man/interleave.html) function from "gdata" to stick two data frames together perfectly riffle shuffled (also known as interleaving).


```r
## Creating a bunch of vectors of columns that are needed 
Selection <- 1:30
View <- rep("Waveform 1", 30)
Channel <- rep(1, 30)
`Low Freq (Hz)` <- rep(0.0, 30)
`High Freq (Hz)` <- rep(22050.0, 30)
Bird <- rep(Participants$BirdID[5], 30)
`Trial Type` <- rep("Dawn", 30)
`Signal Type` <- rep("S", 30)
`Stim Type` <- DawnSongsList$SongSpeed
Response <- rep("", 30)
Overlapping <- rep("", 30)
Notes <- rep("", 30)
SongNum <- DawnSongsList$SongNum
SongSel <- DawnSongsList$SongSel
`Begin Time (s)`<- DawnSongsList$`Begin Time (s)`
`End Time (s)` <- DawnSongsList$`End Time (s)`

## Sticking our columns together into a dataframe that is named according to our naming convention
YBDawnStimtrack <- cbind.data.frame(Selection, 
                                      View, 
                                      Channel, 
                                      `Begin Time (s)`, 
                                      `End Time (s)`,
                                      `Low Freq (Hz)`,
                                      `High Freq (Hz)`,
                                      Bird,
                                      `Trial Type`,
                                      `Signal Type`,
                                      `Stim Type`,
                                      Response,
                                      Overlapping,
                                      Notes,
                                    SongNum,
                                    SongSel,
                                    DawnSongsList$Duration
                                      )

# Duplicating our dataframe, changing the "View" column to "Spectrogram 1", and then pasting it all back together and sorting
YBDawnStimtrack2 <- YBDawnStimtrack
YBDawnStimtrack2$View <- "Spectrogram 1"

YBDawnStimtrack <- interleave(YBDawnStimtrack, YBDawnStimtrack2)
```

17. Outputting our Stimtrack as a file

This part is pretty easy - we're just going to save our Stimtrack in the appropriate folder with the appropriate name


```r
write.csv(DawnSongsList, 
          file = here("/Simulated/Single/Stimlists", 
            paste(YBDawnStimtrack$Bird[1], "-", YBDawnStimtrack$`Trial Type`[1], "-StimTrack.csv", sep ="")))
```

18. Clean up all the trash

We now don't need most of these vectors etc - so lets keep this project space clean and tidy


```r
rm(YBDawnStimtrack2, `Begin Time (s)`, Bird, BirdSongs, Channel, ChosenSongs, Dawn1, Dawn1silent, Dawn2, Dawn2silent,
   Dawn3, Dawn3silent,DawnSongs, DawnSongsD, DawnSongsS, Durations, `End Time (s)`, `High Freq (Hz)`, `Low Freq (Hz)`, Notes,
   Overlapping, Response, Selection, `Signal Type`, SongNum, SongSel, `Stim Type`, View, DawnSongsTrack, Dawn.ad, BaseSongs, `Trial Type`)
```

19. Generating our Afternoon stimtracks using pipes and the tidyverse

So far, we've been using standard assignment (left assignment, seen as <-) for running almost all of our functions.
In some ways, standard assignment is great - especially here where we have splitten up our work piece by piece, and especially while learning the basics of R.

But you can see in the code chunk above that we ended up having a bunch of intermediate dataframes and vectors that we then had to clean up to keep our virtual workspace tidy. These intermediate steps are great, because we can look at them as we go and make sure our code is working right.

But there is also another way. A tidy way. And one that is arguably easier to read for most cases (and that enables all sorts of additional methods of doing things). This way of doing things, that keeps everythign neat and tidy, and standardises how functions are called is known as the [tidyverse](https://hbctraining.github.io/Intro-to-R/lessons/08_intro_tidyverse.html). The most important part of the tidyverse being the use of [pipes](https://www.datacamp.com/community/tutorials/pipe-r-tutorial).

Below I'll write equivalent code to all of the previous 18 sections using pipes and much of the logic of the tidyverse (although I don't use [tibbles](https://r4ds.had.co.nz/tibbles.html) - this is a personal preference because I don't like tibbles, and probably not one you should follow).

The main thing to understand about pipes is that code written for using pipes always takes the name of a dataframe or vector as its first argument, and that we use pipes to pass these arguments without actually specifying them. We see this the first time a pipe is used below: Normally the function expand.grid() takes two arguments - generally vectors and outputs the combination of them as a dataframe.

Here we start with a vector of colors, then pipe it into expand.grid(), so our expand.grid() takes only a single argument (in this case, the same vector). After that line of code is run, we now have a dataframe stored in memory, which we can then pipe into the next function we call: subset(), and so on and so forth. In this section I make use of a variety of functions for the first time - ones that you will need to become well acquainted with to make use of the tidyverse. They are as follows:

At the start of our pipe we are going to use traditional assignment to put the ultimate outcome of our pipe into a dataframe. So in the first case we get all the way to our "Participants" dataframe - which is all combinations of Bird ID (Bands color) and Trial Type (Dawn vs. Afternoon) before we need to close our pipe.

##### Setting the Seed
One thing to return to before this chunk of code is my use of [set.seed](http://rfunction.com/archives/62) in the very first block of code after the setup block. 

In R (in all languages actually), whenever we use a function that relies on randomness - for example the sample() function, R decides how to do this based on a random seed. So every time we call sample() we'll get a different value. E.g. if we called sample(1:100, 5), which samples 5 numbers randomly between 1 and 100, we might get the following: \
\
**Sample 1:** 37, 88, 18, 55, 53 \
**Sample 2:** 5, 44, 75, 16, 78 \
**Sample 3:** 49, 82, 39, 3, 92 \

We can see that all of these samples are different from each other, and most of the time that's exactly what we want when we use functions that rely on randomness. But that's only true **Most of the time**. Sometimes we actually want to be able to reproduce **exactly** what we've done, not just the general process.

This is exactly the case for generating stimlists. Yes, we want some random sampling - but we also want to be able to show our work in the strictest sense of the word - to be able to run the same chunk of code and get the same output every time. This is the benefit of setting the seed - it ensures that every time we use a function we will get the same results. 

So, if for example I call the sample sample() functions at above, but before each call I set the seed for R, we'll always get the same results: \
\
 **Sample 1:** 31, 79, 51, 14, 67 \
 **Sample 2:** 31, 79, 51, 14, 67 \
 **Sample 3:** 31, 79, 51, 14, 67 \

It actually goes further than this though- setting the seed doesn't just ensure that the first random function we call will give us the same value - it ensures that **every** random number we generate after we set the seed is the same. So if we set our seed and generate 3 samples, then re-set the seed and do so again, we will get all the same values every time:\
\
 **Sample 1A:** 31, 79, 51, 14, 67 : **Sample 1B:** 42, 50, 43, 14, 25 : **Sample 1C:** 90, 91, 69, 99, 57\
 **Sample 2A:** 31, 79, 51, 14, 67 : **Sample 2B:** 42, 50, 43, 14, 25 : **Sample 2C:** 90, 91, 69, 99, 57\
 **Sample 3A:** 31, 79, 51, 14, 67 : **Sample 3B:** 42, 50, 43, 14, 25 : **Sample 3C:** 90, 91, 69, 99, 57\

And this is the exact benefit for us. In completed code for setting up stimlists or doing anything else, we can simply set our seed once at the start, and then every time we run our code we will get the same results - meaning that our work is totally reproducible.

Below, I'm going to use set.seed() for exactly this kind of reason. You will recall above that for any given bird, we want to make sure that the don't select the same base songs for their Dawn and Afternoon stimtracks. Normally, we'd write the code to generate our stimtracks programatically, using either a for loop or a function (which we'll do below - but here we're focused on a single case for ease), so we'd only need to set our seed once. Here, however, we've made our Dawn Stimtrack for Bird YB already, and now we're going to make our Afternoon Stimtrack. 

There are several ways that we could ensure that we don't pick the same songs for our Afternoon stimtrack that we did for our Dawn stimtrack - the most obvious one being to simply choose songs randomly then just make sure they haven't been used before. But in this case, we're going to ensure we don't use the same songs by settings the same seed, which will also ensure that when we do things like index referencing, as we do when we call an inline code chunk like Participants$BirdID[5] we're still referencing the same bird that we have been above.

We don't, however, want **everything** we do in the process of generating our Afternoon Stimtrack to be identical to how the Dawn stimtrack was generated. For example we don't want the same randomised order of Song Speeds etc - so before that second section of code we will set a new seed.

For more information about setting seeds, see [Appendix 5- Setting Seeds](#SettingSeeds) and [Appendix 6- Setting Seeds- Yoking](#SettingSeeds- Yoking).

And now, on with the show! Some parts of the code will remain identical and not be rewritten as pipes, although in these cases much of the code has been tightened up.

#### The Single Case- Piped

What we start with has no pipes - its just a way to clean our directory, copy, and rename files that takes up less space and doesn't generate any intermediate dataframes or vectors that we need to clean up from our workspace.

In a case like this, how you prefer to do these things will ultimately be up to you - this takes less space as a code chunk, but that's at the cost of readability for novices - if you prefer to do things with the intermediate steps to track your work as you go, then you do you!


```r
#Getting ourselves the same initial seed
set.seed(49) 

#Make Sure Folder is empty
file.remove(dir(here("Simulated/Stimuli-Shared/Source Files"), pattern = ".wav", full.names = TRUE))

#Make a copy of the files in folder
file.copy(from = dir(here("Simulated/Stimuli-Shared/Source Files/Original"), pattern = ".wav", full.names = TRUE),
          to= here("Simulated/Stimuli-Shared/Source Files")) 

#Rename files
file.rename(from = dir(here("Simulated/Stimuli-Shared/Source Files"), pattern = ".wav", full.names = TRUE),
            to = paste(here("Simulated/Stimuli-Shared/Source Files"), "/",  
                  rep("Song ", 20),   #Just the word "Song" 20 times
                  rep(1:20, 3), "-", # The numbers 1:20 3 times in a row
                  rep(c("Control", "Fast", "Slow"), each = 20), #"Control" 20 times, then "Fast" 20 times, etc.
                  ".wav", sep = "" )) 
```
In the below chunk we make use of pipes to create our list of "Participants" (combinations of Bird and Trial Type (Dawn vs. Afternoon)). In the process of doing this, we make use of a few new functions:

[unite](https://www.rdocumentation.org/packages/tidyr/versions/0.8.3/topics/unite) takes two or more columns and creates a new column with them - its effectively the same thing as calling [paste](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste) but cleaner and part of the tidyverse. In this case we want to retain our original columns from which we constructed our new one, so we include the argument "remove = FALSE"
\
[pull](https://dplyr.tidyverse.org/reference/pull.html): Sometimes we have a dataframe but what we want from it is only a single column. We can use pull to tell R to pass only one of the columns of our dataframe (as a vector) to the next function. in this case, we're passing the BirdID into [expand.grid](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid), which is why expand.grid only has one stated argument (the BirdIDs are piped into the first argument, so this is equivalent to "expand.grid(df$BirdID,c("Dawn", "Afternoon"))".
\
[setNames](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/setNames) is a way to set the names of the columns of a dataframe. This is an alternative to colnames, which we've used above, but it works easily with pipes, as opposed to colnames, which we'd have to call someone awkwardly as: "`colnames<-`(c("BirdID", "TrialType"))" (this is actually calling the generic function that colnames() itself normally calls)


```r
#Set up Bird IDs from combinations of colors
Participants <-                                             #The saved dataframe that we pipe this all out to
  c("B", "W", "O", "R", "G", "Y") %>%                       #set up a vector
    expand.grid(c("B", "W", "O", "R", "G", "Y")) %>%        #combine that vector with itself into a dataframe of permutations
      subset(Var1 != Var2) %>%                              #exclude rows of the dataframe where the two colors are the same
        unite("BirdID", Var1:Var2, sep= "", remove = FALSE) %>%     #Unite two columns into a new column
          pull(BirdID) %>%                                  #Pipe only the BirdID column to the next function
            expand.grid(c("Dawn", "Afternoon")) %>%         #Combine BirdIDs and Trialtypes
              setNames(c("BirdID", "TrialType"))            #Rename columns
```

In the below chunk, we again bring in a few new functions:

[mutate/transmute](https://dplyr.tidyverse.org/reference/mutate.html) is a big one in the tidyverse - its very flexible and can be used for all sorts of things. mutate() adds new variables (usually columns) while preserving the original ones, whereas transmute adds new variables and drops the existing one. In both cases, we use "=" assignment - on the left of this assignment is the name of the column we are going to create, and on the right is what is going into that column. In the tidyverse, "=" is the type of assignment used **within** pipes because of the order of evaluation of assignment. Otherwise it's normally frowned upon in R to use "=" assignment.

Here we call transmute to remove the ".wav" extension from our SongData column, and then put this back into a column called the same thing (SongData). Because of this, transmute() and mutate() are exactly equivalent in this case - mutate takes the data in the column, modifies, and puts it back into the colums, wheareas transmute takes the data from the column, modifies it, deletes that column, and then creates a new column (with the same name) into which it puts the data.

[sample_n](https://dplyr.tidyverse.org/reference/sample.html) is pretty simple. It's the same as sample(), which we used above, but it takes a data frame is its main argument and samples rows of that data frame

In our second use of mutate() we could not use transmute()- becaues what we're putting into our new "Speed" column doesn't actually reference any of the other columns in the dataframe, R assumes somehow that we've referenced **all of them** and deletes every column of the dataframe except the new one we've created. Its probably for this reason that you don't see transmute() nearly as often on stackexchange as mutate()

[select](https://dplyr.tidyverse.org/reference/select.html) is pretty simple - its a way we can choose columns to maintain or delete from a dataframe. It is the same as calling subset(df, select = -c("SongType")). 

This is also the first time in this document that you'll have seen **"::"** associated with a function call. In this case, there are several functions in r and the packages we've loaded called "select" - so when we just call select() it doesn't work. the "::" operator tells R what package we want it to grab the function from (we do the same below for slice). Some people argue that we should use :: for **all** function calls to make code more transparent -whether you want to do that is up to you.

[slice](https://dplyr.tidyverse.org/reference/slice.html) should be familiar to you if you've coded in basically any language. For a single vector, e.g. c(1:6) (1, 2, 3, 4, 5, 6) we can take slices just by using the [vector index](https://www.r-tutor.com/r-introduction/vector/vector-index) **[]**, which you'll have seen a few places through this code. So, for example with our list of the numbers 1 to 6, we could slice the 2nd to 4th number in the list with "c(1:6)[2:4]" and get 2, 3, 4. Vector indexes are super useful, and can do [all sorts of things](http://www.r-tutor.com/r-introduction/vector/numeric-index-vector). You'll see them everywhere in R code, so make sure you're familiar with them. 

The dplyr version of slice is handy because it works within the tidyverse, and it knows that we want to slice the **rows** of a dataframe. We could slice these rows via their numeric index, but it would be more awkward- the equivalent of slice(df, 4:6) would be df[4:6,]. Why the , with no argument after it? Because when we [index a data](https://rspatial.org/intr/4-indexing.html) frame we must supply both the row values (first) and the column values (second). The blank after the comma tells R that we want **all** of the columns. within R, we'd have to pipe to ".[4:6,]", which is awkward and not very transparent


```r
AfternoonSongs <- 
  as.data.frame(dir(here("Simulated/Stimuli-Shared/Source Files"), pattern = ".wav")) %>%   #Retrieve the filenames 
    setNames("SongData") %>%                                                                #Set our column name
      transmute(SongData = str_replace_all(SongData, ".wav", "")) %>%                          #Delete ".wav"
        separate(SongData, into = c("SongNum", "SongType"), sep = "-", remove = FALSE) %>%  #Separate into two new columns
          subset(SongType == "Control") %>%                                                 #Subset to only controls
            sample_n(6) %>%                                                                 #sample(), but for a dataframe
              mutate(Speed = as.vector(replicate(2, (sample(c("Control", "Fast", "Slow"), 3))))) %>%    #New column speeds 
                unite(SongData, c("SongNum", "Speed"), sep = "-", remove = FALSE) %>%      #Unite into a new column
                  dplyr::select(-SongType) %>%  #Get rid of a useless column
                    dplyr::slice(4:6)  %>%                                                  #Take bottom 3 rows of the df
                      mutate(SongSel = c("Aft1", "Aft2", "Aft3"))                     #Adding a songsel column we'll use later
```

Nothing exciting to say about the next chunk - its just a bit more compact than how we did it previously, and again doesn't have intermediate dataframes/vectors


```r
#Make sure destination folder is empty
file.remove(dir(here("Simulated/Single/Stimuli/Afternoon"), pattern = ".wav", full.names = TRUE))

#Copy the chosen files into the Afternoon Folder
file.copy(from = here("Simulated/Stimuli-Shared/Source Files", paste(AfternoonSongs$SongData, ".wav", sep = "")) ,
          to =  here("Simulated/Single/Stimuli/Afternoon", paste(AfternoonSongs$SongData, ".wav", sep = ""))       )
```

We now assemble our AfternoonSongsList dataframe and make almost all the changes we need to it in one place (other than outputting it as a csv). We again use quite a few new functions

[rep_sample_n](https://www.rdocumentation.org/packages/infer/versions/0.5.3/topics/rep_sample_n) replaces our previous use of replicate(sample()) because it samples from a dataframe (as sample_n) instead of a vector.

rep_sample_n has one annoying feature - it outputs the data as a tibble, rather than a dataframe, and it [groups](https://dplyr.tidyverse.org/reference/group_by.html) that tibble by the "replicate" column that it adds. These two things, unchecked, cause problems with two of the functions we call later, so we get rid of them here. 

[ungroup](https://dplyr.tidyverse.org/reference/group_by.html) removes the grouping of the tibble.
[data.frame](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame) turns our tibble into a data frame

The line of code that I've commented on as "WOAH!" is by far the most complex line of code we've seen so far. And its really not very "Tidy". Unfortunately its a necessary evil. so I'll explain it:

Recall that for standard code we read from the inside out (which is why nesting can become much more confusing, and which is the main reason for using Tidy constructions). Our innermost function is paste(), which we've seen before - here we're just using it to create a full filename, which we need to pass to our next function: load.wave(). 

We've also used load.wave before - all it does is reads a .wav file into R as an audioSample object. But why is load.wave to the right of paste, and why isn't load.wave taking any arguments? Because we're calling it within [lapply](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply). lapply stands for "list apply" - which is still pretty opaque. But what lapply does is apply a function (in this case load.wave()) to every element of a list or vector, and then collect those results as a list. So the first time we call lapply() here, we're telling R: "Here is a list of filenames - load all of them and make a list object where each entry is an audiosample" (you can see this for yourself if you call lapply(paste(AfternoonSongsList$SongPath, ".wav", sep=""), load.wave)). The [apply family](https://nicercode.github.io/guides/repeating-things/) is super useful and worth looking into - there are many cases where it is the only way to accomplish a task without writing a for loop (which we'll deal with below)

audioSamples are matrices that cannot be turned into dataframes, and also can't be put into a dataframe as a column of their own, and that is why we immediately call another function on our audioSamples: [duration](https://rdrr.io/cran/seewave/man/duration.html), again using lapply. This outputs a list of 30 elements - the durations of each of the soundfiles, which you can see yourself if you run "lapply(lapply(paste(AfternoonSongsList$SongPath, ".wav", sep=""), load.wave), duration)".

So we now have a column called Duration, which has the duration of each call in it. But it's a bit weird - if at this point you were to take a look at our data frame using [str(AfternoonSongsList)](https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/str), which displays the internal structure of the dataframe, you'd see that Duration is still stored in the dataframe as a list. We need to rectify this, because with Duration being a list, rather than a numeric vector, we can't modify it with the code where we call cumsum() two lines below. To do this we use the [unlist](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/unlist) function, which turns our list back into a vector intelligently. (we could also use [flatten](https://purrr.tidyverse.org/reference/flatten.html) for this purpose).


```r
#We now do everything with this dataframe, including getting it in the right format, in one place (minus a few lines at the end)             
AfternoonSongsList <-
  AfternoonSongs %>%
    rep_sample_n(size = 3, reps = 10) %>%
      ungroup() %>%
        data.frame() %>%
          mutate(SongPath = paste(here("Simulated/Single/Stimuli/Afternoon"), "/", 
                                 SongData, sep = "" )) %>%
           mutate(Duration = lapply(lapply(paste(SongPath, ".wav", sep=""), load.wave), duration)) %>% #WOAH!
            mutate(Duration = unlist(Duration)) %>%
              mutate(`End Time (s)` = cumsum(Duration + 10)) %>%
                mutate(`Begin Time (s)` = `End Time (s)` - Duration ) %>%
                  mutate(Selection = 1:30) %>%
                  mutate(View = "Waveform 1") %>%
                  mutate(Channel = 1) %>%
                  mutate(`Low Freq (Hz)` = 1) %>%
                  mutate(`High Freq (Hz)` = 1) %>%
                  mutate(Bird = Participants$BirdID[5]) %>%
                  mutate(`Trial Type` = "Afternoon") %>%
                  mutate(`Signal Type` = "S") %>%
                  mutate(`Stim Type` = Speed) %>%
                  mutate(Response = "") %>%
                  mutate(Overlapping = "") %>%
                  mutate(Notes = "") %>%
                  mutate(Duration = Duration) %>%
                  mutate(SongPath2 = paste(SongPath, "-", SongSel, "-", "Silent.wav", sep = "")) %>%
                    subset(select = c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", 
                                    "Low Freq (Hz)", "High Freq (Hz)", "Bird", "Trial Type", 
                                    "Signal Type", "Stim Type", "Response", "Overlapping", "Notes", 
                                    "SongNum", "SongSel", "Duration", "SongPath", "SongPath2")) 
```

Below we use pipes to greatly simplify the creation of our wave files to load in the original files, add 10 seconds of silence to start of them, then output those modified files


```r
#Song 1
here("Simulated/Single/Stimuli/Afternoon", paste(AfternoonSongs$SongData[1], ".wav", sep = "")) %>%
  load.wave() %>%
    addsilw(at = "start", d= 10, output = "audioSample") %>%
      save.wave(where = here("Simulated/Single/Stimuli/Afternoon", 
                  paste(AfternoonSongs$SongData[1],"-Aft1-","Silent", ".wav", sep = "")) )

#Song 2
here("Simulated/Single/Stimuli/Afternoon", paste(AfternoonSongs$SongData[2], ".wav", sep = "")) %>%
  load.wave() %>%
    addsilw(at = "start", d= 10, output = "audioSample") %>%
      save.wave(where = here("Simulated/Single/Stimuli/Afternoon", 
                  paste(AfternoonSongs$SongData[2],"-Aft2-","Silent", ".wav", sep = "")) )
#Song 3
here("Simulated/Single/Stimuli/Afternoon", paste(AfternoonSongs$SongData[3], ".wav", sep = "")) %>%
  load.wave() %>%
    addsilw(at = "start", d= 10, output = "audioSample") %>%
      save.wave(where = here("Simulated/Single/Stimuli/Afternoon", 
                  paste(AfternoonSongs$SongData[3],"-Aft3-","Silent", ".wav", sep = "")) )
```

More pipes. More saved trouble. And another super important function: [do.call](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/do.call).

lapply takes a list of arguments and applies a function to every element in that list, then outputs a list of the outputs of each run of that function

do.call works similarly - but instead, it passes a list into a function, with each element of that list being an argument for the function. In this case, this allows us to append all of our wave files into a single large audioSample (using appendSample). You'll recall that when we called appendSample before, we did it very awkwardly by using the vector index of every single line of the dataframe in order, so do.call() saves us a **ton** of work here. Its another function you should make sure you're familiar with!


```r
as.list(AfternoonSongsList$SongPath2) %>%
  do.call(appendSample, .) %>%
    saveSample(filename = here("Simulated/Single/Stimuli/Afternoon", paste("Stimtrack-Afternoon", ".wav", sep = "")), 
           overwrite = TRUE)
```

Above, you may have noticed that when we used do.call() we had a "." in place of the second argument for do.call, which is normally where the list of arguments we want to use in the function goes. Why was this the case?

When we're using pipes, the default is that whatever we pipe into a function takes the place of the first argument of that function. But this is not always what we want. Above, the first argument of do.call() is the function do.call is using - but we want to pass our list to do.call, and the list goes in the second position. This is what the "." does - it tells R to override its assumption about where we are piping something into.

In the chunk below, we use this with the interleave() function, because we want the lines containing "Waveform 1" in the View column to appear before those containing "Spectrogram 1" - so we need the mutated version of the songlist with "Spectrogram 1" to be the second argument to interleave.

```r
# Duplicating our dataframe, changing the "View" column to "Spectrogram 1", and then pasting it all back together and exporting as a csv

AfternoonSongsList %>%
  mutate(View = "Spectrogram 1") %>%
      interleave(AfternoonSongsList, .) %>%
        write.csv(file = here("/Simulated/Single/Stimlists", 
          paste(AfternoonSongsList$Bird[1], "-", AfternoonSongsList$`Trial Type`[1], "-StimTrack.csv", sep ="")))
```



