# Modeling Categories of Aliens with the GCM (Portfolio 4, ACM)
This repository contains the code for the `portfolio 4 assignment` in the course `Advanced Cognitive Modeling` at the Cognitive Science MSc. (F24).

Code was produced jointly by the group members:
* Milena Cholozynska (@milenacholo)
* Daniel Blumenkranz (@daniblu)
* Anton Drasbæk Schiønning (@drasbaek)
* Mina Almasi (@MinaAlmasi)

## Overview 
The repository contains four folders: 
1. `src` - all R scripts 
2. `data` - simulated data and data resulting from fitting models (samples from both simulated and real data)
3. `stan` - the GCM coded in Stan (git ignores the C++ compiled version)
4. `plots` - all plots

For the code in `src`, see the seperate [src/README.md](src/README.md).

## Usage 
### Setup
To use the code, ensure that you have `R` and `RScript` installed. All code was developed using `R` version `4.3.2` and was primarily tested on a MacBook. Note that you also need to have Stan properly installed (see [Rstan: Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)).

Furthermore, please install the package `pacman`. Within your `R console`, this can be done as such: 
```
install.packages("pacman")
```

### Run the code 
Code can be run by using `RScript` in your `bash` terminal:
```bash
RScript src/simulate.R
```