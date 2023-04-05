# Data analysis performed with R for the research paper: E93 expression and links to the juvenile hormone in hemipteran mealybugs with insights on female neoteny


## Summary
This folder includes all data files for the R analyses performed in the research paper: [Vea et al. E93 expression and links to the juvenile hormone in hemipteran mealybugs with insights on female neoteny (Journal of Insect Biochemistry and Molecular Biology)](https://www.sciencedirect.com/science/article/abs/pii/S0965174818301152#preview-section-abstract).
 

## List of files

- [sample2profile-Krh1E93.csv](GITHUBLINK): qPCR data (SDM values) for rpL32, PkKr-h1 and PkE93 isoforms for male and female expression profiles

- [E93-Pyr-male.csv](GITHUBLINK): qPCR data of JHM treatment on males (See methods in the manuscript for details of treatment)

- [E93-Pyr-N3D0.csv](GITHUBLINK): qPCR data on JHM treatment on females (See methods in the manuscript for details of treatment)


## List of variables for each file
### sample2profile-Krh1E93.csv:
- cDNA.ID: unique ID
- previous.cDNA.ID: old unique ID
- Sex: male or female sample                   
- Day.after.oviposition: day after oviposition for each collected sample, starting 0 = 0 to 24 hours
- Stage: indicated the stage (E: embryo, L1: first instar nymph, L2: second instar nymph, L3: third instar nymph, f: female adult, m: male adult) and day within one stage.
- rpL32: SDM value obtained from absolute quantitative RT PCR of PkrpL32
- Kr.h1: SDM value obtained from absolute quantitative RT PCR of PkKr-h1
- E93.1: SDM value obtained from absolute quantitative RT PCR of PkE93-1
- E93.2: SDM value obtained from absolute quantitative RT PCR of PkE93-2
- E93.3: SDM value obtained from absolute quantitative RT PCR of PkE93-3

### E93-Pyr-N3D0.csv
- cDNA.ID: sample unique ID
- Compound: what type of treatment was undertaken for each sample. The control treatment was methanol ("Control"), and JHM treatment was 20mM of pyriproxyfen ("Mimic")      
- treatment.stage
- sex: female or male        
- Day: Day after treatment
- DayL: Day after treatment in letters (A=1, B=2 etc...), this was added to generate the graphs         
- N: number of individuals used for RNA extraction
- rpL32: SDM values of reference gene collected from absolute quantitative RT-PCR for normalization         
- Gene: Name of gene region
- SDM: SDM values collected from absolute quantitative RT-PCR	

### E93-Pyr-male.csv
- Sample.ID: sample unique ID
- treatment: what type of treatment was undertaken for each sample. The control treatment was methanol ("Control"), and JHM treatment was 20mM of pyriproxyfen ("Mimic")
- Day: Day after treatment
- DayL: Day after treatment in letters (A=1, B=2 etc...), this was added to generate the graphs 
- Gene: Name of gene region
- SDM.Gene: SDM values collected from absolute quantitative RT-PCR	
- SDM.rpL32: SDM values of reference gene collected from absolute quantitative RT-PCR for normalization 

## Scripts
To access expression profile and effect of JHM treatment graphs click [here](https://github.com/zourloubidou/E93mealybug/blob/master/RscriptE93.md)

To access statistical analyses of JHM treatments click [here](https://github.com/zourloubidou/E93mealybug/blob/master/E93_stats.md)
