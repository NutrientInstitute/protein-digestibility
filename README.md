# Protein Digestibility Hub

The Protein Digestibility Hub Project serves as a dedicated repository for protein digestibility information, addressing a critical gap in accessible data. Recognizing this need through discussions at The International Symposium: Dietary Protein for Human Health and the FAO's call for a single source of digestibility information, our objective is to establish a centralized resource to support research and education. Leveraging our nonprofit status committed to advancing nutrition science through data, The Nutrient Institute aims to play a supportive role in leading this effort. The forthcoming open-access database, designed in alignment with FAIR principles and utilizing an ontology, will provide a structured and easily searchable repository tailored for protein scientists and beyond. It will be a comprehensive source encompassing digestibility values, metadata, and spanning all food types and analytical methodologies. Designed to be a valuable resource for a wide range of expertise, it aims to foster inclusivity by consolidating much-needed digestibility data in one place. The ultimate goal is to encourage collaboration and propel science forward through a unified language.

To provide feedback on this app or sign up to be notified about future releases, visit the [Nutrient Institute website](https://www.nutrientinstitute.org/protein-digestibility-feedback).


## What is in this repository?
Here you will find an [excel file](https://github.com/NutrientInstitute/protein-digestibility/blob/main/Protein%20Digestibility%20Data%20-%20data%20and%20documentation.xlsx) containing the most recent version of the data along with descriptions of provided variables as well as an R file which contains the code that runs the [Protein Digestibility Hub app](https://nutrientinstitute.shinyapps.io/ProteinDigestibilityData/), and the [CSV file](https://github.com/NutrientInstitute/protein-digestibility/blob/main/Protein%20Digestibility%20Data%20%20-%20full%20data.csv) the app pulls the data from.

The current version of the Protein Digestibility Hub, contains data collected from the following sources:
- [USDA ENERGY VALUE OF FOODS (Agricultural Handbook No. 74, 1955)](https://www.ars.usda.gov/arsuserfiles/80400535/data/classics/usda%20handbook%2074.pdf)
- AMINO-ACID CONTENT OF FOODS AND BIOLOGICAL DATA ON PROTEINS (FAO 1970) (<b>Note:</b> Original publication has been removed from FAO website, but can still be accessed via the [Wayback Machine](https://web.archive.org/web/20231125115519/https://www.fao.org/3/ac854t/AC854T00.htm))
- [Report of a Sub-Committee of the 2011 FAO Consultation on 'Protein Quality Evaluation in Human Nutrition'](https://www.fao.org/ag/humannutrition/36216-04a2f02ec02eafd4f457dd2c9851b4c45.pdf)
- [Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)](https://doi.org/10.17226/13298) <sup>1</sup>

<sup>1</sup>Digestibility and protein data from *Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)* was collected from the following sources:
1. AAFCO (Association of American Feed Control Officials). 2010. Official Publication. Oxford, IN: AAFCO.
2. AminoDat 4.0. 2010. Evonik Industries, Hanau, Germany.
3. Cera, K. R., D. C. Mahan, and G. A. Reinhart. 1989. Apparent fat digestibilities and performance responses of postweaning swine fed diets supplemented with coconut oil, corn oil or tallow. Journal of Animal Science 67:2040-2047.
4. CVB (Dutch PDV [Product Board Animal Feed]). 2008. CVB Feedstuff Database. Available online at http://www.pdv.nl/english/Voederwaardering/about_cvb/index.php. Accessed on June 9, 2011.
5. NRC (National Research Council). 1998. Nutrient Requirements of Swine,10th Rev. Ed. Washington, DC: National Academy Press.
6. NRC. 2007. Nutrient Requirements of Horses, 6th Rev. Ed. Washington, DC: The National Academies Press.
7. Powles, J., J. Wiseman, D. J. A. Cole, and S. Jagger. 1995. Prediction of the apparent digestible energy value of fats given to pigs. Animal Science 61:149-154.
8. Sauvant, D., J. M. Perez, and G. Tran. 2004. Tables of Composition and Nutritional Value of Feed Materials: Pigs, Poultry, Sheep, Goats, Rabbits, Horses, Fish, INRA, Paris, France, ed. Wageningen, the Netherlands: Wageningen Academic.
9. USDA (U.S. Department of Agriculture), Agricultural Research Service. 2010. USDA National Nutrient Database for Standard Reference, Release 23. Nutrient Data Laboratory Home Page. Available online at http://www.ars.usda.gov/ba/bhnrc/ndl. Accessed on August 10, 2011.
10. van Milgen, J., J. Noblet, and S. Dubois. 2001. Energetic efficiency of starch, protein, and lipid utilization in growing pigs. Journal of Nutrition 131:1309-1318.

**Please note that this project is actively under development and data is subject to change prior to a full release.** 
