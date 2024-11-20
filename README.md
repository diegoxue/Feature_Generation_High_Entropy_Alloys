# Feature_Generation_High_Entropy_Alloys

This is the code for paper " Elemental numerical descriptions to enhance classification and regression model performance for high entropy alloys".

generation-fc-class1.R: main function: The definition of individuals, the transformation of individuals into material features, and the evaluation of material features during the genetic algorithm process were highly customized. A logistic regression classifier with the calculated materials features in each individual was built and all data were employed to evaluate its classification accuracy as the fitness value.  At last, two vectors of numerical descriptions for elements with highest classification accuracy were identified for distinguishing solid solution (SS) and non-solid solution (NSS) phases.

 generation-fc-class2.R: main function: The definition of individuals, the transformation of individuals into material features, and the evaluation of material features during the genetic algorithm process were highly customized. A logistic regression classifier with the calculated materials features in each individual was built and all data were employed to evaluate its classification accuracy as the fitness value.  At last, two vectors of numerical descriptions for elements with highest classification accuracy were identified for recognizing FCC, BCC or dual phase.
