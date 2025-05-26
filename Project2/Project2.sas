/* Import the data */
/* problem 1 */
DATA geo;
    INFILE '/home/u63906298/STAT 6338/1366903.txt' DLM=' ' MISSOVER;
    INPUT ID County $ State $ Land_Area Population_1990 Per_18_24 Per_65_older 
          Active_Physicians Hospital_Beds Serious_Crimes Per_HS_Grads 
          Per_College_Grads Per_Poverty Per_Unemployment Per_Capita_Income 
          Total_Personal_Income Region;
    
    /* Compute crime rate */
    Crime_Rate = Serious_Crimes / Population_1990;
    
    /* Creating a poverty level categories */
    IF Per_Poverty < 6 THEN Poverty_Level = 'Under 6%';
    ELSE IF Per_Poverty >= 6 AND Per_Poverty < 10 THEN Poverty_Level = '6-10%';
    ELSE Poverty_Level = '10% or more';
RUN;
/* Perform ANOVA */
PROC GLM DATA=geo;
    CLASS Poverty_Level Region;
    MODEL Crime_Rate = Poverty_Level | Region;
    lsmeans Poverty_Level | Region;
    TITLE "ANOVA for Crime Rate";
RUN;


/* Regression model */
PROC REG DATA=geo;
    MODEL Crime_Rate = Region Per_Poverty;
    TITLE "Regression Model for Crime Rate Prediction";
RUN;

/* Residual analysis */
PROC UNIVARIATE DATA=geo;
    VAR Crime_Rate;
    HISTOGRAM / NORMAL;
    TITLE "Residual Analysis for Crime Rate";
RUN;


/* Regression model with dummy variables */
PROC GLM DATA=geo;
    CLASS Region Poverty_Level;
    MODEL Crime_Rate = Region Poverty_Level Region*Poverty_Level/SOLUTION;
    OUTPUT OUT=residuals P=Predicted R=Residual;
    TITLE "Regression Model for Crime Rate";
RUN;
/* Create residual dot plots */
PROC SGPLOT DATA=residuals;
    DOT Residual / GROUP=Region;
    TITLE "Residual Dot Plot by Region";
RUN;

PROC SGPLOT DATA=residuals;
    DOT Residual / GROUP=Poverty_Level;
    TITLE "Residual Dot Plot by Poverty Level";
RUN;

/* Formating the Output for Readability */
PROC PRINT DATA=geo;
    VAR Crime_Rate Poverty_Level Region;
    TITLE "Formatted Output of Regression Model";
RUN;

/* Regression model */
PROC REG DATA=geo OUTEST=estimates_table;
    MODEL Crime_Rate = Region Per_Poverty / CLB;
    TITLE "Regression Model for Crime Rate Prediction with Parameter Estimates";
RUN;

/* Displaying the parameter estimates */
PROC PRINT DATA=estimates_table;
    TITLE "Parameter Estimates for Regression Model";
RUN;
/* ----------------------- */

/*  */
/* Step 1: Importing the Data */
/* DATA cdi; */
/*     INFILE '/home/u63906298/STAT 6338/1366903.txt' DLM=' ' MISSOVER; */
/*     INPUT Id County $ State $ Land population aged1 aged2  */
/*           physicians beds crimes school college poverty  */
/*           unemployment Percapita income region; */
/* RUN; */
/*  */
/* Step 2: Handling Missing Values */
/* DATA cdi_clean; */
/*     SET cdi; */
/*     IF population = . OR crimes = . OR poverty = . THEN DELETE; */
/* RUN; */
/*  */
/* Step 3: Creating New Variables */
/* DATA cdi_new; */
/*     SET cdi_clean; */
/*     Create categories for poverty */
/*     IF poverty < 6 THEN pov = '1'; */
/*     ELSE IF poverty >= 6 AND poverty < 10 THEN pov = '2'; */
/*     ELSE pov = '3'; /* 10% or more */
/*      */
/*     Calculate crime rate */
/*     crime_rate = crimes / population; */
/* RUN; */
/*  */
/* Step 4: Checking Data Structure */
/* PROC CONTENTS DATA=cdi_new; */
/* RUN; */
/*  */
/* Step 5: ANOVA Model (Equivalent to aov in R) */
/* PROC GLM DATA=cdi_new; */
/*     CLASS pov region; */
/*     MODEL crime_rate = pov|region; */
/*     TITLE "ANOVA Model for Crime Rate"; */
/* RUN; */
/*  */
/* Step 6: Regression Model (Equivalent to lm in R) */
/* PROC REG DATA=cdi_new; */
/*     MODEL crime_rate = pov region pov*region; */
/*     TITLE "Regression Model for Crime Rate"; */
/* RUN; */
/*  */
/* Step 7: Residual Analysis */
/* PROC GLM DATA=cdi_new OUTSTAT=ResidualOutput; */
/*     CLASS pov region; */
/*     MODEL crime_rate = pov region pov*region; */
/*     OUTPUT OUT=Residuals R=Residuals; */
/*     TITLE "Residual Analysis for Crime Rate"; */
/* RUN; */
/*  */
/* Residual Dot Plots */
/* Residuals by Poverty Level */
/* PROC SGPLOT DATA=Residuals; */
/*     DOT Residuals / GROUP=pov; */
/*     TITLE "Residual Dot Plot by Poverty Level"; */
/* RUN; */
/*  */
/* Residuals by Region */
/* PROC SGPLOT DATA=Residuals; */
/*     DOT Residuals / GROUP=region; */
/*     TITLE "Residual Dot Plot by Region"; */
/* RUN; */
/*  */
/* Step 8: Reduced Model (No Interaction Terms) */
/* PROC REG DATA=cdi_new; */
/*     MODEL crime_rate = pov region; */
/*     TITLE "Reduced Model for Crime Rate"; */
/* RUN; */
/*  */
/* Step 9: Compare Models Using F-test (ANOVA) */
/* PROC GLM DATA=cdi_new; */
/*     CLASS pov region; */
/*     MODEL crime_rate = pov region / SS3; */
/*     TEST H=region E=pov; */
/*     TITLE "F-Test Comparing Models"; */
/* RUN; */

/* ________________________________________ */
/* problem 2 */
/* For purposes of this ANOVA study, percent below poverty level is to */
/* be classi ed into two categories (< 8%,   8%) and percent of population 65 or older is to be classi ed */
/* into two groups (< 12%,   12%) */

DATA crime_data;
    INFILE '/home/u63906298/STAT 6338/1366903.txt' DLM=' ' MISSOVER;
    INPUT Id County $ State $ Land Population aged1 aged2 Physicians Beds Crimes
          School College Poverty Unemployment Percapita Income Region;
RUN;

/* Creating Categorical Variables for Poverty Level and Aged 65 or Older */
DATA crime_data_new;
    SET crime_data;
    /* Categorize percent below poverty level */
    IF Poverty < 8 THEN Poverty_Level = '<8%';
    ELSE Poverty_Level = '>=8%';
    
    /* Categorize percent of population 65 or older */
    IF aged2 < 12 THEN Aged_Group = '<12%';
    ELSE Aged_Group = '>=12%';
    
    /* Calculate the crime rate */
    Crime_Rate = Crimes / Population;
RUN;

/* Conduct Three-Way ANOVA */
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region|Poverty_Level|Aged_Group;
    MEANS Region Poverty_Level Aged_Group / TUKEY;
    OUTPUT OUT=residuals R=Residual;
    TITLE "Three-Way ANOVA for Crime Rate";
RUN;


/* Step 4: Diagnostic Procedure - Normality Test (Shapiro-Wilk) */
PROC UNIVARIATE DATA=residuals NORMAL;
    VAR Residual; /* Residuals from the ANOVA model */
    HISTOGRAM Residual / NORMAL; /* Create histogram with a normal curve */
    QQPLOT Residual; /* Generate Q-Q plot for visual normality check */
    TITLE "Shapiro-Wilk Test for Normality of Residuals";
RUN;


PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region|Poverty_Level|Aged_Group / SS3;
    OUTPUT OUT=residuals_data PREDICTED=Fitted_Values RESIDUAL=Residuals;
RUN;

PROC SGPLOT DATA=residuals_data;
    SCATTER X=Fitted_Values Y=Residuals / MARKERATTRS=(SYMBOL=CIRCLEFILLED COLOR=BLUE);
    REFLINE 0 / AXIS=Y LINEATTRS=(COLOR=RED THICKNESS=2);
    TITLE "Residuals vs. Fitted Values";
RUN;



/* Plot Residuals in Observation Order */
PROC SGPLOT DATA=residuals_data;
    SERIES X=_N_ Y=Residuals / MARKERS LINEATTRS=(COLOR=BLUE);
    REFLINE 0 / AXIS=Y LINEATTRS=(COLOR=RED);
    TITLE "Residuals in Sequential Order (Independence Check)";
RUN;


/* Plot Residuals in Observation Order */
PROC SGPLOT DATA=residuals_data;
    SERIES X=_N_ Y=Residuals / MARKERS LINEATTRS=(COLOR=BLUE);
    REFLINE 0 / AXIS=Y LINEATTRS=(COLOR=RED);
    TITLE "Residuals in Sequential Order (Independence Check)";
RUN;
/*individual models-Levene's test  */
PROC GLM DATA=crime_data_new;
    CLASS Region; /* Replace with the categorical variable you want to test */
    MODEL Crime_Rate = Region; /* Replace with your dependent variable */
    MEANS Region / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Equality of Variances */
    TITLE "Levene's Test for Equality of Variances (Region)";
RUN;

PROC GLM DATA=crime_data_new;
    CLASS Poverty_Level; /* Replace with another categorical variable to test */
    MODEL Crime_Rate = Poverty_Level; /* Replace with your dependent variable */
    MEANS Poverty_Level / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Poverty Levels */
    TITLE "Levene's Test for Equality of Variances (Poverty Level)";
RUN;

PROC GLM DATA=crime_data_new;
    CLASS Aged_Group; /* Replace with another categorical variable to test */
    MODEL Crime_Rate = Aged_Group; /* Replace with your dependent variable */
    MEANS Aged_Group / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Aged Group */
    TITLE "Levene's Test for Equality of Variances (Aged Group)";
RUN;

/*single model  */

PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group; /* Include all three categorical variables */
    MODEL Crime_Rate = Region Poverty_Level Aged_Group; /* Dependent variable */
    MEANS Region Poverty_Level Aged_Group / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for all factors */
    TITLE "Levene's Test for Equality of Variances Across All Factors";
RUN;

/* Plot Residuals in Observation Order */
PROC SGPLOT DATA=residuals_data;
    SERIES X=_N_ Y=Residuals / MARKERS LINEATTRS=(COLOR=BLUE);
    REFLINE 0 / AXIS=Y LINEATTRS=(COLOR=RED);
    TITLE "Residuals in Sequential Order (Independence Check)";
RUN;
/*individual models-Levene's test  */
PROC GLM DATA=crime_data_new;
    CLASS Region; /* Replace with the categorical variable you want to test */
    MODEL Crime_Rate = Region; /* Replace with your dependent variable */
    MEANS Region / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Equality of Variances */
    TITLE "Levene's Test for Equality of Variances (Region)";
RUN;

PROC GLM DATA=crime_data_new;
    CLASS Poverty_Level; /* Replace with another categorical variable to test */
    MODEL Crime_Rate = Poverty_Level; /* Replace with your dependent variable */
    MEANS Poverty_Level / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Poverty Levels */
    TITLE "Levene's Test for Equality of Variances (Poverty Level)";
RUN;

PROC GLM DATA=crime_data_new;
    CLASS Aged_Group; /* Replace with another categorical variable to test */
    MODEL Crime_Rate = Aged_Group; /* Replace with your dependent variable */
    MEANS Aged_Group / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for Aged Group */
    TITLE "Levene's Test for Equality of Variances (Aged Group)";
RUN;

/*single model  */

PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group; /* Include all three categorical variables */
    MODEL Crime_Rate = Region Poverty_Level Aged_Group; /* Dependent variable */
    MEANS Region Poverty_Level Aged_Group / HOVTEST=LEVENE(TYPE=ABS); /* Levene's Test for all factors */
    TITLE "Levene's Test for Equality of Variances Across All Factors";
RUN;

/* -------------------------------------------- */
/*2)ii)  */
/* Preparing AB Interaction Plots */
PROC MEANS DATA=crime_data_new NOPRINT;
    CLASS Region Poverty_Level;
    VAR Crime_Rate;
    OUTPUT OUT=means MEAN=Mean_Crime_Rate;
RUN;

PROC SGPLOT DATA=means;
    SERIES X=Region Y=Mean_Crime_Rate / GROUP=Poverty_Level MARKERS;
    TITLE "AB Interaction Plot: Region vs Poverty Level on Crime Rate";
RUN;


/* used PROC MIXED  Prepare AB Interaction Plot using PROC MIXED */
PROC MIXED DATA=crime_data_new METHOD=TYPE3;
    CLASS Region Poverty_Level;
    MODEL Crime_Rate = Region|Poverty_Level;
    LSMEANS Region*Poverty_Level / PDIFF SLICE=Region ADJUST=TUKEY;
    ODS OUTPUT LSMEANS=lsmeans; /* Save Least Square Means for plotting */
    TITLE "AB Interaction Analysis Using PROC MIXED";
RUN;

/* Plotting AB Interaction */
PROC SGPLOT DATA=lsmeans;
    SERIES X=Region Y=Estimate / GROUP=Poverty_Level MARKERS;
    TITLE "AB Interaction Plot: Region vs Poverty Level on Crime Rate (PROC MIXED)";
RUN;


/* -------------------------- */
/* 2)iii) */
/* Reduced Models for Interactions */
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region Poverty_Level Aged_Group 
                       Region*Poverty_Level 
                       Region*Aged_Group 
                       Poverty_Level*Aged_Group 
                       Region*Poverty_Level*Aged_Group / SS3;
    TEST H=Region*Poverty_Level*Aged_Group / E=Error;
    TITLE "Testing Three-Factor and Two-Factor Interactions";
RUN;
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region Poverty_Level Aged_Group 
                       Region*Poverty_Level 
                       Region*Aged_Group 
                       Poverty_Level*Aged_Group / SS3;
    TEST H=Region*Aged_Group / E=Error;
    TITLE "Reduced Model Testing Region*Aged_Group Interaction";
RUN;


/* --------------------------- */
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    /* Include all main effects and interactions in the MODEL statement */
    MODEL Crime_Rate = Region Poverty_Level Aged_Group 
                       Region*Poverty_Level 
                       Region*Aged_Group 
                       Poverty_Level*Aged_Group
                       Region*Poverty_Level*Aged_Group / SS3;
    /* No TEST statements are needed, results are displayed by default */
    TITLE "ANOVA for Main Effects and Interaction Terms";
RUN;



/* 2)iv */
/* Reduced Models for Main Effects */
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region Poverty_Level Aged_Group;
    TITLE "Testing Main Effects for Region, Poverty Level, and Aged Group";
RUN;

/* Optional: Pairwise Comparisons */
PROC GLM DATA=crime_data_new;
    CLASS Region Poverty_Level Aged_Group;
    MODEL Crime_Rate = Region Poverty_Level Aged_Group;
    MEANS Region Poverty_Level Aged_Group / TUKEY;
    TITLE "Post-Hoc Tests for Main Effects";
RUN;
/* ---------------------- */
/* problem 3-Part III */
/* This time assuming a mixed e ects model with  xed factors A and C (< 12%,   12%) and a random
factor B (1: under 4%, 2: 4-8%, 3: 8-12%). Derive a suitable model and conduct the suitable tests. */

PROC MIXED DATA=crime_data_new METHOD=REML; /* Default method for mixed models */
    CLASS Region Aged_Group Poverty_Level; /* Declare categorical variables */
    MODEL Crime_Rate = Region Aged_Group Region*Aged_Group / DDFM=SATTERTH; /* Fixed effects */
    RANDOM Poverty_Level / SUBJECT=Region; /* Random effect for Poverty_Level */
    TITLE "Mixed-Effects Model with Fixed and Random Factors";
RUN;


