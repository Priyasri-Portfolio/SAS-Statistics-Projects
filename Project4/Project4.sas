FILENAME research URL "https://raw.githubusercontent.com/keijioda/KNNL/main/chap24/CH24PR09.txt";
DATA marketing;
    INFILE research;
    INPUT y x1 x2 x3 x4;
    /* Where:
       y = quality rating (response variable)
       x1 = fee schedule (factor 1)
       x2 = work scope (factor 2)
       x3 = supervisory control (factor 3)
       x4 = replicate */
RUN;
/* ----------------------- */
/* problem 1)1) */
/* "conditional" interaction plot for x1(fee)-x2(work-scope);*/
proc sgpanel data=marketing;
panelby  x3/ rows=1 columns=2;
scatter x=x2 y=y / group=x1;
reg x=x2 y=y / group=x1;
rowaxis integer;
run; 
/* "conditional" interaction plot for x2(work-scope)-x3;*/
proc sgpanel data=marketing;
panelby x1 / rows=1 columns=3;
scatter x=x3 y=y / group=x2;
reg x=x3 y=y / group=x2;
rowaxis integer;
run; 
/*"conditional" interaction plot x1(fee)-x3(supervisory control);*/
proc sgpanel data=marketing;
panelby x2 / rows=1 columns=2;
scatter x=x3 y=y / group=x1;
reg x=x3 y=y / group=x1;
rowaxis integer;
run; 
/* --------------------------------------- */

* x2*x3 interaction averaged over x1=fee;
proc sgplot data=marketing;
title "Averaged over x1";
scatter x=x3 y=y / group=x2;
reg x=x3 y=y / group=x2;
xaxis integer;
run; 

/* x1*x2 interaction averaged over x3;*/
proc sgplot data=marketing;
title "Averaged over x3";
scatter x=x2 y=y / group=x1;
reg x=x2 y=y / group=x1;
xaxis integer;
run; 

/* x1*x3 interaction averaged over x2;*/
proc sgplot data=marketing;
title "Averaged over x2";
scatter x=x3 y=y / group=x1;
reg x=x3 y=y / group=x1;
xaxis integer;
run; 
/* Full three-way interaction model with Type III tests */
PROC GLM DATA=marketing;
    CLASS x1 x2 x3;
    MODEL y = x1|x2|x3/solution ss3;
    TITLE "Full Three-Way Interaction Model";
RUN;
/* * x1*x3 interaction averaged over x2=work-scope; */
proc sgplot data=marketing;
title "Averaged over x2";
scatter x=x1 y=y / group=x3;
reg x=x1 y=y / group=x3;
xaxis integer;
run; 
/* ---------------------------------- */
/*Question 1) 2)  */
PROC GLM DATA=marketing;
    CLASS x1 x2 x3;
    MODEL y = x1|x2|x3 / solution ss3;
    TITLE "Full Three-Way Interaction Model";
RUN;

/*Testing reduced models*/
PROC GLM DATA=marketing;
    CLASS x1 x2 x3;
    MODEL y = x1 x2 x3 x1*x2 x1*x2 x2*x3/solution ss3;
    TITLE "Full Three-Way Interaction Model";
RUN;
/* PROC GLM DATA=marketing; */
/*     CLASS x1 x2 x3; */
/*     MODEL y = x2|x3/solution ss3; */
/*     TITLE "Full Three-Way Interaction Model"; */
/* RUN; */

/* PROC GLM DATA=marketing outstat=full; */
/* CLASS X1 X2 X3; */
/* MODEL y = X1 X2 X3 X2*X3; */
/* RUN; */


/*question 1)3)  */
/*Reduced model based on results from Part 2 */
PROC GLM DATA=marketing PLOTS=DIAGNOSTICS RESIDUALS;
    CLASS x1 x2 x3;
    MODEL y = x1 x2 x3 x1*x2 x1*x3 x2*x3/ SOLUTION;
    
/*     Create residual plots for each factor */
    OUTPUT OUT=resids RESIDUAL=raw_resid PREDICTED=pred;
    TITLE "Reduced Model with Diagnostics";
RUN;
PROC UNIVARIATE DATA=resids NORMAL;
    VAR raw_resid;
    HISTOGRAM raw_resid / NORMAL;
    PROBPLOT raw_resid / NORMAL(MU=EST SIGMA=EST);
    TITLE "Univariate Analysis of Residuals";
RUN;

/* --------------------- */
/* Plot residuals vs each factor level */
PROC SGPLOT DATA=resids;
     SCATTER x=x1 y=raw_resid/ NAME="x1(fee)";
     xaxis label="Fee (x1)";
     yaxis label="Residuals";
    TITLE "Residuals vs fee(x1)";
RUN;

PROC SGPLOT DATA=resids;
    SCATTER x=x2 y=raw_resid/NAME="x2(work scope)";
    xaxis label="Work Scope (x2)";
     yaxis label="Residuals";
    TITLE "Residuals vs Work scope (x2)";
RUN;

PROC SGPLOT DATA=resids;
    SCATTER x=x3 y=raw_resid/NAME="Supervisory control(x3)";
    xaxis label="Supervisory control(x3)";
     yaxis label="Residuals";
    TITLE "Residuals vs Supervisory control (x3)";
RUN;

/*Question 1)4) */
/* Part 4: Tukey's pairwise comparisons */
PROC GLM DATA=marketing;
    CLASS x1 x2 x3;
    MODEL y = x1 x2 x3 x1*x2 x1*x3 x2*x3;
    Lsmeans x1 /pdiff adjust=TUKEY cl alpha=0.05;
    Lsmeans x2*x3 /pdiff adjust=bon cl alpha=0.05;
    TITLE "Tukey's Multiple Comparisons";
RUN;
/* ------------------------- */
/* Part 1: Create interaction plots for two-way interactions */
/* x1 * x2 interaction plot (Fee Schedule * Work Scope) */
/* PROC GLM DATA=marketing PLOTS=intplot; */
/*     CLASS x1 x2 x3; */
/*     MODEL y = x1|x2/solution; */
/*     TITLE "Interaction Plot: x1 * x2"; */
/* RUN; */
/*  */
/* x1 * x3 interaction plot (Fee Schedule * Supervisory Control) */
/* PROC GLM DATA=marketing PLOTS=intplot; */
/*     CLASS x1 x2 x3; */
/*     MODEL y = x1|x3/solution; */
/*     TITLE "Interaction Plot: x1 * x3"; */
/* RUN; */
/*  */
/* x2 * x3 interaction plot (Work Scope * Supervisory Control) */
/* PROC GLM DATA=marketing PLOTS=intplot; */
/*     CLASS x1 x2 x3; */
/*     MODEL y = x2|x3/solution; */
/*     TITLE "Interaction Plot: x2 * x3"; */
/* RUN; */
/*  */
/* Part 2: Full three-way interaction model with Type III tests */
/* PROC GLM DATA=marketing; */
/*     CLASS x1 x2 x3; */
/*     MODEL y = x1|x2|x3/solution; */
/*     TITLE "Full Three-Way Interaction Model"; */
/* RUN; */



/* ------------------------------------------------ */
/*question 3  */
/* Uploading the data */

DATA pearls;
    INFILE "/home/u63906298/STAT 6338/CH25PR17.txt";
    INPUT res p1 p2 p3;
    /* Variable descriptions:
       res = response variable (luster measurement)
       p1 = factor A (coating)
       p2 = factor B (batch)
       p3 = replicate within batch */
RUN;
/* question 3)1) */

/* Fitting the model */
PROC GLM DATA=pearls;
    CLASS p1 p2; /* Declaring p1 and p2 as categorical factors */
    MODEL res = p1|p2 / solution; /* Interaction between p1 and p2 */
    OUTPUT OUT=output_data PREDICTED=Pred RESIDUAL=Resid; 
RUN;

/* Plot 1: Linear prediction across levels of batch for different coating */
PROC SGPLOT DATA=output_data;
    SERIES X=p2 Y=Pred / GROUP=p1 MARKERS; /* Batch(p2) levels by coating(p1) groups */
    XAXIS LABEL="Batch Levels";
    YAXIS LABEL="Predicted Response";
    TITLE "Linear Prediction Across Levels of Batch for Different Coating";
RUN;
/* Step 4: Plot 2: Linear prediction across levels of coating for different batch */
/* PROC SGPLOT DATA=output_data; */
/*     SERIES X=p1 Y=Pred / GROUP=p2 MARKERS; /* Coating levels by batch groups */
/*     XAXIS LABEL="Coating Levels"; */
/*     YAXIS LABEL="Predicted Response"; */
/*     TITLE "Linear Prediction Across Levels of Coating for Different Batch"; */
/* RUN; */
/* ----------------------------- */

/*Question3)2)Interaction model in PROC GLM and PROC GLIMMIX */
PROC GLM DATA=pearls;
    CLASS p1 p2;
    MODEL res = p1 p2 p1*p2;
    /* Test for interaction effect */
    TEST H=p1*p2 E=p1*p2;
    TITLE "GLM Analysis with Interaction";
RUN;
/* F critical value */
DATA critical_value;
    fc_value = FINV(0.95, 2, 6);   /* Computes F critical value with alpha = 0.05, DF = 2 and 6 */
    PUT fc_value;
RUN;
/* F statistics */
DATA F_statistic;
    MSB = 50.9505556;    /* Mean Square for Batch */
    MSAB = 0.3086806;    /* Mean Square for Batch Interaction */
    F_stat = MSB / MSAB; /* Calculate the F statistic */
    PUT "The F statistic for Batch is " F_stat; /* Display the F statistic */
RUN;

/* PROC GLIMMIX with REML (default) */
PROC GLIMMIX DATA=pearls METHOD=rspl;
    CLASS p1 p2;
    MODEL res = p1 / SOLUTION;
    RANDOM p2 p1*p2/solution;  /* p2 and p1*p2 as random effects */
    lsmeans p1/pdiff=all cl alpha=0.05;
    TITLE "GLIMMIX Analysis with REML Estimation";

RUN;
/* PROC GLIMMIX with REML (default) */
PROC GLIMMIX DATA=pearls METHOD=mspl;
    CLASS p1 p2;
    MODEL res = p1 / SOLUTION;
    RANDOM p2 p1*p2/solution;  /* p2 and p1*p2 as random effects */
    lsmeans p1/pdiff=all cl alpha=0.05;
    TITLE "GLIMMIX Analysis with REML Estimation";

RUN;

/* PROC GLIMMIX with ML */
/* PROC GLIMMIX DATA=pearls METHOD=mmpl; */
/*     CLASS p1 p2; */
/*     MODEL res = p1 / SOLUTION; */
/*     RANDOM p2 p1*p2;  /* p2 and p1*p2 as random effects */
/*     TITLE "GLIMMIX Analysis with ML Estimation"; */
/* RUN; */

/* question 3: Tests of main effects in PROC GLM */
/* Test fixed effect p1 and estimate variance component for p2 in GLIMMIX */
PROC GLIMMIX DATA=pearls method=mspl;
    CLASS p1 p2;
    MODEL res = p1/solution;
    RANDOM p2 p1*p2;  /* p2 as random effect */
    TITLE "GLIMMIX Analysis of Fixed Effect p1 and Random Effect p2";
RUN;
/* ------------------------- */

/* Part 4: Compare differences in factor p1 means */
PROC GLIMMIX DATA=pearls;
    CLASS p1;
    MODEL res = p1 / SOLUTION;
    LSMEANS p1 / PDIFF CL ADJUST=Tukey ALPHA=0.05;
    TITLE "Comparison of Factor p1 Means";
RUN;
/* ----------------------- */
/* Removing flawed observations */
DATA pearls_filtered;
    SET pearls;
    IF res NOT IN (67.4, 73.7); /* Removing flawed observations */
RUN;

/* question 5: ML estimates for fixed effect p1 and variance components */
PROC GLIMMIX DATA=pearls_filtered METHOD=MSPL;
    CLASS p1 p2;
    MODEL res = p1 / SOLUTION;
    RANDOM p2 p1*p2 / SOLUTION;
    COVTEST / WALD;  /* Test if variance components are zero */
    TITLE "MSPL Estimates for Fixed Effect p1 and Variance Components";
RUN;

/* --------------------------- */
/* Question 3.6: Variance Component Testing */
PROC GLIMMIX DATA=pearls_filtered METHOD=MSPL;
    CLASS p1 p2;
    MODEL res = p1/solution;
    RANDOM p2/solution;
    lsmeans p1/pdiff adjust=tukey alpha=0.05;

    COVTEST zerog / CL;
RUN;
/* --------------- */


