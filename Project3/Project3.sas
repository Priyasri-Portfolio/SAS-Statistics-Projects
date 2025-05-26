filename heal "/home/u63906298/HW/health.csv";
DATA h;
INFILE heal DSD FIRSTOBS = 2;
INput Death_Rate Doctor_Availability Hospital_Availability Capital_income Population_Density;
RUN;


/* Q1) Fitting the model including all predictors */
PROC REG DATA=h;
   MODEL Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density;
/*    OUTPUT OUT=residuals P=predicted   R=residual; /* Save predicted values and residuals */
   OUTPUT OUT=D RSTUDENT=R PREDICTED=P;
   TITLE "Model with All Predictors";
RUN;


/* make a multi-celled scatter plot*/
PROC SGSCATTER DATA=h;
PLOT (Death_Rate)*(Doctor_Availability Hospital_Availability Capital_income Population_Density);
title "Scatter plot Death Rate vs all predictors";
RUN;
 
/* Check correlation coefficients*/
PROC CORR DATA =h plots = matrix;
VAR Death_Rate Doctor_Availability Hospital_Availability Capital_income Population_Density; 
RUN;

/* Calculate absolute residuals for plotting */
data D;
   set D;
   abs_residual = abs(R);
run;

/* Plot absolute residuals vs predicted values */
proc sgplot data=D;
   SCATTER x = P Y =abs_residual;
   title "Absolute Residuals vs Predicted Values";
run;


/* Lack-of-fit test */
PROC REG DATA=h;
   MODEL Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / lackfit;
   OUTPUT OUT=D RSTUDENT=R PREDICTED=P;
   TITLE "Model with Lack-of-Fit Test";
RUN;

/* Breusch Pagan Test*/
PROC MODEL DATA=D;
PARMS b0 b1 b2 b3 b4;
Death_Rate = b0 + b1*Doctor_Availability + b2*Hospital_Availability + b3*Capital_income + b4*Population_Density;
/*Breusch-Pagan test for heteroscedasticity (BREUSCH) wrt the specified predictors*/
fit Death_Rate /WHITE BREUSCH=(Doctor_Availability Hospital_Availability Capital_income Population_Density);
/*Breusch-Pagan test for heteroscedasticity individually for each of the predictors*/
fit Death_Rate /BREUSCH=(Doctor_Availability);
fit Death_Rate /BREUSCH=(Hospital_Availability);
fit Death_Rate /BREUSCH=(Capital_income);
fit Death_Rate /BREUSCH=(Population_Density);
RUN;

/* Brown Forsythe Test for homogeneity of variance*/
/* Get the medians of the predictors*/
PROC UNIVARIATE DATA = D NOPRINT;
VAR Doctor_Availability Hospital_Availability Capital_income Population_Density;
OUTPUT OUT = Medians Median = MedDoctor Median = MedHospital Median = MedIncome Median = MedPopulation N=N;
RUN;

DATA Medians;
SET medians;
DO i = 1 TO N;
OUTPUT;
END;
RUN;

/*Test for homogeneity of residuals grouped by Doctor_availability*/
DATA DoctorAvailBF;
MERGE D Medians;
Group=(Doctor_Availability > MedDoctor);
RUN;

PROC GLM Data = DoctorAvailBF;
class Group;
model R = Group;
means Group / hovtest = BF;
run;

/*Test for homogeneity of residuals grouped by Hospital_availability*/

Data HospitalAvailBF;
MERGE D Medians;
Group=(Hospital_availability > MedHospital);
RUN;

PROC GLM Data = HospitalAvailBF;
class Group;
model R = Group;
means Group / hovtest = BF;
run;


/*Test for homogeneity of residuals grouped by capital_income*/
DATA CapitalBF;
MERGE D Medians;
Group = (Capital_income > MedIncome); 
RUN;

PROC GLM Data = CapitalBF;
class Group;
model R = Group;
means Group / hovtest = BF; 
run;

/*Test for homogeneity of residuals grouped by population density*/
DATA PopulationDBF;
MERGE D Medians;
Group = (PopD > MedPopulation); 
RUN;

PROC GLM Data = PopulationDBF; 
class Group;
model R = Group;
means Group / hovtest = BF; 
run;

/*Residual QQ Plot*/
PROC UNIVARIATE DATA = D NORMAL PLOT; /* Check normality of the studentized residuals */ 
VAR R;
RUN;

/*Q2-------*/
/* based on the q-qplot before trasformation,there were 3 data points below the line, I performed log transformation
still the transformed q-qplot data points were not on the line. 
So i am performing square trasformation to fix the left skewed data points.*/
/*square trasformation */
DATA transformed;
   SET h;
   square_Death_Rate = Death_Rate**2; /* Square transformation */
RUN;

PROC REG DATA=transformed;
   MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density;
   TITLE "Model with Squared Death Rate";
RUN;
/* Q-Q plot for the squared death rate */
PROC UNIVARIATE DATA=transformed;
   VAR square_Death_Rate;
   QQPLOT square_Death_Rate / NORMAL(MU=EST SIGMA=EST);
   TITLE "Q-Q Plot of Squared Death Rate";
RUN;
/*observered the q-qplot after trasformation, now the data points that were on the left fit the line, 
condluding that the square trasformation worked well*/

/* Lack-of-fit test */
PROC REG DATA=transformed;
   MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / lackfit;
   OUTPUT OUT=D RSTUDENT=R PREDICTED=P;
   TITLE "Model(transformed)with Lack-of-Fit Test";
RUN;

PROC REG DATA = transformed;
MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density/ lackfit;
/*save the studentized residuals and the predicted values*/
OUTPUT OUT=D RSTUDENT=R PREDICTED=P;
run;
PROC REG DATA=transformed;
   MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density;
/*    OUTPUT OUT=residuals P=predicted   R=residual; /* Save predicted values and residuals */
   OUTPUT OUT=D RSTUDENT=R PREDICTED=P;
   TITLE "Model with All Predictors";
RUN;


/* make a multi-celled scatter plot*/
PROC SGSCATTER DATA=transformed;
PLOT (square_Death_Rate)*(Doctor_Availability Hospital_Availability Capital_income Population_Density);
title "Scatter plot Death Rate vs all predictors";
RUN;
 
/* Check correlation coefficients*/
PROC CORR DATA =transformed plots = matrix;
VAR square_Death_Rate Doctor_Availability Hospital_Availability Capital_income Population_Density; 
RUN;


/* Breusch Pagan Test*/
PROC MODEL DATA=transformed;
PARMS b0 b1 b2 b3 b4;
square_Death_Rate = b0 + b1*Doctor_Availability + b2*Hospital_Availability + b3*Capital_income + b4*Population_Density;
/*Breusch-Pagan test for heteroscedasticity (BREUSCH) wrt the specified predictors*/
fit square_Death_Rate /WHITE BREUSCH=(Doctor_Availability Hospital_Availability Capital_income Population_Density);
/*Breusch-Pagan test for heteroscedasticity individually for each of the predictors*/
fit square_Death_Rate /BREUSCH=(Doctor_Availability);
fit square_Death_Rate /BREUSCH=(Hospital_Availability);
fit square_Death_Rate /BREUSCH=(Capital_income);
fit square_Death_Rate /BREUSCH=(Population_Density);
RUN;


/*Residual QQ Plot*/
PROC UNIVARIATE DATA = transformed NORMAL PLOT; /* Check normality of the studentized residuals */ 
VAR square_Death_Rate;
RUN;
/* Q3----------*/


/*  Fit the Full Model using the square Transformed Data */
DATA transformed;
   SET h;
   square_Death_Rate = Death_Rate**2; /* Square transformation */
  Title "Square Transformation";
RUN;

PROC REG DATA=transformed;
   MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density;
   TITLE "Model with Squared Death Rate-transformed";
RUN;


PROC GLM DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density /  SS1 SS3;
    TITLE "Full Model Analysis (transformed)- Type I and Type III SS";
RUN;



/* Partial correlation coefficient between Square_Death_rate and Capital_income Population_Density after */
/* adjusting for Doctor_Availability Hospital_Availability */

PROC CORR DATA= transformed;
VAR square_Death_Rate Capital_income Population_Density ;
PARTIAL Doctor_Availability Hospital_Availability;
RUN;


/*Model1: without Hospital_Availability */
PROC GLM DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Capital_income Population_Density / SS1 SS3;
    TITLE "Model without Hospital_Availability - Type I and Type III SS";
RUN;

/* Final model: without Hospital_Availability and Doctor_Availability */
PROC GLM DATA=transformed;
    MODEL square_Death_Rate = Capital_income Population_Density / SS1 SS3;
    TITLE "Model without Hospital_Availability and Doctor_Availability - Type I and Type III SS";
RUN;
/* Final Model: without Hospital_Availability, Doctor_Availability,Capital_income  */
/* PROC GLM DATA=transformed; */
/*     MODEL square_Death_Rate = Population_Density / SS1 SS3; */
/*     TITLE "Model without Hospital_Availability and Doctor_Availability - Type I and Type III SS"; */
/* RUN; */

/* Partial F test*/
PROC REG DATA = transformed;
MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density;
/* User-specified hypotheses about estimated parameters */
x1x2x3: test Doctor_Availability= Hospital_Availability = Capital_income= 0;
title "Partial F test removing Doctor_Availability Hospital_Availability Capital_income ";
RUN;


/*Q4--------------------------
 find the best model(s) using adjusted R2,
Cp, and BIC criterion. Also,  nd models using stepwise, forward, and
backward selection methods. Compare all these models.*/
/* PROC RSQUARE DATA =transformed; */
/*     MODEL square_Death_Rate = LENGTH Doctor_Availability Hospital_Availability Capital_income Population_Density / SELECTION=ADJRSQ CP BIC; */
/*     TITLE "Full Model with All Predictors (Transformed)"; */
/* RUN; */
/*model selection using adjusted R-square, Cp and BIC*/
PROC  REG DATA= transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / selection = ADJRSQ CP BIC;
    TITLE "Model Selection Using Adjusted R-Square, Cp, and BIC";
RUN;

PROC REG DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / SELECTION=CP ADJRSQ BIC;
    TITLE "Model Selection based on CP";
RUN;
/* Stepwise Selection */
PROC REG DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / SELECTION=STEPWISE SLENTRY=0.05 SLSTAY=0.05;
    TITLE "Model Selection Using Stepwise Selection Method";
RUN;

/* Forward Selection */
PROC REG DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / SELECTION=FORWARD SLENTRY=0.05;
    TITLE "Model Selection Using Forward Selection Method";
RUN;

/* Backward Selection */
PROC REG DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Hospital_Availability Capital_income Population_Density / SELECTION=BACKWARD SLSTAY=0.05;
    TITLE "Model Selection Using Backward Selection Method";
RUN;
/*Q5--best final model-------------*/
/* as per the results obtained from the question 4 we are going with the model with 3 variables */
PROC REG DATA=transformed;
    MODEL square_Death_Rate = Doctor_Availability Capital_income Population_Density;
    TITLE "Final Model with Population_Density as Predictor";
RUN;
/* small r^2 */
PROC CORR DATA=transformed;
    VAR square_Death_Rate Population_Density;
    PARTIAL Doctor_Availability Capital_income;
    TITLE "Partial Correlation Controlling for Doctor_Availability vs Doctor_Availability Capital_income";
RUN;
PROC CORR DATA=transformed;
    VAR square_Death_Rate Doctor_Availability;
    PARTIAL  Population_Density Capital_income;
    TITLE "Partial Correlation Controlling for Doctor_Availability vs Doctor_Availability Capital_income";
RUN;
PROC CORR DATA=transformed;
    VAR square_Death_Rate Capital_income;
    PARTIAL Doctor_Availability Population_Density;
    TITLE "Partial Correlation Controlling for Doctor_Availability vs Doctor_Availability Capital_income";
RUN;
/*Q6-----------*/
PROC REG DATA=transformed;
    MODEL square_Death_Rate = Capital_income Population_Density ;
    Output out=cp Predicted=Pred Rstudent=RS;
    TITLE "Simple Model with Population_Density as Predictor";
RUN;
/* without hospital_availability */
PROC REG DATA=transformed;
    MODEL Doctor_Availability= Capital_income Population_Density;
    Output out=Dr_cp Predicted=Dr_Pred Rstudent=Dr_R;
    TITLE "Reduced Model without Hospital_Availability";
RUN;
/* Merging the dataseta */
data merged;
merge cp Dr_cp;
Run;
 


proc corr data=merged;
var RS Dr_R;
run;

Proc reg data=transformed;
Model square_Death_Rate= Doctor_Availability Capital_income Population_Density;
output out=x predicted=P;
run;
proc corr data=x;
var square_Death_Rate P;
run;

/*Q7---------------*/
/* try1 */

/* Calculating the means for other predictors */
PROC MEANS DATA=transformed NOPRINT;
    VAR Doctor_Availability Capital_income Population_Density;
    OUTPUT OUT=mean_values MEAN=avg_Doctor_Availability avg_Capital_income avg_Population_Density;
RUN;

/* Creating the prediction datasets for Population Density */
DATA prediction_data;
    SET mean_values;
    
    /* Range of Population Density */
    DO Population_Density = 0 TO 100 BY 1; /* Adjust the range and increment as needed */
        Doctor_Availability = avg_Doctor_Availability;
        Capital_income = avg_Capital_income;

        OUTPUT; /* Output for each value */
    END;
RUN;

/* Fitting the model and get predictions with intervals */
PROC REG DATA=transformed OUTEST=estimates;
    MODEL square_Death_Rate = Doctor_Availability Capital_income Population_Density;
    OUTPUT OUT=predictions 
           P=Predicted 
           LCLM=Lower_CI 
           UCLM=Upper_CI 
           LCL=Lower_PI 
           UCL=Upper_PI; 
RUN;

/*Printing the confidence and prediction intervals */
PROC PRINT DATA=predictions;
    VAR Population_Density Predicted Lower_CI Upper_CI Lower_PI Upper_PI;
    TITLE "Confidence and Prediction Intervals for Predicted Values";
RUN;

/* Fitting the model to calculate simultaneous confidence bands */
PROC REG DATA=transformed OUTEST=estimates;
    MODEL square_Death_Rate = Doctor_Availability Capital_income Population_Density / CLB;
    OUTPUT OUT=simultaneous_bands
           P=Predicted
           LCLM=CB_Lower
           UCLM=CB_Upper; /* Simultaneous Confidence Bands */
RUN;

/* Sort datasets before merging */
PROC SORT DATA=predictions; 
    BY Population_Density; 
RUN;

PROC SORT DATA=simultaneous_bands; 
    BY Population_Density; 
RUN;

/* Merge predictions with simultaneous bands */
DATA plot_data;
    MERGE predictions simultaneous_bands;
    BY Population_Density; /* Ensure datasets are sorted by Population_Density */
RUN;

/* Create the plot */
PROC SGPLOT DATA=plot_data;
    BAND x=Population_Density lower=Lower_CI upper=Upper_CI / fillattrs=(color=lightyellow transparency=0.5) legendlabel="95% Confidence Interval";
    BAND x=Population_Density lower=Lower_PI upper=Upper_PI / fillattrs=(color=lightpink transparency=0.5) legendlabel="95% Prediction Interval";
    BAND x=Population_Density lower=CB_Lower upper=CB_Upper / fillattrs=(color=lightgreen transparency=0.5) legendlabel="95% Simultaneous Confidence Bands";
    SCATTER x=Population_Density y=Predicted / markerattrs=(symbol=circlefilled color=black);
    XAXIS LABEL="Population Density";
    YAXIS LABEL="Predicted Square Root of Death Rate";
    TITLE "Predicted Death Rate with 95% Confidence and Prediction Intervals";
    KEYLEGEND / position=topright;
RUN;







