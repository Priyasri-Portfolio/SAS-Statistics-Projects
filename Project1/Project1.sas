/* The Questions are on the report */
/* Loading the Data */
Proc import datafile="/senic.csv"
out=senic
DBMS=CSV
replace;
getnames=yes;
run;

/*1)a)Test whether or not the mean infection risk (variable 4) is the same in the four geographic regions
(variable 9); use alpha=0.05. Assume that ANOVA model is applicable. State the alternatives,
conclusion.
b)Obtain confidence intervals for all pairwise comparisons between four regions,
c)tukey procedure */

/*tukey 90% confidence  */
PROC GLM DATA=senic;
    CLASS region;
    MODEL infprob = region;  /* Infection risk as response variable;*/
    lsmeans region / adjust=TUKEY ALPHA=0.10 cl;
    lsmeans region / adjust=bon ALPHA=0.10 cl;
    lsmeans region / adjust=scheffe ALPHA=0.10 cl;
RUN;
QUIT;
/* ------------------------------------- */
/* Problem 2
The effect of average age of patient (variable 3) on mean infection risk (variable 4)
ANOVA, alpha=0.10 */
/*Creating Age Group Variable */
DATA senic_age;
    SET senic;
    IF age < 50 THEN age_group = "Under 50";
    ELSE IF 50 <= age < 55 THEN age_group = "50-54.9";
    ELSE IF 55 <= age < 60 THEN age_group = "55.0-59.9";
    ELSE IF age >= 60 THEN age_group = "60 and Over";
RUN;

/* ANOVA-table */
PROC GLM DATA=senic_age;
    CLASS age_group;
    MODEL infprob = age_group;
    lsmeans age_group /adjust=TUKEY ALPHA=0.10; /* Tukey’s test for multiple comparisons */
    TITLE "ANOVA: Effect of Age on Mean Infection Risk";
RUN;
QUIT;

/* Problem 3)*/
PROC GLM DATA=senic;
    CLASS region;
    MODEL stay = region;  
    OUTPUT OUT=residuals_data R=residuals;  /* Save]ing residuals */
    TITLE "ANOVA for Length of Stay Across Regions";
RUN;
QUIT;
PROC SGPLOT DATA=residuals_data;
    SCATTER X=region Y=residuals / JITTER;
    XAXIS LABEL="Geographic Region";
    YAXIS LABEL="Residuals";
    TITLE "Residual Dot Plot by Region";
RUN;

/* 3)Conduct a test of whether or not mean length of stay ANOVA. Conduct a test of whether or not mean length of stay (variable 2) is the same in the four geographic
regions.*/
/* ANOVA Model */
proc glm data=senic plots=all;
    class region; /* Region is a categorical variable */
    model stay = region; /* Length of stay as response variable */
/*     means region / stderr; */
    lsmeans region;
run;
/* plotting the residual */

/* Create a residual plot by region */
proc sgplot data=residuals_data;
    scatter x=region y=residuals / markerattrs=(symbol=circlefilled color=blue);
    xaxis label="Region";
    yaxis label="Residuals";
    title "Residual Plot by Region";
run;

/* -------------------------- */
/* 3)b)Brown-Forsythe Test for Equality of Variances */

proc glm data=senic;
    class region;
    model stay = region;  /* Length of stay as the response variable*/
    means region / hovtest=bf;  /* Brown-Forsythe Test for Equality of Variances*/
    output out=residuals_data p=fitted_values r=residuals;  /* Saving fitted values and residuals*/
    title2 "Brown-Forsythe Test for Equal Variance Across Regions";
run;
/* 3)b)Mean and Standard deviation */
PROC GLM DATA=senic;
    CLASS region;
    MODEL stay = region;
    MEANS region / HOVTEST=LEVENE;
    TITLE "Levene's Test for Homogeneity of Variance";
RUN;

/* ------------------- */
/* 3)c)Calculating mean and standard deviation for each region */
/* ----------------------------- */
proc means data=senic mean std;
    class region; /* Region is a categorical variable */
    var stay; /* Length of stay as the variable of interest */
   output out=stats mean=mean_stay std=std_stay; */
    title "Means and Standard Deviations for Length of Stay by Region";
run;


/* ---------------- */
/*Calculating the additional metrics */
data stats_calculated;
    set stats;
    s1 = (std_stay**2) / mean_stay;
    s2 = std_stay / mean_stay;
    s3 = std_stay / (mean_stay**2);
run;

/* Displaying the additional calculated statistics for each region */
proc print data=stats_calculated;
    var region s1 s2 s3;
    title "Calculated Metrics for Transformation Decision by Region";
run;
/* 3)d) Box-Cox transformation with specified range of lambda values */

ods graphics on;


proc transreg data=senic;
    model boxcox(stay / lambda=-2 to 2 by 0.01) = class(region);
    output out=boxcox_out; /* Corrected output statement*/
run;

/* Box-Cox transformation with specified range of lambda values with convenient */
proc transreg data=senic;
    model boxcox(stay / lambda=-2 to 2 by 0.01 convenient) = class(region);
    output out=boxcox_out;
run;

ods graphics off;
/* ------------------------ */

PROC TRANSREG DATA=senic;
   MODEL BOXCOX(stay / convenient) = CLASS(region);
   OUTPUT OUT=Box_res;
RUN;

/* Creating a macro- lambda values */
%MACRO lambda_BC(data=, response=, predictor=, lambda_start=-1, lambda_end=1, lambda_step=0.2);
   /* Creating a dataset with the original response*/
   DATA ogdata;
      SET &data;
      KEEP &response &predictor;
   RUN;

   /* Calculating the geometric mean (gm) of the response variable 'stay' */
   DATA _null_;
      SET ogdata END=last; 
      RETAIN s_log 0 count 0; 
      s_log = s_log + LOG(&response); /* Obtain geometric mean */
      count = count + 1; /* Counting the iterations */
      IF last THEN DO; /* If the current observation is the last observation, then get geometric mean */
         m_log = s_log / count; /* Calculating the mean */
         CALL SYMPUT('gm', EXP(m_log)); /* Geometric mean to create a macro */
      END;
   RUN;

   /* Calculate the number of steps so that macro knows exactly how many iterations to perform */
   %LET num_steps = %SYSFUNC(FLOOR(%SYSEVALF((&lambda_end - &lambda_start) / &lambda_step))) + 1;

   /* Process each lambda value */
   %DO i = 0 %TO %EVAL(&num_steps - 1); /* Number of iterations */
      %LET lambda = %SYSEVALF(&lambda_start + &i * &lambda_step); /* Calculate lambda for the i_th iteration and increment for the i+1_th iteration */
      
      /* Create transformed dataset for this lambda */
      DATA transformed_&i;
         SET ogdata;
         lambda = &lambda; /* Create new lambda and assign it to the current lambda value */
         gm = &gm; /* Calculate geometric mean */
         
         /* Transform the response variable based on lambda */
         %IF %SYSEVALF(&lambda = 0) %THEN %DO; /* If lambda=0, then scale using geometric mean */
            stay_BC_transformed = gm * LOG(&response);
         %END;
         %ELSE %DO; /* If lambda is not=0, then calculate scaling factor 'sf' for the boxcox transformation */
            SF = 1 / (&lambda * (gm**(&lambda - 1)));
            stay_BC_transformed = SF * ((&response**&lambda) - 1); /* Use boxcox transformed variable */
         %END;
      RUN;

      /* Fit the model with the transformed variable */
      PROC GLM DATA=transformed_&i NOPRINT;
         CLASS &predictor;
         MODEL stay_BC_transformed = &predictor;
         OUTPUT OUT=fit_&i PREDICTED=pred;
      RUN;

      /* Calculate SSE=(y_hat - y_i)^2 */
      DATA sse_&i;
         SET fit_&i;
         sse_i = (pred - stay_BC_transformed)**2;
      RUN;

      /* Sum all the SSE for output */
      DATA _null_;
         SET sse_&i END=last;
         RETAIN s_sse 0;
         s_sse = s_sse + sse_i;
         IF last THEN DO;
            CALL SYMPUT('l_value', lambda);
            CALL SYMPUT('sse_value', s_sse);
         END;
      RUN;

      /* Output results lambda and SSE */
      %PUT Lambda = &l_value, SSE = &sse_value;

      /* Store results in a dataset for display */
      DATA l_res_&i;
         lambda = &l_value;
         sse = &sse_value;
      RUN;
   %END;

   /* Combine all results of required lambdas */
   DATA all_res;
      SET 
      %DO i = 0 %TO %EVAL(&num_steps - 1);
         l_res_&i
      %END;
      ;
   RUN;

   /* Print the results */
   PROC PRINT DATA=all_res;
      TITLE "Box-Cox SSE Results for Different Lambda Values";
   RUN;

   /* Plot the results of the lambdas and their corresponding SSEs */
   PROC SGPLOT DATA=all_res;
      SERIES X=lambda Y=sse / MARKERS;
      XAXIS LABEL="Lambda";
      YAXIS LABEL="SSE";
      TITLE "Box-Cox Transformation: SSE vs Lambda";
   RUN;
%MEND lambda_BC;

/* Call the macro with our specific dataset values */
%lambda_BC(data=senic, response=stay, predictor=region, lambda_start=-1, lambda_end=1, lambda_step=0.2);


/*3(e)Use the reciprocal transformation Y 0 = 1=Y to obtain transformed response data. Fit ANOVA
model to the transformed data and obtain the residuals */

/* Reciprocal transformation of the response variable */
data transformed;
    set senic;
    stay_reciprocal = 1 / stay;
run;

/* Fit the ANOVA model to the transformed data */
proc glm data=transformed;
    class region;
    model stay_reciprocal = region;
    output out=residuals p=predicted r=residual;
run;

/* Print the residuals */
proc print data=residuals (obs=10);
    var region stay_reciprocal predicted residual;
run;

/*3)f) Examine by means of the Brown-Forsythe test test whether or not the geographic region variances for
the transformed response variable are equal. use alpha = :01.*/

/* Reciprocal transformation of the response variable */
data transformed;
    set senic;
    stay_reciprocal = 1 / stay;
run;

/* Perform the Brown-Forsythe test for equal variances */
proc glm data=transformed;
    class region;
    model stay_reciprocal = region;  /* Transformed length of stay as the response variable */
    means region / hovtest=bf;  /* Brown-Forsythe Test for Equality of Variances */
    output out=residuals_data p=fitted_values r=residuals;  /* Save fitted values and residuals */
    title2 "Brown-Forsythe Test for Equal Variance Across Regions";
run;
/* Print the residuals */
proc print data=residuals_data;
    var region stay_reciprocal fitted_values residuals;
run;
/* --------------------------------- */





