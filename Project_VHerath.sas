/*Importing data */ 
data MRI;
infile "/folders/myfolders/sasuser.v94/Longitudinal/MRI.dat" DELIMITER='09'x;
input ID Group $ time Gender $ Age EDUC SES MMSE CDR eTIV nWBV ASF;
run;

data MRI;
	set MRI;
	IF time=1 THEN time = 0;
	IF time=2 THEN time = 1;
	IF time=3 THEN time = 2;
	IF time=4 THEN time = 3;
	IF time=5 THEN time = 4;
proc print;
run;


proc means data= MRI maxdec = 2 n mean var nway;
	var MMSE ASF eTIV nWBV Age ;
	class CDR time;
	output out=outmean mean=mean;
 run;
 
proc plot data = outmean;
 	plot mean*time=CDR;
run;



 /* Model without Covariates */ 
 PROC LOGISTIC data=MRI DESCENDING;
 model CDR = ;

/*Multinomial Logistic Regression Model */ 
PROC LOGISTIC DATA=MRI; 
CLASS ID Gender (REF = "M") / PARAM= REF;
MODEL CDR=time Age Gender MMSE eTIV nWBV time*MMSE time*nWBV / LINK=CLOGIT SCALE=NONE AGGREGATE RSQ LACKFIT;
RUN; 


/* Marginal Model */
PROC genmod data=MRI;
	CLASS ID Gender;
	MODEL CDR=time Age Gender MMSE eTIV nWBV time*MMSE time*nWBV / dist=multinomial link=cumlogit type3 wald;
	repeated subject = id / type = ind;
run;

/* Random-intercept Model */
PROC GLIMMIX DATA=MRI METHOD=QUAD NOCLPRINT;
	CLASS ID Gender;
	MODEL CDR=time Age Gender MMSE eTIV  nWBV time*MMSE time*nWBV /SOLUTION DIST=MULTINOMIAL LINK=CUMLOGIT;
	RANDOM INTERCEPT /SUBJECT=ID;
RUN;



/* Random intercept & slope model */ 
PROC GLIMMIX DATA=MRI METHOD=QUAD NOCLPRINT;
	CLASS ID Gender;
	MODEL CDR=time Age Gender MMSE eTIV  nWBV time*MMSE time*nWBV /SOLUTION DIST=MULTINOMIAL LINK=CUMLOGIT;
	RANDOM INTERCEPT time /SUBJECT=ID;
RUN;



