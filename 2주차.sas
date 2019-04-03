DATA bodyweight;
	INFILE 'C:\대학원\2018-2\1. 전공\임상시험자료분석2\과제\bodyweight.CSV' DELIMITER=',' FIRSTOBS=2;
	INPUT weight Time Rat Diet;
RUN;

PROC BOXPLOT DATA=bodyweight;
	PLOT weight*Diet ;
RUN;

PROC BOXPLOT DATA=bodyweight;
	PLOT weight*Rat ;
RUN;

/*PROC SGPLOT DATA=bodyweight;
	SCATTER X = Time Y = weight / GROUP=Diet;
RUN;

PROC SGPLOT DATA=bodyweight;
	SCATTER X = Time Y = weight;
	SERIES X = Time Y = weight / GROUP=Diet;
RUN;
*/

PROC SGPANEL DATA=bodyweight;
	PANELBY Diet / COLUMNS=3;
	SERIES X = Time Y = weight / GROUP=Rat;
RUN;

PROC GLM DATA=bodyweight;
	CLASS Time Rat Diet;
	MODEL weight = Time Diet Rat(Diet) Time*Diet;
	RANDOM Rat(Diet);
	TEST H=Diet E=Rat(Diet);
	QUIT;
RUN;

PROC MIXED DATA = bodyweight;
	CLASS Time Rat Diet;
	MODEL weight = Time Diet Time*Diet;
	REPEATED Time / SUBJECT=Rat(Diet) TYPE=un;
RUN;


DATA vision;
	INFILE 'C:\대학원\2018-2\1. 전공\임상시험자료분석2\과제\vision.txt';
	INPUT acuity power $ eye $ subject;
RUN;

PROC SGPLOT DATA=vision;
	VBOX acuity / CATEGORY=power GROUP=eye;
RUN;

PROC SGPANEL DATA=vision;
	PANELBY subject / COLUMNS=3;
	SCATTER X = power Y = acuity;
	SERIES X = power Y = acuity / GROUP=eye;
RUN;

PROC GLM DATA=vision;
	CLASS power eye subject;
	MODEL acuity = eye subject(eye) power eye*power;
	RANDOM subject(eye);
	TEST H=eye E=subject(eye);
	QUIT;
RUN;

PROC MIXED DATA = vision;
	CLASS power eye subject;
	MODEL acuity = eye power eye*power;
	REPEATED power / SUBJECT=subject(eye) TYPE=un r rcorr;
RUN;
