libname sdtm "C:\review2\sdtm";

ods output variables=variables; 
proc contents data=sdtm.dv;
run;

/*
DVSPID, DVTERM, DVDECOD, DVCAT, DVSCAT, DVSTDTC, DVENDTC and EPOCH.
*/

data fda;
length variable $8.;
variable='DVSPID'; output;
variable='DVTERM'; output;
variable='DVDECOD'; output;
variable='DVCAT'; output;
variable='DVSCAT'; output;
variable='DVSTDTC'; output;
variable='DVENDTC'; output;
variable='EPOCH'; output;
run;

proc sort data=variables; by variable;
proc sort data=fda; by variable;


data abc(keep=variable);
merge variables(in=a) fda(in=b);
by variable;
if b and not a;
run;

proc print data=abc;
title1 "List of FDA required variables not available in DV";
run;
