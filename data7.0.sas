
/*
Purpose: this macro combine an SDTM domain with SUPP QUAL
inlib is the input data library
insupp is the supp qual domain i.e. suppae
output is a data in the WORK library, i.e. ae
*/
%macro supp(inlib=, insupp=);
%if %sysfunc(exist(&inlib..&insupp)) %then %do;

%let domain=%sysfunc(substr(&insupp,5));

proc sql noprint;
select count(*) into :suppobs from &inlib..&insupp;

%if &suppobs=0 %then %do;
data &domain; 
set &inlib..&domain; 
run;

data _null_;
put "WARNING: There is no record in &insupp!";
run;
%end;

%else %do;

%if &insupp=suppdm %then %do;
proc sql;
create table t1 as 
select distinct qnam, qlabel from &inlib..&insupp;

data _null_; 
set t1 end=eof;
i+1;
call symput(compress("qnam"||put(i,best.)), trim(left(qnam)));
call symput(compress("qlabel"||put(i,best.)), trim(left(qlabel)));
if eof then call symput("tot", trim(left(put(_n_,best.))));
run;

%do k=1 %to &tot;
data a&k(keep=usubjid &&qnam&k); 
set &inlib..&insupp;
if qnam="&&qnam&k";
&&qnam&k=qval;
label &&qnam&k="&&qlabel&k";

proc sort data=%if &k=1 %then &inlib..&domain; %else &domain; out=&domain; by usubjid;
proc sort data=a&k; by usubjid;

data &domain; 
merge &domain(in=a) a&k;
by usubjid;
if a;
run;

%end;
%end;

%else %do;
proc contents data=&inlib..&domain out=a noprint;

proc sql;
create table t1 as 
select distinct qnam, qlabel, idvar from &inlib..&insupp;

data _null_; 
set t1 end=eof;
i+1;
call symput(compress("qnam"||put(i,best.)), trim(left(qnam)));
call symput(compress("qlabel"||put(i,best.)), trim(left(qlabel)));
call symput(compress("idvar"||put(i,best.)), trim(left(idvar)));
if eof then call symput("tot", trim(left(put(_n_,best.))));
run;

%do k=1 %to &tot;
data _null_; set a;
where name="&&idvar&k";
call symput("varlen", trim(left(put(length,best.))));
call symput("vartype", trim(left(put(type,best.))));
run;


%if &vartype=1 %then %do;
data a&k(keep=usubjid &&qnam&k &&idvar&k); 
length &&idvar&k &varlen;
set &inlib..&insupp;
if qnam="&&qnam&k";
&&qnam&k=qval;
&&idvar&k=input(idvarval, best.);
label &&qnam&k="&&qlabel&k";
%end;

%else %do;
data a&k(keep=usubjid &&qnam&k &&idvar&k); 
length &&idvar&k $&varlen;
set &inlib..&insupp;
if qnam="&&qnam&k";
&&qnam&k=qval;
&&idvar&k=idvarval;
label &&qnam&k="&&qlabel&k";
%end;


proc sort data=%if &k=1 %then &inlib..&domain; %else &domain; out=&domain; by usubjid &&idvar&k;
proc sort data=a&k; by usubjid &&idvar&k;

data &domain; 
merge &domain(in=a) a&k;
by usubjid &&idvar&k;
if a;
run;

%end;
%end;
%end;
%end;

%else %do;
%put Error: The dataset &inlib..&insupp does not exsit.;
%end;
%mend;


/*Example;
options- mprint mlogic symbolgen;
libname- sdtm */
%*supp(inlib=sdtm, insupp=suppae);
