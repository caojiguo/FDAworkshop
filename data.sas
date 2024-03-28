options sasautos=("C:\review\checklist");
libname sdtm "C:\review\sdtm";
%let fdf_file=C:\review\sdtm\acrf.fdf;


data afdf;
infile "&fdf_file" print lrecl=2000 pad missover;
input text $char2000.;
run;
data match(keep=anno page_num); set afdf;
pos1=prxmatch("/\/Contents\(/", text);
pos2=prxmatch("/\)\/Creation/", text);
anno=substrn(text, pos1+10, pos2-pos1-10);
pos3=prxmatch("/\/Page /", text);
pos4=prxmatch("/\/RC/", text); 
page_num=substrn(text, pos3+1, pos4-pos3-1); 
if anno>' ';
run;
data match; set match; 
retain pat1 pat2;
if _N_=1 then do; 
pat1=prxparse("/\w+\.\w+/");
pat2=prxparse("/\w+\.QNAM=\w+/");
end;
call prxsubstr(pat2, anno, start, length);

if start gt 0 then do;
match0 = substrn(anno,start,length ); 
end;
start=1; 
stop=length(anno); 
call prxnext(pat1, start, stop, anno, position, length);
array match[3] $20.;
do i = 1 to 3 while (position gt 0);
match[i] = substr(anno,position,length);
call prxnext(pat1,start,stop,anno,position,length);
end;
run;

proc sort data=match out=chk(keep=match0) nodupkey; by match0;
where match0>' ';
run;

data chk; set chk;
mydomain=scan(match0, 1, '.');
myvar=scan(match0,2,'='); 
run;

%list_files(path=%str(%sysfunc(pathname(sdtm))));
run;

data f1; set f1; 
name=scan(names,1,'.');
if upcase(substr(name,1,4))='SUPP';
run;

data _null_;
set f1 end=eof;
i+1;
call symput(compress('name'||i), trim(left(name)));
if eof then call symput('tot', trim(left(_n_)));
run;

%macro getit; 
%do k=1 %to &tot;
proc sql;
create table tt&k as 
select distinct qnam as myvar, qlabel as mylabel from sdtm.&&name&k;

data tt&k; set tt&k;
mydomain=upcase("&&name&k"); 
run;

data final; 
set %if &k=1 %then %do; tt1 %end; %else %do; final tt&k %end;;
run;
%end;
%mend;

%getit;


** do the comparison; 

proc sort data=final; by mydomain myvar;
proc sort data=chk; by mydomain myvar;

data comp2(drop=mylabel);
merge final(in=a) chk(in=b);
by mydomain myvar;
if b and not a;
run;

proc print data=comp2;
title "QNAM in the aCRF but not in data";
run;

