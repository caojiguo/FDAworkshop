/*statistics availability: 
     https://github.com/igargikaushik/FDAworkshop/new/master   */
libname sdtm "C:\review\sdtm";
proc contents data=sdtm._all_ out=out1 noprint;

proc print data=out1(keep=memname memlabel name label format informat length);
run;

proc print data=out1(keep=memname memlabel name label format informat length);
where format>' ' or informat>' ';
run;

proc print data=out1(keep=memname memlabel name label format informat length);
where anypunct(label)>0 and index(label,'/')=0 and index(label,'-')=0; 
run;

proc print data=out1(keep=memname memlabel name label format informat length);
where anypunct(memlabel)>0 and index(memlabel,'/')=0 and index(label, '-')=0; 
run;
