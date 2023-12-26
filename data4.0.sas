libname sdtm "C:\review\sdtm";

/*statistics avail@:
             https://github.com/igargikaushik/FDAworkshop/new/master */
data fda;
length tsparmcd $10; 
tsparmcd='ACTSUB'; output; 
tsparmcd='ADAPT'; output; 
tsparmcd='ADDON'; output; 
tsparmcd='AGEMAX'; output; 
tsparmcd='AGEMIN'; output; 
tsparmcd='COMPTRT'; output; 
tsparmcd='DCUTDESC'; output; 
tsparmcd='DCUTDTC'; output; 
tsparmcd='EXTTIND'; output; 
tsparmcd='FCNTRY'; output; 
tsparmcd='HLTSUBJI'; output; 
tsparmcd='LENGTH'; output; 
tsparmcd='NARMS'; output; 
tsparmcd='NCOHORT'; output; 
tsparmcd='OBJPRIM'; output; 
tsparmcd='OBJSEC'; output; 
tsparmcd='OUTMSPRI'; output; 
tsparmcd='PDPSTIND'; output; 
tsparmcd='PDSTIND'; output; 
tsparmcd='PIPIND'; output; 
tsparmcd='PLANSUB'; output; 
tsparmcd='RDIND'; output; 
tsparmcd='REGID'; output; 
tsparmcd='SENDTC'; output; 
tsparmcd='SEXPOP'; output; 
tsparmcd='SPONSOR'; output; 
tsparmcd='SDTMVER'; output; 
tsparmcd='SDTIGVER'; output; 
tsparmcd='STOPRULE'; output; 
tsparmcd='SSTDTC'; output; 
tsparmcd='STYPE'; output; 
tsparmcd='TBLIND'; output; 
tsparmcd='TCNTRL'; output; 
tsparmcd='THERAREA'; output; 
tsparmcd='TITLE'; output; 
tsparmcd='TPHASE'; output; 
tsparmcd='TTYPE'; output; 
run;

proc sql;
create table actual as 
select distinct tsparmcd from sdtm.ts; 

proc sort data=fda; by tsparmcd; 

data data4;
merge fda(in=a) actual(in=b);
by tsparmcd; 
if a and not b;

proc print data=data4;
title " TS parameters desired by FDA but not in the TS domain";
run;
