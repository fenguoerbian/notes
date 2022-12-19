************************************************************************************************;
*** Macro:      ratediff_strat_mn;
*** Purpose:    Estimation of Miettinen and Nurminen method stratified by factors using CMH weights
*** Author:     Bruce Hu
*** Date:       16June2022
*** Validator:  Yuansheng Huang
*** References: 1. Constructing Confidence Intervals for the Differences of Binomial
				Proportions in SAS?
                2. Cochran-Mantel-Haenszel Weighted Miettinen & Nurminen Method
				for Confidence Intervals of the Difference in Binomial Proportions
				from Stratified 2x2 Samples
************************************************************************************************;

*******************************************************************;
** Note: This is a dummy data for test purpose                     ;
** Trial is the stratification factor                              ;
** The comparison is x1/N1 vs x0/N0                                ;
** x0 is number of subjects with response in control group         ; 
** x1 is number of subjects with response in treatment group       ; 
** n0 is number of subjects in control group                       ; 
** n1 is number of response in treatment group                     ; 
*******************************************************************;


data testdata;
 input trial x0 n0 x1 n1  ;
 datalines;
 1  1 15  3 5
 2  3 10 4 10  
 3  2 25 18 35  
;

%Macro  ratediff_strat_mn(indata=testdata, outdata=data,  strata=trial);
proc sort data=&indata;
	by &strata;
run;

data data;
	set &indata;
	by &strata;
	id =1;
		*** Initial set up;
      n11=n1;
      n12=n0;
 	  n = n11+n12;
      r11 = x1; r12 = x0;
      nr11=n11-r11;
      nr12=n12-r12;
      r1=r11+r12;
      nr1=nr11+nr12;
      p11 = r11/n11;
      p12 = r12/n12;
	  pdiff=p11-p12;
	  pi=constant("pi");
	  strata=&strata;
     *	Miettinen and Nurminen provide a closed-form solution for the maximum likelihood given by the following; 
		*   You can check the formula in the page 2 of paper <Constructing Confidence Intervals for the Differences of Binomial   ;
		*		Proportions in SAS>   ;
      delta =0;
      L3 = n;
      L2 = (n1 + 2*n12) * delta - n - r1;
      L1 = (n12 * delta - n - 2*r12) * delta + r1;
      L0 = r12 * delta * (1 - delta);
      q = L2**3/(27*L3**3) - (L1*L2)/(6*L3**2) + L0/(2*L3);  * c;
      p = SIGN(q) * SQRT(L2**2/(9*L3**2) - L1/(3*L3));  *B;
      A = 1/3 * (pi + arCOS (q/p**3));
      p12hat = 2*p*COS(A) - L2/(3*L3);
      p11hat = p12hat + delta;
      q12hat = 1 - p12hat;
      q11hat = 1 - p11hat;
      varMN1 = ((p11hat*q11hat)/n11+(p12hat*q12hat)/n12)*(n/(n-1));
	  keep id strata n11  n12 r11 r12 p12hat p11hat  varMN1  p11 p12 pdiff trial;
run;

data data1;
	set data;
	by id trial;
	 ** w0 is the CMH weights for MN mothod;
	w0=n11*n12/(n11+n12);  
	w1+w0;
	call symputx("_total",w1);   
run;

**** Derivation of CMH weighted difference, Z score, variance and P value; 
data estimation;
	set data1;
	by id trial;
  ** this is standardized CMH weights for MN method; 
	w_mn=w0/&_total; 
	pdff1=w_mn*pdiff;

	pdff2+pdff1;
	if first.id then pdff2=pdff1;

	var=varMN1*(w_mn**2);
	var1+var;
	z=pdff2/sqrt(var1);
	 p_final = 2*(1-probnorm(abs(z)));
	if last.id;
	order=1;
	keep order pdff2 var1 p_final z;
run;

*** Derivation of CMH weighted MN confidence interval; 
data ci1;
	set &indata;
	do delta = -0.99999 to 0.99999 by 0.00001; 
      n11=n1;
      n12=n0;
 	  n = n11+n12;
      r11 = x1; r12 = x0;
      nr11=n11-r11;
      nr12=n12-r12;
      r1=r11+r12;
      nr1=nr11+nr12;
      p11 = r11/n11;
      p12 = r12/n12;
	  pdiff=p11-p12;
	  pi=constant("pi");
      L3 = n;
      L2 = (n1 + 2*n12) * delta - n - r1;
      L1 = (n12 * delta - n - 2*r12) * delta + r1;
      L0 = r12 * delta * (1 - delta);
      q = L2**3/(27*L3**3) - (L1*L2)/(6*L3**2) + L0/(2*L3);  * c;
      p = SIGN(q) * SQRT(L2**2/(9*L3**2) - L1/(3*L3));  *B;
      A = 1/3 * (pi + arCOS (q/p**3));
      p12hat = 2*p*COS(A) - L2/(3*L3);
      p11hat = p12hat + delta;
      q12hat = 1 - p12hat;
      q11hat = 1 - p11hat;
      varMN1 = ((p11hat*q11hat)/n11+(p12hat*q12hat)/n12)*(n/(n-1));
	output;
	end;
run;

proc sort data=ci1;
	by delta trial;
run;

data ci2;
	set ci1;
	by delta trial;
	w0=n11*n12/(n11+n12);
	sum+w0;
	if first.delta then sum=w0;
run;

data total;
	set ci2;
	by delta trial;
	if last.delta;
	keep delta sum;
run;

data ci3;
	merge ci2(drop=sum) total;
	by delta;
	w_mn=w0/sum;
	r1_w=p11*w_mn;
	r2_w=p12*w_mn;
	var_w=varMN1*w_mn**2;
run;

proc sql;
	create table ci4 as select  delta,  sum(r1_w) - sum(r2_w) as est, sum(var_w) as var_sum from ci3 
	group by delta; 
quit;

data ci5;
	set ci4;
	alpha=0.05;
	z = probit(1-alpha/2);
	** Get Z score for each delta;
	 z_score = (est - delta)/sqrt(var_sum );
	 if -z < z_score < z;
run;

proc sql;
	create table ci as select 1 as order,  min(delta) as lowci, max(delta) as highci from ci5 ;
quit;

data &outdata;
	retain  pdff2 z var1 lowci highci p_final;
	merge estimation ci;
	by order;
	rename pdff2=diff p_final=pval z=zscore;
	drop order;
run;

%mend ratediff_strat_mn;

%ratediff_strat_mn(indata=testdata, outdata=mn,  strata=trial);

