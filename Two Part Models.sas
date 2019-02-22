proc import datafile='c:\Users\mcraft\Desktop\twopart.xlsx' out=twopart dbms=excel REPLACE;
sheet='twopart';
getnames=yes;
run;
data twopart;
	set twopart(keep = M2ID physicalT P_u P_m);
run;
***Two-part mixed model;
proc nlmixed maxiter=10000 gconv=0 absconv=0 data=twopart qpoints=5;
title1 "two part mixed model normal continuous part no covar";
parms a0 = 2.8977 b10 = 2.1442 vara = 4.6345
covab =	2.2328 varb = 1.6559 vare = 1.4244;
pi = arcos(-1); 
u = P_u; 
m = P_m;

*LL1: Binary part;
*eta is the logit transformation of u;
ueta = a0 + au0; *add time varying or invariant predictors here;
expeta = exp(ueta); 
p = expeta/(1+expeta);
LL1 = log((1-p)**(1-u)) + log(p**(u)); *loglikelihood for a binary dist;

*LL2: Continuous part;
if physicalT > 0 then do;
mu = b10 + bu0; *add time varying or invariant predictors here;
LL2 = -.5*(((m-mu)**2)/vare + log(vare) + log(2*pi)); *loglikelihood for a normal dist;
end;

if physicalT = 0 then Loglik=LL1; 
else if physicalT > 0 then Loglik=LL1+LL2;

random au0 bu0 ~ normal([0,0],[vara,covab,varb]) subject=M2ID out=ForPlot;
model physicalT ~ general(Loglik);
bounds vara >= 0, varb >= 0, vare >= 0;
estimate 'mean probability' exp(a0)/(1+exp(a0));
run;
*How to get the empirical bayes estimates:
out = ForPlot on the random statement after subject = M2ID;
*Use the estimate statement to put it in terms of probabilities not logits;
data aForPlot;
	set ForPlot;
	where Effect = 'au0';
	MixedEffect = .;
	MixedEffect = -0.3927 + Estimate;
	pMixedEffect = exp(MixedEffect)/(1+exp(MixedEffect)); *turn logits into probabilities;
run;
data bForPlot;
	set ForPlot;
	where Effect = 'bu0';
	MixedEffect = .;
	MixedEffect = -0.1385 + Estimate;
	pMixedEffect = exp(MixedEffect)/(1+exp(MixedEffect)); *turn logits into probabilities;
run;
*plot the mixed effects;
proc sgplot data=aForPlot;
	title "au0";
	histogram MixedEffect / binwidth=.1 binstart=-2.3406 showbins;
	density MixedEffect;
	keylegend / location=inside position=topright;
run;
proc univariate data=aForPlot;
	var MixedEffect;
	histogram MixedEffect / vscale=count barlabel=count
	endpoints=-2.3406 to 2.0836 by .1;
run;
proc sort data = aForPlot;
	by MixedEffect;
run;
*look more carefully at the people with super low probabilities/logits;
data test;
	set TwoPart;
	where M2ID = 10174 or M2ID = 10211 or M2ID = 10280 or M2ID = 10414 or M2ID = 10416 or M2ID = 10443;
run;

/*NOTES
*mixture model for the continuous part;
LL1_1 = log(1-p)^(1-u) + log(p)^(u)
LL1_2 = log(1-p)^(1-u) + log(p)^(u)

ueta = a0 + au0; 
	*where a0 is the fixed effect and au0 is the random effect;
	*we estimate the fixed effect and we estimate the variance of the random effect;
expueta = exp(ueta);
p = expueta/(1+expueta);

ueta1 = a01 + au01; 
expueta1 = exp(ueta1);
p1 = expueta1/(1+expueta1);

ueta2 = a02 + au02; 
expueta2 = exp(ueta2);
p2 = expueta2/(1+expueta2);
*/

***Mixture model;
*NOT finished;
proc nlmixed maxiter=10000 gconv=0 absconv=0 data=twopart qpoints=5;
title1 "two part mixed model with mixed model for continuous part";
parms 
b101 = .5 
b102 = 2 
sdb1 = .5 
sdb2 = 1.7208 
s2e = 2.5272;

prop1 = .5; 
prop2 = 1-prop1;
pi = arcos(-1); 
m = P_m;

*LL_1: Continuous part 1;
if physicalT > 0 then do;
mu1 = b101 + bu01; *add time varying or invariant predictors here;
LL1_1 = -.5*(((m-mu1)**2)/s2e + log(s2e) + log(2*pi)); *loglikelihood for a normal dist;

*LL_2: Continuous part 2;
mu2 = b102 + bu02;
LL1_2 = -.5*(((m-mu1)**2)/s2e + log(s2e) + log(2*pi)); *loglikelihood for a normal dist;
end;

newlik = prop1*LL1_1+prop2*LL1_2;

model physicalT ~ general(newlik);
random bu01 bu02 ~ normal([0,0],[sdb1,0,sdb2]) subject = M2ID;
*bounds sdb1 >= 0, sdb2 >=0, s2e >= 0;
*bounds prop1 <1;
*bounds prop2 >0;
run;
