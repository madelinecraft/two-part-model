proc import datafile='c:\Users\mcraft\Desktop\Book1.xlsx' out=set1 dbms=excel REPLACE;
sheet='Sheet1';
getnames=yes;
run;
proc mixed data = set1 method = ML;
	model y = /s;
	title 'Unconditional Model';
run;
*int=100.11 (0.1834), e=16.8087;
proc nlmixed data = set1;
  xb = b0;
  model y ~ normal(xb,s2e);
run;
*int=100.11 (0.1834), e=16.8087;
proc nlmixed data = set1;
  pi = arcos(-1);
  llik = -log(sqrt(s2e))-((1/2)*log(2*pi))-((1/(2*s2e))*(y-mu)**2);
  model y ~ general(llik);
run;
*useful for two part models where each part of the model follows a different distribution 
*data with a lot of zeros follow a logit distribution whereas the continuous bits of data 
*might follow a normal distribution (one is discrete, one is continuous)
*write the llik for a logit (LL1) and the llik for a normal (LL2) and then specify 
*loglik = LL1+LL2 and say y ~ general(loglik)
*also useful for mixture modeling 
