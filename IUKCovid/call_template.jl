using DataFrames, Distributions
import Pkg; Pkg.activate("IUKCovid")
import IUKCovid

numRuns = 5
mrres = DataFrame(
  runNum = sample(1:5, 2785),
  underlying = sample(1:33994, 2785)
);
thisAdmProps = rand(557);
directICUProp = 0
propGW2IC = 40; propIC2SD = 40
losGWT = "Exponential"; losGWM = 11.5; losGWS = 11.5
losICT = "Weibull";  losICM = 16.5; losICS = 14.5
losSDT = "Weibull";  losSDM = 22; losSDS = 17
numGW = 20; numICU = 10; startOffset = 526

IUKCovid.createInHRunWSD(
  numRuns, mrres, thisAdmProps, directICUProp,
  propGW2IC, propIC2SD,
  losGWT, losGWM, losGWS,
  losICT,  losICM, losICS,
  losSDT, losSDM, losSDS,
  numGW, numICU, startOffset
);
