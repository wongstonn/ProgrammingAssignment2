#### RETAIL #####
EAD=200
pd = 0.01
LGD=0.7


p = 0.03 + 0.13*exp(-35*pd)  #retail loan formula
p

WCDR = pnorm (     (qnorm(pd) + sqrt(p)*qnorm(0.999))  /  sqrt(1-p)   )
WCDR

CapReq = EAD * LGD * (WCDR - pd)   # retail formula
CapReq

RWA = CapReq * 12.5
RWA

#### corporate #####
EAD=200
pd = 0.02
LGD=0.7
M = 1

p = 0.12*(1 + exp(-50*pd))  #corporate loan formula
p

WCDR = pnorm (     (qnorm(pd) + sqrt(p)*qnorm(0.999))  /  sqrt(1-p)   )
WCDR

b = (0.11852 - 0.05478*log(pd))^2

MA = (1 + ((M - 2.5)*b)    )/     (1- (1.5*b) )
MA 

CapReq = EAD * LGD * (WCDR - pd) * MA  # corporate  formula
CapReq

RWA = CapReq * 12.5
RWA


############## CDS ####################
R=0.6
principle = 1000000
payment = 4*5000
term=2
CDSspread2 = payment/principle
print( CDSspread2 )

PD2 = CDSspread2 / (1 - R)
print (PD2)

CDSspread5 = 0.025
PD5 = CDSspread5 / (1 - R)
print (PD5)

AvPD = ((5 * PD5) - (2 * PD2))/(5-2)
print (AvPD)
