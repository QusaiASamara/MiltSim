$PROB Verrest_pk_model
$SET 
end = 1368
delta = 12

$PARAM @annotated
THETA1        : 0.0745   : CL
THETA2        : 13.2     : V
THETA3        : 0.0371   : KA
THETA4        : 0.00679   : Q
THETA5        : 2.15     : V3
THETA6        : 1           : F1
THETA7        : 0.677    : COVF1_WEEK1
THETA8        : -3.14   : COVF2_DDOS
THETA9        : 1.84        : h0
THETA10       : 167.28      : I50

$PARAM @covariates
FFM = 40;
AGE = 70;
WT = 70;
HT = 70;
SEX = 70;
FLAG = 1;
TSWITCH = 0 // Time after which to start calculating EOC AUC (e.g., day 58)



$OMEGA @annotated
ETA1          : 0.0267  : CL
ETA2          : 0.584   : COVF1 F1

$SIGMA @annotated
EPS1          : 0.0982   : proportional_PK // Wendy


$CMT
DEPOT
CENT
PERIPH
AUC  
TEC90
Cmax_track
Tmax_track
AUCEOC

$GLOBAL
static double cumamt = 0;
static double last_id = -1;


$MAIN 
double EC90 = 10.6; 

if(ID != last_id) {
  cumamt = 0;
  last_id = ID;
}

if(EVID == 1) {
  cumamt += AMT;
}

double DDOS_calc = cumamt/WT;


double ALLOCL = pow(FFM/18, 0.75);
double ALLOV  = pow(FFM/18, 1);




if(TIME <= 168) {
  double COVF1 = (1 - THETA7)*exp(ETA2);
} else {
  COVF1 = 1;
}

if(DDOS_calc >= 70) {
  double COVF2 = pow(DDOS_calc/70, THETA8);
} else {
  COVF2 = 1;
}


double CL     = THETA1 * ALLOCL * exp(ETA1);
double V      = THETA2 * ALLOV;
double KA     = THETA3;
double Q      = THETA4;
double V3     = THETA5 * ALLOV;
double CHECK  = THETA6 * COVF1 * COVF2;
double h0     = THETA9;
double I50    = THETA10;

F_DEPOT= CHECK;
if(CHECK > 1 && TIME <= 168) {
  F_DEPOT = 1;
}

double KE     = CL/V;
double K23    = Q/V;
double K32    = Q/V3;

CENT_0 = 0;
Cmax_track_0= 0; 
Tmax_track_0 = 0; 

$ODE
double CONC_CENT = CENT/V;
double CONC_PER  = PERIPH/V3;



dxdt_DEPOT        = -KA*DEPOT;
dxdt_CENT         = KA*DEPOT - KE*CENT - K23*CENT + K32*PERIPH;
dxdt_PERIPH       = K23*CENT - K32*PERIPH;
dxdt_AUC          = CONC_CENT;

if(SOLVERTIME >= TSWITCH) {
  dxdt_AUCEOC= CONC_CENT;
} else {
  dxdt_AUCEOC = 0;
}

dxdt_Cmax_track = 0;
dxdt_Tmax_track = 0;
if(CONC_CENT > Cmax_track){
  dxdt_Cmax_track = CONC_CENT - Cmax_track;
  dxdt_Tmax_track = SOLVERTIME - Tmax_track; // Update Tmax
} 


double RT1 = 0;
if(CONC_CENT > EC90 && SOLVERTIME > 0) RT1 = 1; //https://github.com/metrumresearchgroup/mrgsolve/issues/375

dxdt_TEC90 = RT1;
double TOEC90 = TEC90;

$TABLE 
double IPRED = CENT/V;
double Y = IPRED * (1+EPS(1)); //PK
double IRES = CONC_CENT - IPRED;
double IWRES = IRES/IPRED;
double Cmax = Cmax_track;
double Tmax = Tmax_track;

double AUC_accum = AUCEOC; // The accumulated AUC since TSWITCH
double AUC_inf = 0;  // Initialize AUC to infinity

// Only calculate AUC to infinity if we're past TSWITCH
if(TIME >= TSWITCH) {
  AUC_inf = AUC_accum + (CONC_CENT/KE);
} else {
  AUC_inf = 0; // Before TSWITCH, AUC is zero
}


double MIL = TEC90;  
double h = h0 * (1- (MIL/(I50+MIL)));

$CAPTURE
CONC_CENT AMT AUC_inf Cmax h Tmax FFM DDOS_calc COVF1 COVF2 F_DEPOT AGE WT HT SEX FLAG CL KE V 
