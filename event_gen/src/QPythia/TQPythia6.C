#include <TQPythia6.h>
#include <TClonesArray.h>
#include <TMCParticle.h>

extern "C" {
  void  pystat_(int*);
  void  pylist_(int*);
  void  pyinit_(char*,char*,char*,double*,long,long,long);
  void  pyevnt_();
  void qpygin0_();
  void* pythia6_common_address(const char*);

  void dophqpysetup_(int* addr1, double* addr2, double* addr3){
    TQPythia6:: _mrpy_p = addr1;
    TQPythia6::_toqpygin_p = addr2;
    TQPythia6::_qpc1_p = addr3;
  }

  void calculatestuff_(double* qhat, double* pl){
    *qhat = TQPythia6::_qhat;
    *pl = TQPythia6::_pathLength;
  }
}

int* TQPythia6:: _mrpy_p = 0;
double* TQPythia6::_toqpygin_p = 0;
double* TQPythia6::_qpc1_p = 0;
double TQPythia6::_qhat = 5.0;
double TQPythia6::_pathLength = 3.0;

TQPythia6::TQPythia6() : _seed(0) {

  fParticles = new TClonesArray("TMCParticle",50);

  fPysubs   = (Pysubs_t*)   pythia6_common_address("PYSUBS");
  fPypars   = (Pypars_t*)   pythia6_common_address("PYPARS");
  fPyjets   = (Pyjets_t*)   pythia6_common_address("PYJETS");

}

TQPythia6::~TQPythia6() {
  fParticles->Delete();
  delete fParticles;
}

void TQPythia6::Initialize(const char *frame, const char *beam, const char *target, float win) {
  char  cframe[4];
  strncpy(cframe,frame,4);
  char  cbeam[8];
  strncpy(cbeam,beam,8);
  char  ctarget[8];
  strncpy(ctarget,target,8);

  if ( (!strncmp(frame, "CMS"  ,3)) &&
       (!strncmp(frame, "FIXT" ,4)) &&
       (!strncmp(frame, "USER" ,4)) &&
       (!strncmp(frame, "FOUR" ,4)) &&
       (!strncmp(frame, "FIVE" ,4)) &&
       (!strncmp(frame, "NONE" ,4)) ) {
    printf("WARNING! In TQPythia6:Initialize():\n");
    printf(" specified frame=%s is neither of CMS,FIXT,USER,FOUR,FIVE,NONE\n",frame);
    printf(" resetting to \"CMS\" .");
    sprintf(cframe,"CMS");
  }

  if ( (!strncmp(beam, "e"       ,1)) &&
       (!strncmp(beam, "nu_e"    ,4)) &&
       (!strncmp(beam, "mu"      ,2)) &&
       (!strncmp(beam, "nu_mu"   ,5)) &&
       (!strncmp(beam, "tau"     ,3)) &&
       (!strncmp(beam, "nu_tau"  ,6)) &&
       (!strncmp(beam, "gamma"   ,5)) &&
       (!strncmp(beam, "pi"      ,2)) &&
       (!strncmp(beam, "n"       ,1)) &&
       (!strncmp(beam, "p"       ,1)) &&
       (!strncmp(beam, "Lambda"  ,6)) &&
       (!strncmp(beam, "Sigma"   ,5)) &&
       (!strncmp(beam, "Xi"      ,2)) &&
       (!strncmp(beam, "Omega"   ,5)) &&
       (!strncmp(beam, "pomeron" ,7)) &&
       (!strncmp(beam, "reggeon" ,7)) ) {
    printf("WARNING! In TPythia6:Initialize():\n");
    printf(" specified beam=%s is unrecognized .\n",beam);
    printf(" resetting to \"p+\" .");
    sprintf(cbeam,"p+");
  }

  if ( (!strncmp(target, "e"       ,1)) &&
       (!strncmp(target, "nu_e"    ,4)) &&
       (!strncmp(target, "mu"      ,2)) &&
       (!strncmp(target, "nu_mu"   ,5)) &&
       (!strncmp(target, "tau"     ,3)) &&
       (!strncmp(target, "nu_tau"  ,6)) &&
       (!strncmp(target, "gamma"   ,5)) &&
       (!strncmp(target, "pi"      ,2)) &&
       (!strncmp(target, "n"       ,1)) &&
       (!strncmp(target, "p"       ,1)) &&
       (!strncmp(target, "Lambda"  ,6)) &&
       (!strncmp(target, "Sigma"   ,5)) &&
       (!strncmp(target, "Xi"      ,2)) &&
       (!strncmp(target, "Omega"   ,5)) &&
       (!strncmp(target, "pomeron" ,7)) &&
       (!strncmp(target, "reggeon" ,7)) ){
    printf("WARNING! In TPythia6:Initialize():\n");
    printf(" specified target=%s is unrecognized.\n",target);
    printf(" resetting to \"p+\" .");
    sprintf(ctarget,"p+");
  }

  _mrpy_p[1] = 0;
  _mrpy_p[0] = _seed;

  Pyinit(cframe, cbeam ,ctarget, win);
}

void TQPythia6::Pyinit(char* frame, char* beam, char* target, double wint){
  long  s1    = strlen(frame);
  long  s2    = strlen(beam);
  long  s3    = strlen(target);
  pyinit_(frame,beam,target,&wint,s1,s2,s3);
}

void TQPythia6::Pystat(int flag){
  pystat_(&flag);
}

void TQPythia6::Pylist(int flag){
  pylist_(&flag);
}

TObjArray* TQPythia6::ImportParticles(Option_t *option) {
  fParticles->Clear();
  Int_t numpart   = fPyjets->N;
  TClonesArray &a = *((TClonesArray*)fParticles);
  for (Int_t i = 0; i<numpart; i++) {
    new(a[i]) TMCParticle(fPyjets->K[0][i] ,
			  fPyjets->K[1][i] ,
			  fPyjets->K[2][i] ,
			  fPyjets->K[3][i] ,
			  fPyjets->K[4][i] ,
			  fPyjets->P[0][i] ,
			  fPyjets->P[1][i] ,
			  fPyjets->P[2][i] ,
			  fPyjets->P[3][i] ,
			  fPyjets->P[4][i] ,
			  fPyjets->V[0][i] ,
			  fPyjets->V[1][i] ,
			  fPyjets->V[2][i] ,
			  fPyjets->V[3][i] ,
			  fPyjets->V[4][i]);
  }
  return fParticles;
}

void TQPythia6::GenerateHardScatteringOrigin(){
  qpygin0_();
//   _toqpygin_p[0] = 0.0;
//   _toqpygin_p[1] = 0.0;
//   _toqpygin_p[2] = 0.0;
//   _toqpygin_p[3] = 0.0;
//   _qpc1_p[1] = _qhat*_pathLength;
//   _qpc1_p[2] = 0.5*_qpc1_p[1]*_pathLength/0.1973;
}

void TQPythia6::GenerateEvent(){
  GenerateHardScatteringOrigin();
  pyevnt_();
}
