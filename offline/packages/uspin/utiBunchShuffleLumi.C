/////////////////////////////////////////////////////////////////////
// Systematic error evaluation of the relative luminosity measurement
// with the bunch shuffling method ...
// 
// Original author: Takahiro Kawabata
// C++ version    : Yuji Goto
// Created        : Apr.24, 2003
/////////////////////////////////////////////////////////////////////

#include "utiBunchShuffleLumi.hh"

#include <TF1.h>
#include <TH1.h>
#include <TRandom.h>

#include <cmath>
#include <cstdlib>
#include <string>

using namespace std;

utiBunchShuffleLumi::utiBunchShuffleLumi():
  h1(NULL),
  staterr(NAN),
  fiterr(NAN)
{
  int spflag=0;

  /////////////////////////////////////////////////////////////
  // Define polarization pattern

  ///////////// spin information
  // normal bunch structure
  // 0 before 1st+, 1 before 2nd+, 2 before 1st-, 3 before 2nd- for Blue
  // 0 before +,    1 before - for Yellow

  //1111
  for(int i=0;i<60;i++){
    // zero polarization bunch
    if(i==0 || i==20 || i==40){
      polb[i]=0;poly[(i+20)%60]=0;
      continue;
    }

    // Define blue ring
    switch(spflag%4){
    case 0:
    case 1:
      polb[i]=1;
      break;
    case 2:
    case 3:
      polb[i]=-1;
      break;
    }

    // Define yellow ring	
    if(spflag&0x01) poly[(i+20)%60]=-1;
    else poly[(i+20)%60]=1;
    spflag++;
  }

  // Large emittance bunch
  polb[10]=-50;poly[10]=-50;
  polb[30]=-50;poly[30]=-50;

  // abort gap
  for(int i=55;i<60;i++) polb[i]=-100;
  for(int i=15;i<20;i++) poly[i]=-100;
  //2222

  // parallel or anti-parallel
  for(int i=0;i<60;i++) polby[i]=polb[i]*poly[i];

  /////
  // bunch selection should be done here
  // change polby[i] to abort wrong bunches
  //3333
  /////

  nloop=1;
  strcpy(chopt,"c");
  //  h1 = new TH1D("res","res",200,0.9,1.1);
}

utiBunchShuffleLumi::utiBunchShuffleLumi(const int* pol_b, const int* pol_y, const int n_loop=1, const char *ch_opt="c"){
  for(int i=0; i<60; i++){
    polb[i]=pol_b[i];
    poly[i]=pol_y[i];
  }

  // parallel or anti-parallel
  for(int i=0;i<60;i++) polby[i]=polb[i]*poly[i];

  /////
  // bunch selection should be done here
  // change polby[i] to abort wrong bunches
  //3333
  /////

  nloop=n_loop;
  strcpy(chopt,ch_opt);
  //  h1 = new TH1D("res","res",200,0.9,1.1);
}

utiBunchShuffleLumi::~utiBunchShuffleLumi(){
  if(h1!=0) delete h1;
}

void utiBunchShuffleLumi::BunchShuffle(int* ch1, int* ch2){
  TH1D *h2 = new TH1D("res","res",200,0.9,1.1);

  //  int spflag=0;
  int ininp=0; // Number of spin parallel bunches
  int inina=0; // Number of spin anti-parallel bunches
  int nppf,npaf,napf,naaf;
  int halfmax;
  halfmax=RAND_MAX/2;

  // count the number of spin parallel and anti-parallel crossings
  for(int i=0;i<60;i++) {
    if(polby[i]==1) ininp++;
    if(polby[i]==-1) inina++;
  }
  nppf=ininp/2+ininp%2;
  npaf=ininp/2;
  napf=inina/2+inina%2;
  naaf=inina/2;
  /////////////////////////////////////////////////////////////

  // loop the MC sequence
  for(int iloop=0;iloop<nloop;iloop++){
    //    int spflag=0;  // parameter for make polarization pattern
    int nsppa[60]; // pseude polarization pattern
    
    Int_t scsum1[2];  // sum of gl1p scaler counts
    Int_t scsum2[2];  // [0]..parallel [1]..anti-parallel

    double rell[2];  // spin relative luminosity for each scaler channel
    double ratl=0;      // ratio of spin relative luminosity
    //////////////////////////////////////
    // Make pseud polarization pattern
    for(int i=0;i<60;i++) nsppa[i]=0;    

    /// Radom distribution
    if(strchr(chopt,'c')!=NULL){
      int np=0;  // number of spin parallel crossing
      int na=0;  // number of spin anti-parallel crossing
      for(int i=0;i<60;i++){
	if(abs(polby[i])!=1) continue;
	if(np==ininp){nsppa[i]=-1;continue;}
	if(na==inina){nsppa[i]=1;continue;}
	//	if(gRandom->Rndm()>0.5){nsppa[i]=1;np++;}
	if(rand()>halfmax){nsppa[i]=1;np++;}
	else{nsppa[i]=-1;na++;}
      }
    }

    /// keep numbers of parallel and anti-parallel are equal
    if(strchr(chopt,'C')!=NULL){
      int npp=0,npa=0,nap=0,naa=0;
      for(int i=0;i<60;i++){
	switch(polby[i]){
	case 1:
	  if(npp==nppf){nsppa[i]=-1;break;}
	  if(npa==npaf){nsppa[i]=1;break;}
	  if(gRandom->Rndm()>0.5){nsppa[i]=1;npp++;}
	  else{nsppa[i]=-1;npa++;}
	  break;
	case -1:
	  if(nap==napf){nsppa[i]=-1;break;}
	  if(naa==naaf){nsppa[i]=1;break;}
	  if(gRandom->Rndm()>0.5){nsppa[i]=1;nap++;}
	  else{nsppa[i]=-1;naa++;}
	  break;
	}
      }
    }
    /// use real pattern
    if(strchr(chopt,'n')!=NULL){
      for(int i=0;i<60;i++) nsppa[i]=polby[i];
    }

    //////////////////////////////////////
    // Calculate ratio

    scsum1[0]=0;scsum1[1]=0;
    scsum2[0]=0;scsum2[1]=0;
    //	scdata[i][1]=0;scdata[i][2]=0;uddata[i][1]=0;uddata[i][2]=0;
    //	udrer[i]=0;
    for(int i=0;i<60;i++){
      switch(nsppa[i]){
      case 1:
	scsum1[0]+=ch1[i];
	scsum2[0]+=ch2[i];
	break;
      case -1:
	scsum1[1]+=ch1[i];
	scsum2[1]+=ch2[i];
	break;
      }
    }

    rell[0]=0;rell[1]=0;
    if(scsum1[0]*scsum1[1]*scsum2[0]*scsum2[1]!=0){
      rell[0]=(double)scsum1[0]/scsum1[1];
      rell[1]=(double)scsum2[0]/scsum2[1];
      ratl=rell[0]/rell[1];
      staterr=ratl*sqrt(1.0/scsum1[0]+1.0/scsum1[1]
		      +1.0/scsum2[0]+1.0/scsum2[1]);
    }

    h2->Fill(ratl);
    //    fprintf(stdout,"%7.3f\n",ratl);
  }

  h1 = static_cast<TH1D*>(h2->Clone("res"));

  // Fit the result
  if(strchr(chopt,'c')!=NULL || strchr(chopt,'C')!=NULL){
    TF1 *ffit=new TF1("ffit","gaus");
    //    if(h1->GetSumOfWeights()<10) continue;
    if(h1->GetSumOfWeights()>10){
      ffit->SetParameter(1,h1->GetMean());
      ffit->SetParameter(2,h1->GetRMS());
      ffit->SetParameter(0,(double)nloop/h1->GetRMS());
      h1->Fit(ffit,"QNO");
      fiterr=ffit->GetParameter(2);
    }
  }
}

// so we can include the file declaring strcpy here, not in the header file
void
utiBunchShuffleLumi::SetChOpt(const char *ch_opt)
{
  strcpy(chopt,ch_opt);
}
