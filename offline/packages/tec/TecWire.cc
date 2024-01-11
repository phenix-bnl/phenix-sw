#include "TecWire.hh"

TecWire::TecWire() {
  index=-1;
  wire=-1;
  for(int i=0; i<TECMAXTIMEBIN; i++) { adc[i]=0; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { charge[i]=0.; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { hitnumber[i]=-1; }
  xyz[0]=0.;
  xyz[1]=0.;
  xyz[2]=0.;
}

TecWire::TecWire(int iindex, int iwire) {
  index=iindex;
  wire=iwire;
  for(int i=0; i<TECMAXTIMEBIN; i++) { adc[i]=0; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { charge[i]=0.; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { hitnumber[i]=-1; }
  xyz[0]=0.;
  xyz[1]=0.;
  xyz[2]=0.;
}

TecWire::TecWire(int iindex, int iwire, int* iadc, float* fcharge, float* fxyz) {
  index=iindex;
  wire=iwire;
  for(int i=0; i<TECMAXTIMEBIN; i++) { adc[i]=iadc[i]; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { charge[i]=fcharge[i]; }
  for(int i=0; i<TECMAXTIMEBIN; i++) { hitnumber[i]=-1; }
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
  xyz[2]=fxyz[2];
}

void TecWire::identify(std::ostream& out) const {
  out << "Single Tec Wire." << std::endl;
}

