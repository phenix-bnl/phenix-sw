#include "TecLongHit.hh"

ClassImp(TecLongHit)

TecLongHit::TecLongHit() { 
  index=0;
  wire=0;
  bin=0;
  trackid[0] = -1;
  trackid[0] = -1;
  trackid[0] = -1;
  adc=0;
  charge=0.;
  xyz[0]=0.;
  xyz[1]=0.;
  xyz[2]=0.;
}

TecLongHit::TecLongHit(int iindex, 
               int iwire, int ibin, int iadc, float fcharge,
               float* fxyz, int itrackid) {
  index = iindex;
  wire=iwire;
  bin=ibin;
  adc=iadc;
  charge=fcharge;
  trackid[0]=itrackid;
  trackid[1]=-1;
  trackid[2]=-1;
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
  xyz[2]=fxyz[2];
}

TecLongHit::TecLongHit(int isector, int iside, int iplane, 
               int iwire, int ibin, int iadc, float fcharge, 
               float* fxyz, int itrackid) { 
  index = isector*TECMAXPLANE*TECMAXSIDE + iplane*TECMAXSIDE + iside;
  wire=iwire;
  bin=ibin;
  adc=iadc;
  charge=fcharge;
  trackid[0]=itrackid;
  trackid[1]=-1;
  trackid[2]=-1;
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
  xyz[2]=fxyz[2];
}

TecLongHit::TecLongHit(int isector, int iside, int iplane, int iwire, int ibin,
               int iadc, float fcharge,
               TecGeometryObject* TGO, TecCalibrationObject* TCO, int itrackid) {

  index = isector*TECMAXPLANE*TECMAXSIDE + iplane*TECMAXSIDE + iside;
  wire=iwire;
  bin=ibin;
  trackid[0] = itrackid;
  trackid[1] = -1;
  trackid[2] = -1;
  adc=iadc;
  charge=fcharge;

// Calculate coordinates of the time bin of the wire 

// First calculate normal to the plane
    PHPanel tecpanel = TGO->getTecPanel(index);
    PHVector normal = tecpanel.getNormal ();
    double Cosinus = normal.getX();
    double Sinus = normal.getY();

// Get wire coordinates
    float Xwire = TGO->getGlobalX(index,iwire);
    float Ywire = TGO->getGlobalY(index,iwire);

// Calculate time bin coordinates
    float FirstBin = (float)TCO->getFirstTimeBin(index);
    float LastBin = (float)TCO->getLastTimeBin(index);
    float relativebin = ((float)ibin - FirstBin)/(LastBin - FirstBin);
    xyz[0] = Xwire + (TECYWIDTH - TECWIREDIST) * Cosinus * relativebin;
    xyz[1] = Ywire + (TECYWIDTH - TECWIREDIST) * Sinus * relativebin;
    xyz[2] = 1.; if(iside==0) {xyz[2]=-1.;}

}

void TecLongHit::identify(ostream& out) const {
  out << "I am a TecLongHit object." << endl;
}


