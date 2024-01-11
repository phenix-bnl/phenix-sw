#include "TecHit.hh"
#include <iostream>

ClassImp(TecHit)

using namespace std;

TecHit::TecHit() {
  index=0;
  trackid = -1;
  adc=0;
  charge=0.;
  xyz[0]=0.;
  xyz[1]=0.;
}

TecHit::TecHit(int iindex, int iadc, float fcharge,
               float* fxyz, int itrackid) {
  index = iindex;
  adc=iadc;
  charge=fcharge;
  trackid=itrackid;
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
}

TecHit::TecHit(int iindex, 
               int iwire, int ibin, int iadc, float fcharge,
               float* fxyz, int itrackid) {
  index = iindex*100000 + iwire*100 + ibin;
  adc=iadc;
  charge=fcharge;
  trackid=itrackid;
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
}

TecHit::TecHit(int isector, int iside, int iplane, 
               int iwire, int ibin, int iadc, float fcharge, 
               float* fxyz, int itrackid) { 
  index = (isector*TECMAXPLANE*TECMAXSIDE + iplane*TECMAXSIDE + iside)*100000 +
           iwire*100 + ibin;
  adc=iadc;
  charge=fcharge;
  trackid=itrackid;
  xyz[0]=fxyz[0];
  xyz[1]=fxyz[1];
}

TecHit::TecHit(int isector, int iside, int iplane, int iwire, int ibin,
               int iadc, float fcharge,
               TecGeometryObject* TGO, TecCalibrationObject* TCO, int itrackid) {

  index = (isector*TECMAXPLANE*TECMAXSIDE + iplane*TECMAXSIDE + iside)*100000 +
           iwire*100 + ibin;
  trackid = itrackid;
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
    float FirstBin = TCO->getFirstTimeBin(index);
    float LastBin = TCO->getLastTimeBin(index);
    float relativebin = (ibin - FirstBin) / (LastBin - FirstBin);
    xyz[0] = Xwire + (TECYWIDTH - TECWIREDIST) * Cosinus * relativebin;
    xyz[1] = Ywire + (TECYWIDTH - TECWIREDIST) * Sinus * relativebin;

}


void TecHit::identify(ostream& out) const {
  out << "I am a TecHit object." << endl;
}


int TecHit::get_plane() {
  int tmp = index/100000;
  return (tmp-(tmp/(TECMAXPLANE*TECMAXSIDE))*(TECMAXPLANE*TECMAXSIDE))/TECMAXSIDE;
}

int TecHit::get_wire() {
  int tmp = index%100000;
  return tmp/100;
}

int TecHit::get_bin() {
  return (index%100000)%100;
}

int TecHit::get_side() {
  return (index/100000)%TECMAXSIDE;
}

int TecHit::get_sector() {
  return index/(TECMAXPLANE*TECMAXSIDE*100000);
}

void TecHit::set_index(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  iindex = i;
  index = iindex*100000 + iwire*100 + ibin;
}

void TecHit::set_wire(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  iwire = i;
  index = iindex*100000 + iwire*100 + ibin;
}

void TecHit::set_bin(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  ibin = i;
  index = iindex*100000 + iwire*100 + ibin;
}





