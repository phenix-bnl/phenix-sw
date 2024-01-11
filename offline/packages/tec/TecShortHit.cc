#include "TecHit.hh"
#include "TecShortHit.hh"
#include <iostream>

ClassImp(TecShortHit)

using namespace std;

TecShortHit::TecShortHit() {
  index=0;
  trackid = -1;
  adc=0;
}

TecShortHit::TecShortHit(TecHit* techit) { 
  index=techit->get_index();
  trackid = techit->get_trackid();
  adc=techit->get_adc();
}

TecShortHit::TecShortHit(int iindex, int iadc, int itrackid) {
  index = iindex;
  adc=iadc;
  trackid=itrackid;
}

TecShortHit::TecShortHit(int iindex, int iadc, float fcharge,
               float* fxyz, int itrackid) {
  index = iindex;
  adc=iadc;
  trackid=itrackid;
}

TecShortHit::TecShortHit(int iindex, 
               int iwire, int ibin, int iadc, float fcharge,
               float* fxyz, int itrackid) {
  index = iindex*100000 + iwire*100 + ibin;
  adc=iadc;
  trackid=itrackid;
}

TecShortHit::TecShortHit(int isector, int iside, int iplane, 
               int iwire, int ibin, int iadc, float fcharge, 
               float* fxyz, int itrackid) { 
  index = (isector*TECMAXPLANE*TECMAXSIDE + iplane*TECMAXSIDE + iside)*100000 +
           iwire*100 + ibin;
  adc=iadc;
  trackid=itrackid;
}


void TecShortHit::identify(ostream& out) const {
  out << "I am a TecShortHit object." << endl;
}


int TecShortHit::get_plane() {
  int tmp = index/100000;
  return (tmp-(tmp/(TECMAXPLANE*TECMAXSIDE))*(TECMAXPLANE*TECMAXSIDE))/TECMAXSIDE;
}

int TecShortHit::get_wire() {
  int tmp = index%100000;
  return tmp/100;
}

int TecShortHit::get_bin() {
  return (index%100000)%100;
}

int TecShortHit::get_side() {
  return (index/100000)%TECMAXSIDE;
}

int TecShortHit::get_sector() {
  return index/(TECMAXPLANE*TECMAXSIDE*100000);
}

void TecShortHit::set_index(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  iindex = i;
  index = iindex*100000 + iwire*100 + ibin;
}

void TecShortHit::set_wire(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  iwire = i;
  index = iindex*100000 + iwire*100 + ibin;
}

void TecShortHit::set_bin(int i) {
  int iindex = index/100000;
  int iwire = (index%100000)/100;
  int ibin = (index%100000)%100;
  ibin = i;
  index = iindex*100000 + iwire*100 + ibin;
}





