
#include "TecTrackHit.hh"

TecTrackHit::TecTrackHit(const TecTrackV2 &source) {

  float in[3],out[3];
  in[0]=source.getXin(); in[1]=source.getYin(); in[2]=source.getZin();
  out[0]=source.getXout(); out[1]=source.getYout(); out[2]=source.getZout();
  TRACK = TecTrackV2(in,out);

  TRACK.setNhits(source.getNhits());
  //TRACK.setPc3Pointer(source.getPc3Pointer());
  //TRACK.setPc3Distance(source.getPc3Distance());
  //TRACK.setPc3sPointer(source.getPc3sDistance());
  //TRACK.setPc3sDistance(source.getPc3sDistance());
  TRACK.setSector(source.getSector());
  TRACK.setSide(source.getSide());
  for(int k=0; k<6; k++) TRACK.setNwires(k, source.getNwires(k));
  for(int k=0; k<6; k++) TRACK.setNHITS(k, source.getNHITS(k));

}

TecTrackHit::~TecTrackHit() { Clear(); }

//---------------------------------------------------------

void TecTrackHit::Clear() {
  HITS.clear();
  return;
}

//void AddTrack(TecTrackV2& track);

int TecTrackHit::AddHit(TecHit& hit) {
  HITS.push_back(hit);
  return HITS.size();
}

TecHit* TecTrackHit::getHit(int i) {
  //TecHit hit = HITS[i];
  //return &hit;
	return &HITS[i];
}



