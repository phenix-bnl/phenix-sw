
#include "SvxTrackProj.h"

ClassImp(SvxTrackProj);

using namespace std;


SvxTrackProj::Location SvxTrackProj::PrimaryVertex = 0;
SvxTrackProj::Location SvxTrackProj::DCA = 1;
SvxTrackProj::Location SvxTrackProj::DCA2D = 2;
SvxTrackProj::Location SvxTrackProj::Layer0 = 3;
SvxTrackProj::Location SvxTrackProj::Layer1 = 4;
SvxTrackProj::Location SvxTrackProj::Layer2A = 5;
SvxTrackProj::Location SvxTrackProj::Layer2B = 6;
SvxTrackProj::Location SvxTrackProj::Layer2C = 7;
SvxTrackProj::Location SvxTrackProj::Layer3A = 8;
SvxTrackProj::Location SvxTrackProj::Layer3B = 9;
SvxTrackProj::Location SvxTrackProj::Layer3C = 10;
SvxTrackProj::Location SvxTrackProj::DCH = 11;
SvxTrackProj::Location SvxTrackProj::Undefined = 12;

SvxTrackProj::SvxTrackProj( Location location,
                            Float_t x, 
                            Float_t y, 
                            Float_t z, 
                            Float_t px, 
                            Float_t py, 
                            Float_t pz, 
                            Float_t chi_square )
 { }
