
#include <PHHijingHeaderV2.h>

ClassImp(PHHijingHeaderV2);

PHHijingHeaderV2::PHHijingHeaderV2()
{
  
  event = 0;
  npart = 0;
  atarg = 0;
  ztarg = 0;
  aproj = 0;
  zproj = 0;
  ecoll = 0;
  nbin = 0;
  bimpact = 0;
  
  // setting the primary vertex to zero here is safe
  // since this is usually what pythia does.
  primary_vertex_x = 0; 
  primary_vertex_y = 0;  
  primary_vertex_z = 0; 
  
}

void
PHHijingHeaderV2::Reset()
{

}

void
PHHijingHeaderV2::identify(std::ostream& os) const
{

}

void
PHHijingHeaderV2::print(std::ostream& os) const
{

}
