#include <L2TrackPC3LowOcupyMicroV1.h>
#include <L2SingleTrackPC3LowOcupyMicroV1.h>

static unsigned int NTRACKS = 500; 

ClassImp(L2TrackPC3LowOcupyMicroV1);

using namespace std;

//______________________________
L2TrackPC3LowOcupyMicroV1::L2TrackPC3LowOcupyMicroV1()
{
  NumTracks = 0;
  L2Track = new TClonesArray("L2SingleTrackPC3LowOcupyMicroV1", NTRACKS);
}

//______________________________
L2TrackPC3LowOcupyMicroV1::~L2TrackPC3LowOcupyMicroV1()
{
 L2Track->Clear();
}

//______________________________
void L2TrackPC3LowOcupyMicroV1::identify(ostream& os) const
{
  os << "identify yourself: L2TrackPC3LowOcupyMicroV1 Object" << endl;
  os << "No of PC1/PC3 tracks : " << NumTracks << endl;
}

//______________________________
void L2TrackPC3LowOcupyMicroV1::Reset()
{
 L2Track->Clear();
  if (NumTracks>NTRACKS)
  {
    L2Track->Expand(NTRACKS);
  }
  NumTracks = 0;
}

//______________________________
int L2TrackPC3LowOcupyMicroV1::isValid() const
{
  return ((NumTracks>0) ? 1 : 0);
}

//______________________________
int L2TrackPC3LowOcupyMicroV1::set_TClonesArraySize(unsigned int input)
{
  if (input > NTRACKS)
    {
     L2Track->Expand(input);
     }
  return input;

}

//______________________________
void L2TrackPC3LowOcupyMicroV1::AddL2TrackPC3(unsigned int itrack)
{
  TClonesArray &L2Tr = *L2Track;
  new(L2Tr[itrack]) L2SingleTrackPC3LowOcupyMicroV1();
}

//_______________________________________
int  L2TrackPC3LowOcupyMicroV1::get_arm(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_arm() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_arm(unsigned int i, int input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_arm(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_alpha(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_alpha() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_alpha(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_alpha(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_beta(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_beta() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_beta(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_beta(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_p(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_p() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_p(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_p(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pxdet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pxdet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pxdet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pxdet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pydet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pydet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pydet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pydet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pzdet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pzdet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pzdet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pzdet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pxphys(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pxphys() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pxphys(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pxphys(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pyphys(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pyphys() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pyphys(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pyphys(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_pzphys(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_pzphys() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_pzphys(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_pzphys(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_zvertex(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_zvertex() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_zvertex(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_zvertex(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_xdet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_xdet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_xdet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_xdet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_ydet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_ydet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_ydet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_ydet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_zdet(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_zdet() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_zdet(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_zdet(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_xdetout(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_xdetout() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_xdetout(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_xdetout(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_ydetout(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_ydetout() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_ydetout(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_ydetout(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_zdetout(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_zdetout() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_zdetout(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_zdetout(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_theta0(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_theta0() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_theta0(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_theta0(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_phi0(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_phi0() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_phi0(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_phi0(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2TrackPC3LowOcupyMicroV1::get_charge(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_charge() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_charge(unsigned int i, float input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_charge(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
unsigned int L2TrackPC3LowOcupyMicroV1::get_regionPC3(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_regionPC3() : 999999);
}

//.......................
void L2TrackPC3LowOcupyMicroV1::set_regionPC3(unsigned int i, unsigned int input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_regionPC3(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
unsigned int L2TrackPC3LowOcupyMicroV1::get_clustIDPC3(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_clustIDPC3() : 999999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_clustIDPC3(unsigned int i, unsigned int input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_clustIDPC3(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}

//_______________________________________
int L2TrackPC3LowOcupyMicroV1::get_CNTmatch(unsigned int i) const
{
   L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1*) GetL2Track()->UncheckedAt(i);
   return ((L2Tr) ? L2Tr->get_CNTmatch() : -9999);
}

//.......................
void  L2TrackPC3LowOcupyMicroV1::set_CNTmatch(unsigned int i, int input)
{
  L2SingleTrackPC3LowOcupyMicroV1 *L2Tr = (L2SingleTrackPC3LowOcupyMicroV1 *) GetL2Track()->UncheckedAt(i);
  if (L2Tr)
  {
      L2Tr->set_CNTmatch(input);
  } else {
    cout << PHWHERE << "ERROR no L2SingleTrackPC3LowOcupyMicroV1 object found" << endl;
  }
}













