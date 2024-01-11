#include <L2ElecInvMassLowOcupyMicroV1.h>
#include <L2ElecSnglInvMassLowOcupyMicroV1.h>

static unsigned int NELECTRONCANDIDATE = 100; 

ClassImp(L2ElecInvMassLowOcupyMicroV1);

using namespace std;

//______________________________
L2ElecInvMassLowOcupyMicroV1::L2ElecInvMassLowOcupyMicroV1()
{
  NumCandidate = 0;
  L2ElecCandidate = new TClonesArray("L2ElecSnglInvMassLowOcupyMicroV1", NELECTRONCANDIDATE);
}

//______________________________
L2ElecInvMassLowOcupyMicroV1::~L2ElecInvMassLowOcupyMicroV1()
{
  L2ElecCandidate->Clear();
}

//______________________________
void L2ElecInvMassLowOcupyMicroV1::identify(ostream& os) const
{
  os << "identify yourself: L2ElecInvMassLowOcupyMicroV1 Object" << endl;
  os << "No of electron candidate : " << NumCandidate << endl;
}

//______________________________
void L2ElecInvMassLowOcupyMicroV1::Reset()
{
  L2ElecCandidate->Clear();
  if (NumCandidate>NELECTRONCANDIDATE)
  {
     L2ElecCandidate->Expand(NELECTRONCANDIDATE);
  }
  NumCandidate = 0;
}

//______________________________
int L2ElecInvMassLowOcupyMicroV1::isValid() const
{
  return ((NumCandidate>0) ? 1 : 0);
}

//______________________________
int L2ElecInvMassLowOcupyMicroV1::set_TClonesArraySize(unsigned int input)
{
  if (input > NELECTRONCANDIDATE)
    {
      L2ElecCandidate->Expand(input);
     }
  return input;
}

//______________________________
void L2ElecInvMassLowOcupyMicroV1::AddL2ElecCandidate(unsigned int icandidate)
{
  TClonesArray &L2Elec = *L2ElecCandidate;
  new(L2Elec[icandidate]) L2ElecSnglInvMassLowOcupyMicroV1();
}

//_______________________________________
bool   L2ElecInvMassLowOcupyMicroV1::get_KilledByChargeID(unsigned int i) const
{
   L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_KilledByChargeID() : -9999);
}

//.......................
void  L2ElecInvMassLowOcupyMicroV1::set_KilledByChargeID(unsigned int i, bool input)
{
  L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_KilledByChargeID(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglInvMassLowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecInvMassLowOcupyMicroV1::get_Mass(unsigned int i) const
{
   L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_Mass() : -9999);
}

//.......................
void  L2ElecInvMassLowOcupyMicroV1::set_Mass(unsigned int i, float input)
{
  L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_Mass(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglInvMassLowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecInvMassLowOcupyMicroV1::get_Pt(unsigned int i) const
{
   L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_Pt() : -9999);
}

//.......................
void  L2ElecInvMassLowOcupyMicroV1::set_Pt(unsigned int i, float input)
{
  L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_Pt(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglInvMassLowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
int   L2ElecInvMassLowOcupyMicroV1::get_candID0(unsigned int i) const
{
   L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_candID0() : -9999);
}

//.......................
void  L2ElecInvMassLowOcupyMicroV1::set_candID0(unsigned int i, int input)
{
  L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_candID0(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglInvMassLowOcupyMicroV1 object found" << endl;
  }
}
//_______________________________________
int   L2ElecInvMassLowOcupyMicroV1::get_candID1(unsigned int i) const
{
   L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_candID1() : -9999);
}

//.......................
void  L2ElecInvMassLowOcupyMicroV1::set_candID1(unsigned int i, int input)
{
  L2ElecSnglInvMassLowOcupyMicroV1 *L2Elec = (L2ElecSnglInvMassLowOcupyMicroV1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_candID1(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglInvMassLowOcupyMicroV1 object found" << endl;
  }
}
