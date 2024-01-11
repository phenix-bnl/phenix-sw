#include <L2PHIeeInvMassMicrov1.h>
#include <L2PHIeeInvSnglMassMicrov1.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

static unsigned int NPHIPAIR = 1; 

ClassImp(L2PHIeeInvMassMicrov1)

using namespace std;
//______________________________
L2PHIeeInvMassMicrov1::L2PHIeeInvMassMicrov1()
{
  NumPHIPair = 0;
  L2PHIeePair = new TClonesArray("L2PHIeeInvSnglMassMicrov1", NPHIPAIR);
}

//______________________________
L2PHIeeInvMassMicrov1::~L2PHIeeInvMassMicrov1()
{
  if (L2PHIeePair)
    {
      L2PHIeePair->Clear();
      delete L2PHIeePair;
    }
  return;
}

//______________________________
void L2PHIeeInvMassMicrov1::identify(ostream& os) const
{
  os << "identify yourself: L2PHIeeInvMassMicrov1 Object" << endl;
  os << "No of PHI pair candidate: " << NumPHIPair << endl;
}

//______________________________
void L2PHIeeInvMassMicrov1::Reset()
{
  L2PHIeePair->Clear();
  if (NumPHIPair>NPHIPAIR)
  {
     L2PHIeePair->Expand(NPHIPAIR);
  }
  NumPHIPair = 0;
}

//______________________________
int L2PHIeeInvMassMicrov1::isValid() const
{
  return ((NumPHIPair>0) ? 1 : 0);
}

//______________________________
int L2PHIeeInvMassMicrov1::set_TClonesArraySize(unsigned int input)
{
  if (NumPHIPair > NPHIPAIR)
    {
      L2PHIeePair->Expand(NumPHIPair);
     }
  return NumPHIPair;

}

//______________________________
void L2PHIeeInvMassMicrov1::AddL2PHIeePair(unsigned int ipair)
{
  TClonesArray &L2PHI = *L2PHIeePair;
  new(L2PHI[ipair]) L2PHIeeInvSnglMassMicrov1();
  //cout<<" AddL2PHIeePair: ipair = "<<ipair<<endl;
}

//_______________________________________________________________________
int   L2PHIeeInvMassMicrov1::get_1stCandidateID(unsigned int i) const
{
   L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1*) GetL2PHIeePair()->UncheckedAt(i);
   return ((L2PHI) ? L2PHI->get_1stCandidateID() : -9999);
}
//_______________________________________________________________________
int   L2PHIeeInvMassMicrov1::get_2ndCandidateID(unsigned int i) const
{
   L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1*) GetL2PHIeePair()->UncheckedAt(i);
   return ((L2PHI) ? L2PHI->get_2ndCandidateID() : -9999);
}
//_______________________________________________________________________
float   L2PHIeeInvMassMicrov1::get_Mass(unsigned int i) const
{
   L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1*) GetL2PHIeePair()->UncheckedAt(i);
   return ((L2PHI) ? L2PHI->get_Mass() : -9999);
}

//________________________________________________________________________
void  L2PHIeeInvMassMicrov1::set_1stCandidateID(unsigned int i, int input)
{
  L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1 *) GetL2PHIeePair()->UncheckedAt(i);
  if (L2PHI)
  {
      L2PHI->set_1stCandidateID(input);
  } else {
    cout << PHWHERE << "ERROR no L2PHIeeInvSnglMassMicrov1 object found" << endl;
  }
}
//________________________________________________________________________
void  L2PHIeeInvMassMicrov1::set_2ndCandidateID(unsigned int i, int input)
{
  L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1 *) GetL2PHIeePair()->UncheckedAt(i);
  if (L2PHI)
  {
      L2PHI->set_2ndCandidateID(input);
  } else {
    cout << PHWHERE << "ERROR no L2PHIeeInvSnglMassMicrov1 object found" << endl;
  }
}
//_____________________________________________________________________
void  L2PHIeeInvMassMicrov1::set_Mass(unsigned int i, float input)
{
  L2PHIeeInvSnglMassMicrov1 *L2PHI = (L2PHIeeInvSnglMassMicrov1 *) GetL2PHIeePair()->UncheckedAt(i);
  if (L2PHI)
  {
      L2PHI->set_Mass(input);
  } else {
    cout << PHWHERE << "ERROR no L2PHIeeInvSnglMassMicrov1 object found" << endl;
  }
}
