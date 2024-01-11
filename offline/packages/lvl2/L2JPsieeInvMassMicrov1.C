#include <L2JPsieeInvMassMicrov1.h>
#include <L2JPsieeInvSnglMassMicrov1.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

static unsigned int NJPSIPAIR = 1; 

ClassImp(L2JPsieeInvMassMicrov1)

using namespace std;

//______________________________
L2JPsieeInvMassMicrov1::L2JPsieeInvMassMicrov1()
{
  NumJPsiPair = 0;
  L2JPsieePair = new TClonesArray("L2JPsieeInvSnglMassMicrov1", NJPSIPAIR);
}

//______________________________
L2JPsieeInvMassMicrov1::~L2JPsieeInvMassMicrov1()
{
  if (L2JPsieePair)
    {
      L2JPsieePair->Clear();
      delete L2JPsieePair;
    }
  return;
}

//______________________________
void L2JPsieeInvMassMicrov1::identify(ostream& os) const
{
  os << "identify yourself: L2JPsieeInvMassMicrov1 Object" << endl;
  os << "No of JPsi pair candidate: " << NumJPsiPair << endl;
}

//______________________________
void L2JPsieeInvMassMicrov1::Reset()
{
  L2JPsieePair->Clear();
  if (NumJPsiPair>NJPSIPAIR)
  {
     L2JPsieePair->Expand(NJPSIPAIR);
  }
  NumJPsiPair = 0;
}

//______________________________
int L2JPsieeInvMassMicrov1::isValid() const
{
  return ((NumJPsiPair>0) ? 1 : 0);
}

//______________________________
int L2JPsieeInvMassMicrov1::set_TClonesArraySize(unsigned int input)
{
  if (NumJPsiPair > NJPSIPAIR)
    {
      L2JPsieePair->Expand(NumJPsiPair);
     }
  return NumJPsiPair;

}

//______________________________
void L2JPsieeInvMassMicrov1::AddL2JPsieePair(unsigned int ipair)
{
  TClonesArray &L2JPsi = *L2JPsieePair;
  new(L2JPsi[ipair]) L2JPsieeInvSnglMassMicrov1();
  //cout<<" AddL2JPsieePair: ipair = "<<ipair<<endl;
}

//_______________________________________________________________________
int   L2JPsieeInvMassMicrov1::get_1stCandidateID(unsigned int i) const
{
   L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1*) GetL2JPsieePair()->UncheckedAt(i);
   return ((L2JPsi) ? L2JPsi->get_1stCandidateID() : -9999);
}
//_______________________________________________________________________
int   L2JPsieeInvMassMicrov1::get_2ndCandidateID(unsigned int i) const
{
   L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1*) GetL2JPsieePair()->UncheckedAt(i);
   return ((L2JPsi) ? L2JPsi->get_2ndCandidateID() : -9999);
}
//_______________________________________________________________________
float   L2JPsieeInvMassMicrov1::get_Mass(unsigned int i) const
{
   L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1*) GetL2JPsieePair()->UncheckedAt(i);
   return ((L2JPsi) ? L2JPsi->get_Mass() : -9999);
}

//________________________________________________________________________
void  L2JPsieeInvMassMicrov1::set_1stCandidateID(unsigned int i, int input)
{
  L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1 *) GetL2JPsieePair()->UncheckedAt(i);
  if (L2JPsi)
  {
      L2JPsi->set_1stCandidateID(input);
  } else {
    cout << PHWHERE << "ERROR no L2JPsieeInvSnglMassMicrov1 object found" << endl;
  }
}
//________________________________________________________________________
void  L2JPsieeInvMassMicrov1::set_2ndCandidateID(unsigned int i, int input)
{
  L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1 *) GetL2JPsieePair()->UncheckedAt(i);
  if (L2JPsi)
  {
      L2JPsi->set_2ndCandidateID(input);
  } else {
    cout << PHWHERE << "ERROR no L2JPsieeInvSnglMassMicrov1 object found" << endl;
  }
}
//_____________________________________________________________________
void  L2JPsieeInvMassMicrov1::set_Mass(unsigned int i, float input)
{
  L2JPsieeInvSnglMassMicrov1 *L2JPsi = (L2JPsieeInvSnglMassMicrov1 *) GetL2JPsieePair()->UncheckedAt(i);
  if (L2JPsi)
  {
      L2JPsi->set_Mass(input);
  } else {
    cout << PHWHERE << "ERROR no L2JPsieeInvSnglMassMicrov1 object found" << endl;
  }
}
