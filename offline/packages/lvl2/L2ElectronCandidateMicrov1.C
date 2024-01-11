#include <L2ElectronCandidateMicrov1.h>
#include <L2ElectronSnglCandidateMicrov1.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

static unsigned int NELECTRONCANDIDATE = 50; 

ClassImp(L2ElectronCandidateMicrov1)

using namespace std;

//______________________________
L2ElectronCandidateMicrov1::L2ElectronCandidateMicrov1()
{
  NumCandidate = 0;
  L2ElecCandidate = new TClonesArray("L2ElectronSnglCandidateMicrov1", NELECTRONCANDIDATE);
}

//______________________________
L2ElectronCandidateMicrov1::~L2ElectronCandidateMicrov1()
{
  if (L2ElecCandidate)
    {
      L2ElecCandidate->Clear();
      delete L2ElecCandidate;
    }
  return;
}

//______________________________
void L2ElectronCandidateMicrov1::identify(ostream& os) const
{
  os << "identify yourself: L2ElectronCandidateMicrov1 Object" << endl;
  os << "No of electron candidate : " << NumCandidate << endl;
}

//______________________________
void L2ElectronCandidateMicrov1::Reset()
{
  L2ElecCandidate->Clear();
  if (NumCandidate>NELECTRONCANDIDATE)
  {
     L2ElecCandidate->Expand(NELECTRONCANDIDATE);
  }
  NumCandidate = 0;
}

//______________________________
int L2ElectronCandidateMicrov1::isValid() const
{
  return ((NumCandidate>0) ? 1 : 0);
}

//______________________________
int L2ElectronCandidateMicrov1::set_TClonesArraySize(unsigned int input)
{
  if (NumCandidate > NELECTRONCANDIDATE)
    {
      L2ElecCandidate->Expand(NumCandidate);
     }
  return NumCandidate;

}

//______________________________
void L2ElectronCandidateMicrov1::AddL2ElecCandidate(unsigned int icandidate)
{
  TClonesArray &L2Elec = *L2ElecCandidate;
  new(L2Elec[icandidate]) L2ElectronSnglCandidateMicrov1();
  //cout<<" AddL2ElecCandidate: icandidate = "<<icandidate<<endl;
}

//______________________________
Bool_t  L2ElectronCandidateMicrov1::get_KilledByPC3(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_KilledByPC3() : true);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_ChargeLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_ChargeLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_Theta0Lvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_Theta0Lvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_Phi0Lvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_Phi0Lvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_Ptot0Lvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_Ptot0Lvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_NtowerLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_NtowerLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_ArmLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_ArmLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_SectorLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_SectorLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_EcentLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_EcentLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_IyLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_IyLvl2() : -9999);
}

//______________________________
float   L2ElectronCandidateMicrov1::get_IzLvl2(unsigned int i) const
{
   L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_IzLvl2() : -9999);
}
//_______________________________
void  L2ElectronCandidateMicrov1::set_KilledByPC3(unsigned int i, Bool_t input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_KilledByPC3(input);
      //cout<<" set_KilledByPC3:  i = "<<i<<"  input = "<<input<<endl;
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_ChargeLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_ChargeLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_Theta0Lvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_Theta0Lvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_Phi0Lvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_Phi0Lvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_Ptot0Lvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_Ptot0Lvl2(input);
      //cout<<" set_Ptot0Lvl2:  i = "<<i<<"  input = "<<input<<endl;
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_NtowerLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_NtowerLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_ArmLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_ArmLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_SectorLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_SectorLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_EcentLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_EcentLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_IyLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_IyLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}

//_______________________________
void  L2ElectronCandidateMicrov1::set_IzLvl2(unsigned int i, float input)
{
  L2ElectronSnglCandidateMicrov1 *L2Elec = (L2ElectronSnglCandidateMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_IzLvl2(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElectronSnglCandidateMicrov1 object found" << endl;
  }
}
