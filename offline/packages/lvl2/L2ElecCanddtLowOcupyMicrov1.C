#include <L2ElecCanddtLowOcupyMicrov1.h>
#include <L2ElecSnglCanddtLowOcupyMicrov1.h>

static unsigned int NELECTRONCANDIDATE = 50; 

ClassImp(L2ElecCanddtLowOcupyMicrov1);

using namespace std;

//______________________________
L2ElecCanddtLowOcupyMicrov1::L2ElecCanddtLowOcupyMicrov1()
{
  NumCandidate = 0;
  L2ElecCandidate = new TClonesArray("L2ElecSnglCanddtLowOcupyMicrov1", NELECTRONCANDIDATE);
}

//______________________________
L2ElecCanddtLowOcupyMicrov1::~L2ElecCanddtLowOcupyMicrov1()
{
  L2ElecCandidate->Clear();
}

//______________________________
void L2ElecCanddtLowOcupyMicrov1::identify(ostream& os) const
{
  os << "identify yourself: L2ElecCanddtLowOcupyMicrov1 Object" << endl;
  os << "No of electron candidate : " << NumCandidate << endl;
}

//______________________________
void L2ElecCanddtLowOcupyMicrov1::Reset()
{
  L2ElecCandidate->Clear();
  if (NumCandidate>NELECTRONCANDIDATE)
  {
     L2ElecCandidate->Expand(NELECTRONCANDIDATE);
  }
  NumCandidate = 0;
}

//______________________________
int L2ElecCanddtLowOcupyMicrov1::isValid() const
{
  return ((NumCandidate>0) ? 1 : 0);
}

//______________________________
int L2ElecCanddtLowOcupyMicrov1::set_TClonesArraySize(unsigned int input)
{
  if (input > NELECTRONCANDIDATE)
    {
      L2ElecCandidate->Expand(input);
     }
  return input;

}

//______________________________
void L2ElecCanddtLowOcupyMicrov1::AddL2ElecCandidate(unsigned int icandidate)
{
  TClonesArray &L2Elec = *L2ElecCandidate;
  new(L2Elec[icandidate]) L2ElecSnglCanddtLowOcupyMicrov1();
}

//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_charge(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_charge() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_charge(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_charge(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_theta0(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_theta0() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_theta0(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_theta0(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_phi0(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_phi0() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_phi0(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_phi0(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_ptot(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_ptot() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_ptot(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_ptot(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_xrich(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_xrich() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_xrich(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_xrich(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_yrich(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_yrich() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_yrich(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_yrich(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_zrich(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_zrich() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_zrich(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_zrich(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
int   L2ElecCanddtLowOcupyMicrov1::get_pmtCent(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_pmtCent() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_pmtCent(unsigned int i, int input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_pmtCent(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
int   L2ElecCanddtLowOcupyMicrov1::get_npmt(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_npmt() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_npmt(unsigned int i, int input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_npmt(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_npe(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_npe() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_npe(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_npe(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_TrkRingDist(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_TrkRingDist() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_TrkRingDist(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_TrkRingDist(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_xemc(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_xemc() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_xemc(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_xemc(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_yemc(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_yemc() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_yemc(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_yemc(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_zemc(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_zemc() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_zemc(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_zemc(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_energy(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_energy() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_energy(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_energy(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_xpc1(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_xpc1() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_xpc1(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_xpc1(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_ypc1(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_ypc1() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_ypc1(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_ypc1(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
//_______________________________________
float   L2ElecCanddtLowOcupyMicrov1::get_zpc1(unsigned int i) const
{
   L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1*) GetL2ElecCandidate()->UncheckedAt(i);
   return ((L2Elec) ? L2Elec->get_zpc1() : -9999);
}

//.......................
void  L2ElecCanddtLowOcupyMicrov1::set_zpc1(unsigned int i, float input)
{
  L2ElecSnglCanddtLowOcupyMicrov1 *L2Elec = (L2ElecSnglCanddtLowOcupyMicrov1 *) GetL2ElecCandidate()->UncheckedAt(i);
  if (L2Elec)
  {
      L2Elec->set_zpc1(input);
  } else {
    cout << PHWHERE << "ERROR no L2ElecSnglCanddtLowOcupyMicrov1 object found" << endl;
  }
}
