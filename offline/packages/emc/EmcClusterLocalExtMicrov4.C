#include "EmcClusterLocalExtMicrov4.h"
#include "EmcSnglClusterLocalExtMicrov4.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(EmcClusterLocalExtMicrov4)

using namespace std;

static const unsigned int EMCNCLUSV4 = 200;

EmcClusterLocalExtMicrov4::EmcClusterLocalExtMicrov4()
{
  EmcNCluster = 0;
  EmcClus = new TClonesArray("EmcSnglClusterLocalExtMicrov4",EMCNCLUSV4);
  return;
}

EmcClusterLocalExtMicrov4::~EmcClusterLocalExtMicrov4()
{
  if (EmcClus)
    {
      EmcClus->Clear();
      delete EmcClus;
    }
  return;
}

void EmcClusterLocalExtMicrov4::identify(ostream& os) const
{
  os << "identify yourself: EmcClusterLocalExtMicrov4 Object" << std::endl;
  os << "No of Clusters: " << EmcNCluster << std::endl;
  return;
}

void EmcClusterLocalExtMicrov4::Reset()
{
  EmcClus->Clear();
  if (EmcNCluster>EMCNCLUSV4)
    {
      EmcClus->Expand(EMCNCLUSV4);
    }
  EmcNCluster = 0;
  return;
}

int EmcClusterLocalExtMicrov4::isValid() const
{
  return((EmcNCluster>0) ? 1 : 0);
}

int EmcClusterLocalExtMicrov4::set_TClonesArraySize(unsigned int nclus)
{
  if (nclus > EMCNCLUSV4)
    {
      EmcClus->Expand(nclus);
    }
  return nclus;
}

void  EmcClusterLocalExtMicrov4::AddEmcCluster(unsigned int iclus)
{
  TClonesArray &emcclus = *EmcClus;
  new(emcclus[iclus]) EmcSnglClusterLocalExtMicrov4();
  return;
}

int EmcClusterLocalExtMicrov4::get_index(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_index() : -999);
}

void EmcClusterLocalExtMicrov4::set_index(unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_index(ival);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

int EmcClusterLocalExtMicrov4::get_warnmap(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_warnmap() : -999);
}

void EmcClusterLocalExtMicrov4::set_warnmap(unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_warnmap(ival);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

int EmcClusterLocalExtMicrov4::get_deadmap(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_deadmap() : -999);
}

void EmcClusterLocalExtMicrov4::set_deadmap(unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_deadmap(ival);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}


short EmcClusterLocalExtMicrov4::get_arm(const unsigned int iclus) const
{
  
  int key=get_index(iclus);
  return key/100000;
  
}

short EmcClusterLocalExtMicrov4::get_sector(const unsigned int iclus) const
{

  int key=get_index(iclus);
  return (key%100000)/10000;

}

short EmcClusterLocalExtMicrov4::get_ind(const unsigned int iclus, const short i) const
{
  // i==0 corresponds to the X direction in the local EMC sector frame
  // (which coincides with the Z direction in the global PHENIX frame)
  // i==1 corresponds to the Y direction in the local EMC sector fram

  int key=get_index(iclus);

  if(i==0) return key%100;
  else if(i==1) return (key%10000)/100;

  std::cout<<PHWHERE<<"ERROR index "<<i<<"out of range"<<std::endl;
  return -999;

}


short EmcClusterLocalExtMicrov4::get_twrhit(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_twrhit() : -999);
}

void EmcClusterLocalExtMicrov4::set_twrhit(unsigned int iclus, short ival)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_twrhit(ival);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_chi2(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_chi2() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_chi2(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_chi2(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_qual(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_qual() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_qual(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_qual(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_chi2_sh(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_chi2_sh() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_chi2_sh(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_chi2_sh(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_e(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_e() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_e(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_e(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_ecore(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_ecore() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_ecore(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecore(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_ecorr(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_ecorr() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_ecorr(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecorr(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_ecent(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_ecent() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_ecent(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecent(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_e9(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_e9() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_e9(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_e9(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_prob_photon(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_prob_photon() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_prob_photon(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_prob_photon(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_prob_photon_sh(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_prob_photon_sh() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_prob_photon_sh(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_prob_photon_sh(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_re9(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_re9() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_re9(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_re9(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_tofcorr(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_tofcorr() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_tofcorr(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofcorr(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_tofmin(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_tofmin() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_tofmin(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmin(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_tofmax(unsigned int iclus) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_tofmax() : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_tofmax(unsigned int iclus, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmax(rval);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_disp(unsigned int iclus, short i) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_disp(i) : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_disp(unsigned int iclus, short i, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_disp(rval,i);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_padisp(unsigned int iclus, short i) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_padisp(i) : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_padisp(unsigned int iclus, short i, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_padisp(rval,i);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_xyz(unsigned int iclus, short i) const
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_xyz(i) : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_xyz(unsigned int iclus, short i, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_xyz(rval,i);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

float EmcClusterLocalExtMicrov4::get_yz_cg(unsigned int iclus, short i) const
{

  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  return((emcclus) ? emcclus->get_yz_cg(i) : -9999.9);
}

void EmcClusterLocalExtMicrov4::set_yz_cg(unsigned int iclus, short i, float rval)
{
  EmcSnglClusterLocalExtMicrov4 *emcclus = (EmcSnglClusterLocalExtMicrov4 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_yz_cg(rval,i);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtMicrov4 object found" << std::endl;
    }
  return;
}

