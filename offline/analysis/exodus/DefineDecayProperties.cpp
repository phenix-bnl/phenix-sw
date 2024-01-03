//-----------------------------------------------------------------------------
//
//  Read decay properties from file
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <memory>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include "CreateDalitzLeptonPairMass.h"
#include "DecayList.h"
#include "DefineDecayProperties.h"

class TH1F;

DecayList* DefineDecayProperties(const ParticlePropertyList& PPList,
    const std::string& decayfile)
{
  std::auto_ptr<DecayList> DefinedDecays(new DecayList);

  std::cout << "Reading particle decay definitions from file: " << decayfile
    << std::endl;

  std::ifstream fin(decayfile.c_str());
  char ch;
  while ( fin.get(ch) )
  {
    double branchingratio = 0;
    int id = 0;
    int nbody = 0;
    int parentid = 0;
    int childid[5] = {0};
    bool enabled = false;
    bool children_stable = false;
    bool dalitz = false;
    std::string label, name;

    int i=0;
    fin >> label;
    if ( label=="@{" )
    {
      fin >> name;
      while (true)
      {
        fin >> label;
        if ( label=="DecayId=" )        fin >> id;
        if ( label=="NBody=" )          fin >> nbody;
        if ( label=="BranchingRatio=" ) fin >> branchingratio;
        if ( label=="ParentID=" )       fin >> parentid;
        if ( label=="ChildID=" )
        {
          i++;
          fin >> childid[i];
        }
        if ( label=="Enabled=" )        fin >> enabled;
        if ( label=="ChildrenStable=" ) fin >> children_stable;
        if ( label=="}" )               break;
      }
    }
    else continue;
    if ( nbody!=2 && nbody!=3 && nbody!=4 )
    {
      std::cout << "Only 2 and 3 body decays are implemented at the moment."
        << std::endl
        << "Check the file '"<< decayfile <<"' for invalid decays."
        << std::endl;
      return 0;
    }
    if ( !enabled ) continue;

    Decay *pDec = new Decay;

    pDec->SetID(id);
    pDec->SetNBody(nbody);
    pDec->SetBranchingRatio(branchingratio);
    pDec->SetParentID(parentid);
    for ( int j=1; j<=nbody; j++)
      pDec->SetChildID(j,childid[j]);
    pDec->SetChildrenStable(children_stable);
    DefinedDecays->Insert(pDec);

    dalitz = false;
    if ( nbody==3 )
    {
      bool lepton_n = false;
      bool lepton_p = false;
      for ( int j=1; j<=3; j++ )
      {
        if ( childid[j]==-11 || childid[j]==-13 ) lepton_p = true;
        if ( childid[j]==11  || childid[j]==13 )  lepton_n = true;
      }
      if ( lepton_p && lepton_n ) dalitz = true;
    }

    if ( nbody==4
        && std::abs(childid[1])==11 && std::abs(childid[2])==11
        && std::abs(childid[3])==11 && std::abs(childid[4])==11 )
      dalitz = true;

    if ( dalitz )
    {
      TH1F* hdalitz = CreateDalitzLeptonPairMass(*pDec, PPList);
      pDec->SetHistogram(hdalitz);
    }
    else
    {
      pDec->SetHistogram(0);
    }

    std::cout << "Defined: " << name << std::endl;
  }
  fin.close();

  std::cout << std::endl;

  double pid;
  double br;
  const int MAX_PARTICLE_TYPES = 20;
  double brsum_array[MAX_PARTICLE_TYPES][2];
  for ( int i=1; i<=MAX_PARTICLE_TYPES; i++ )
  {
    brsum_array[i-1][0] = 0.0;
    brsum_array[i-1][1] = 0.0;
  }

  for ( int i=1; i<=DefinedDecays->GetSize(); i++ )
  {
    Decay* CurrentDecay = DefinedDecays->Get(i);
    pid = CurrentDecay->GetParentID();
    br = CurrentDecay->GetBranchingRatio();
    for ( int j=1; j<=MAX_PARTICLE_TYPES; j++ )
    {
      if ( std::abs(brsum_array[j-1][0]-pid) < std::numeric_limits<double>::epsilon() )
      {
        brsum_array[j-1][1]+=br;
        break;
      }
      if ( std::abs(brsum_array[j-1][0]) < std::numeric_limits<double>::epsilon() )
      {
        brsum_array[j-1][0] = pid;
        brsum_array[j-1][1] = br;
        break;
      }
    }
  }

  for ( int i=1; i<=DefinedDecays->GetSize(); i++ )
  {
    Decay* CurrentDecay = DefinedDecays->Get(i);
    pid = CurrentDecay->GetParentID();
    for ( int j=1; j<=MAX_PARTICLE_TYPES; j++ )
    {
      if ( std::abs(pid-brsum_array[j-1][0]) < std::numeric_limits<double>::epsilon() )
      {
        CurrentDecay->SetBRSum(brsum_array[j-1][1]);
        break;
      }
    }
  }

  return DefinedDecays.release();

}
