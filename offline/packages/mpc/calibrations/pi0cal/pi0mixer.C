#include "pi0mixer.h"
#include "MpcPi0Cal.h"
#include "MpcPi0Mass.h"
#include <phool.h>
#include <TH1.h>
#include <TH1F.h>
#include <TMath.h>
#include <THmulf.h>


using namespace std;


pi0mixer::pi0mixer(int maxpooldepth, int type, int verbosity)
{
  kcurrentptcls       = 0;
  kcurrentpooldepth   = 0;
  kmaxpooldepth       = maxpooldepth;
  kverbosity          = verbosity;
  koldest             = 0; 
  ktype               = type;
}
void pi0mixer::setverbosity(int verbosity)
{
  kverbosity = verbosity;
}


int pi0mixer::setpooldepth(int depth)
{
  if(depth > skmaxpooldepth || depth < 0 )
    {cout << PHWHERE << "Incorrect pool depth\n"; return 0;}
  kmaxpooldepth = depth;
  return 1;
}


int pi0mixer::addevent()
{
  if(kcurrentptcls <= 0 || kcurrentptcls > skmaxptcls)
    {cout << PHWHERE << "Ptcl Array has strange # of ptcls; not adding event\n"; return 0;}
  
  else if(kmaxpooldepth > skmaxpooldepth || kmaxpooldepth < 0 )
    {cout << PHWHERE << "Incorrect max pool depth; not adding event\n"; return 0;}

  else if(kcurrentpooldepth >= kmaxpooldepth || kcurrentpooldepth < 0 )
    {cout << PHWHERE << "Pool is probably full...not adding; use clear\n"; return 0;}


  for(int iptcl=0;iptcl<kcurrentptcls;iptcl++)
    {
      event_pool[kcurrentpooldepth][iptcl] = pi0base(event[iptcl]);
      knumptcls[kcurrentpooldepth] = kcurrentptcls;
    }
  kcurrentptcls = 0;
  kcurrentpooldepth++;
  return 1;
      
}

int pi0mixer::pushevent()
{
  if(kcurrentptcls <= 0 || kcurrentptcls > skmaxptcls)
    {cout << PHWHERE << "Ptcl Array has strange # of ptcls; not adding event\n"; return 0;}
  
  else if(kmaxpooldepth > skmaxpooldepth || kmaxpooldepth < 0 )
    {cout << PHWHERE << "Incorrect max pool depth; not adding event\n"; return 0;}

  else if(kcurrentpooldepth > kmaxpooldepth || kcurrentpooldepth < 0 )
    {cout << PHWHERE << "Incorrect current pool depth...not adding; use clear\n"; return 0;}
  
  for(int iptcl=0;iptcl<kcurrentptcls;iptcl++)
    {
      event_pool[koldest][iptcl] = pi0base(event[iptcl]);
      knumptcls[koldest] = kcurrentptcls;
    }
  
  koldest++;
  if(koldest == kmaxpooldepth) koldest = 0;  //this makes the additon of new particles cyclic
  
  kcurrentptcls = 0;
  if(kcurrentpooldepth<kmaxpooldepth) kcurrentpooldepth++;
  
  return 1;
  
}


int pi0mixer::addptcl(pi0base *ptcl)
{
  if( kcurrentptcls >= skmaxptcls || kcurrentptcls < 0) 
    {cout << PHWHERE << "PtclArray is full...not adding\n"; return 0;}

  event[kcurrentptcls] = pi0base(*ptcl);
  kcurrentptcls++;
  return 1;
}


void pi0mixer::clear()
{
  kcurrentptcls     = 0;
  kcurrentpooldepth  = 0;
}

int pi0mixer::isfull()
{
  if(kcurrentpooldepth == kmaxpooldepth)
    return 1;
  return 0;
}

int pi0mixer::isempty()
{
  if(kcurrentpooldepth > 0)
    return 0;
  return 1;
}


int pi0mixer::getpooldepth()
{
  return kcurrentpooldepth;
}

int pi0mixer::getnumptcls(int ev)
{
  if(ev >= kcurrentpooldepth)
    {cout << PHWHERE << "Event index too large...there aren't this many events on the pool...returning 0\n"; return 0;}
  
  return knumptcls[ev];
}


void pi0mixer::printtest()
{
  cout << "skmaxptcls = "          << skmaxptcls << endl
       << "skmaxpooldepth = "      << skmaxpooldepth << endl
       << "kcurrentptcls = "       << kcurrentptcls << endl
       << "kcurrentpooldepth = "   << kcurrentpooldepth << endl
       << "kmaxpooldepth = "       << kmaxpooldepth << endl
       << "koldest = "       << koldest << endl;
}



//-----------now we mix---------------------------


const pi0base* pi0mixer::getelement(int iev, int iptcl)
{
  //do this first to make sure we have a good array index for the second if
  if(iev >= kcurrentpooldepth)
    {cout << PHWHERE << "Event index too large...there aren't this many events on the pool...returning null\n"; return 0;}

  if(iptcl >= knumptcls[iev])
    {cout << PHWHERE << "Particle index too large...not this many particles in this event..returning null\n"; return 0;}
  
  return &event_pool[iev][iptcl];
}




int pi0mixer::mixmass( TH1* h )
{
  //just loop through one pi0mixer object and create bkgd pi0 spectrum
  int numevents = 0;
  for( int ieva = 0; ieva< this->getpooldepth(); ieva++)
    {
      numevents+= mixmassevents( ieva,h );
    }
  return numevents;
  
}

int pi0mixer::mixmassevents( int eva, TH1* h )
{
  if(eva >= this->getpooldepth())
    {cout << PHWHERE << "An event index is too large...there aren't this many events on the pool...not mixing\n"; return 0;}
  
  //cout << "eventa = " << eva << ", " << "eventb = " << evb << endl;
  int numevents = 0;
  for(int aptcl=0;aptcl< this->getnumptcls(eva); aptcl++)
    {
      const pi0base *pha = this->getelement(eva, aptcl);
      TLorentzVector va = TLorentzVector(pha->px,pha->py,pha->pz,pha->e);
      for(int bptcl=0;bptcl< kcurrentptcls; bptcl++)
	{
	  const pi0base *phb = &event[bptcl];
	  if(pha->event == phb->event && pha->run == phb->run) return 0;
	  TLorentzVector vb = TLorentzVector(phb->px,phb->py,phb->pz,phb->e);
	  TLorentzVector vtot = va + vb;
	  float mass = sqrt((vtot)*(vtot));
	  //float energy = pha->e + phb->e;
	  //float pt = vtot.Perp();
	  //float r = sqrt((float)pow( (float)(pha->ia - phb->ia), 2 ) + (float)pow( (float)(pha->ib - phb->ib), 2 ) );
	  //cout << r << endl;
	  //float sep = sqrt((float)pow( (float)(pha->a - phb->a), 2 ) + (float)pow( (float)(pha->b - phb->b), 2 ) );
	  
	  bool goodpair = false;
	  if( MpcPi0Cal::passedMpcPionCuts(pha,phb) && fabs(pha->zvtx - phb->zvtx) < 60 )
	    goodpair = true;
		  
	  if(goodpair)
	    {
			    
	      h->Fill(mass);
	      numevents++;
	    }
	}
    }
  return numevents;//(a->getnumptcls(eva)*b->getnumptcls(evb));
}

int pi0mixer::mixmass(  MpcPi0MassContainer* h )
{
  //just loop through one pi0mixer object and create bkgd pi0 spectrum
  int numevents = 0;
  for( int ieva = 0; ieva< this->getpooldepth(); ieva++)
    {
      numevents+= mixmassevents( ieva,h );
    }
  return numevents;
  
}

int pi0mixer::mixmassevents( int eva, MpcPi0MassContainer* h )
{
  if(eva >= this->getpooldepth())
    {cout << PHWHERE << "An event index is too large...there aren't this many events on the pool...not mixing\n"; return 0;}
  
  //cout << "eventa = " << eva << ", " << "eventb = " << evb << endl;
  int numevents = 0;
  for(int aptcl=0;aptcl< this->getnumptcls(eva); aptcl++)
    {
      const pi0base *pha = this->getelement(eva, aptcl);
      TLorentzVector va = TLorentzVector(pha->px,pha->py,pha->pz,pha->e);
      for(int bptcl=0;bptcl< kcurrentptcls; bptcl++)
	{
	  const pi0base *phb = &event[bptcl];
	  if(pha->event == phb->event && pha->run == phb->run) return 0;
	  TLorentzVector vb = TLorentzVector(phb->px,phb->py,phb->pz,phb->e);
	  TLorentzVector vtot = va + vb;
	  float mass = sqrt((vtot)*(vtot));
	  //float energy = pha->e + phb->e;
	  float pt = vtot.Perp();
	  float ptot = vtot.P();
	  //float r = sqrt((float)pow( (float)(pha->ia - phb->ia), 2 ) + (float)pow( (float)(pha->ib - phb->ib), 2 ) );
	  //cout << r << endl;
	  //float sep = sqrt((float)pow( (float)(pha->a - phb->a), 2 ) + (float)pow( (float)(pha->b - phb->b), 2 ) );
	  
	  bool goodpair = false;
	  if( MpcPi0Cal::passedMpcPionCuts(pha,phb) && fabs(pha->zvtx - phb->zvtx) < 60 )
	    goodpair = true;
	  //std::cout << "do we have a good pair? : " << goodpair << std::endl;
	  if(goodpair)
	    {
	      //  std::cout << "filling mixing histos: 1\n";
	      if(!phb->frozen)
		h->m[ pha->arm ][ pha->ia ][ pha->ib ]->FillMassBkgd(mass, pt, ptot, pha->e );
	      //std::cout << "filling mixing histos: 2\n";
	      if(!pha->frozen)
		h->m[ phb->arm ][ phb->ia ][ phb->ib ]->FillMassBkgd(mass, pt, ptot, phb->e );
	      //std::cout << "filling mixing histos: 1\n";
	      //h[ tarm[iclus2] ][ tixpos[iclus2] ][ tiypos[iclus2] ]->FillMass(mass_pi0, pt_pi0, p_pi0, e_2 );	    
	      //h->Fill(mass);
	      numevents++;
	    }
	}
    }
  return numevents;//(a->getnumptcls(eva)*b->getnumptcls(evb));
}



//------------------------------------V2 ADDITIONS END HERE------------------------


void pi0mixer::mixtester()
{
  
}
