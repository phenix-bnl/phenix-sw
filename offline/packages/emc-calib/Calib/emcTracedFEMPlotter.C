// $Id: emcTracedFEMPlotter.C,v 1.6 2004/06/23 10:31:01 aphecetc Exp $

#include "emcDefines.h"
#include "emcTracedFEMPlotter.h"
#include "emcTracedFEM.h"
#include "emcTracedValue.h"
#include "TGraph.h"
#include <vector>

using namespace std;

//_____________________________________________________________________________
emcTracedFEMPlotter::emcTracedFEMPlotter(const emcTracedFEM& tf,
					 time_t tics0)
  : fTracedFEM(tf.clone()), fTics0(tics0)
{
  fGraphs.resize(fTracedFEM->size(), 0) ;
}

//_____________________________________________________________________________
emcTracedFEMPlotter::emcTracedFEMPlotter(const emcTracedFEM& tf,
					 int tics0)
  : fTracedFEM(tf.clone()), fTics0(tics0)
{
  fGraphs.resize(fTracedFEM->size(), 0) ;
}

//_____________________________________________________________________________
emcTracedFEMPlotter::emcTracedFEMPlotter(const emcTracedFEM& tf)
  : fTracedFEM(tf.clone()), fTics0(tf.GetXmin())
{
  fGraphs.resize(fTracedFEM->size(), 0) ;
}

//_____________________________________________________________________________
emcTracedFEMPlotter::~emcTracedFEMPlotter()
{
  size_t i ;
  for ( i = 0 ; i < fGraphs.size() ; ++i )
    {
      delete fGraphs[i] ;
    }
}

//_____________________________________________________________________________
TGraph*
emcTracedFEMPlotter::CopyGraph(int ichannel)
{
  if ( !ValidChannel(ichannel) )
    return 0 ;

  if ( !fGraphs[ichannel] )
    {
      MakeGraph(ichannel) ;
    }

  return static_cast<TGraph*>(fGraphs[ichannel]->Clone());
}


//_____________________________________________________________________________
bool
emcTracedFEMPlotter::Draw(int ichannel, const char* option)
{
  if ( !ValidChannel(ichannel) )
    return false ;

  if ( !fGraphs[ichannel] )
    {
      MakeGraph(ichannel) ;
    }

  fGraphs[ichannel]->Draw(option) ;

  return true ;
}

//_____________________________________________________________________________
void
emcTracedFEMPlotter::MakeGraph(int ichannel)
{
  vector<double> x;
  vector<double> y;

  fTracedFEM->FirstItem(ichannel);

  x.push_back(0);

  emcTracedValue* tv1 = fTracedFEM->NextItem();

  y.push_back(tv1->GetConstant());

  emcTracedValue* tv2 = 0;

  while ( ( tv2 = fTracedFEM->NextItem() ) )
    {
      x.push_back(tv1->GetX());
      y.push_back(tv1->GetConstant());
      x.push_back(tv2->GetX());
      y.push_back(tv1->getValue(tv2->GetX()));
      tv1 = tv2;
    }

  x.push_back(tv1->GetX());
  y.push_back(tv1->GetConstant());

  int xrelmax = static_cast<int>(fTracedFEM->GetXmax()-fTracedFEM->GetXmin());

  x.push_back(xrelmax);
  y.push_back(tv1->getValue(xrelmax));

#ifdef DEBUG  
  for ( size_t i = 0; i < x.size(); ++i )
    {
      x[i] += fTics0;
      cout << "Point(" << i << ")=(" << x[i] << "," << y[i] << ")"
	   << endl;
    }
#endif

  fGraphs[ichannel] = new TGraph(x.size(),&x[0],&y[0]) ;
}

//_____________________________________________________________________________
bool
emcTracedFEMPlotter::ValidChannel(int ichannel) const
{
  if ( ichannel < 0 || ichannel >= static_cast<int>(fGraphs.size()) )
    {
      return false ;
    }
  else
    {
      return true ;
    }
}

//_____________________________________________________________________________
bool
emcTracedFEMPlotter::Write(int ichannel, const char* name)
{
  if ( !ValidChannel(ichannel) )
    return false ;

  if ( !fGraphs[ichannel] )
    {
      MakeGraph(ichannel) ;
    }

  fGraphs[ichannel]->Write(name) ;

  return true ;
}
