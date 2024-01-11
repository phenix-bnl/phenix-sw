#include <utiMemoryWatcher.h>
#include <TSystem.h>
#include <iostream>
#include <TGraph.h>
#include <TH2.h>
#include <TStopwatch.h>

#include <cassert>
using namespace std;

//_____________________________________________________________________________
utiMemoryWatcher::utiMemoryWatcher(unsigned int maxsize)
{
  fMAXSIZE=maxsize;
  fPID = gSystem->GetPid();
  sprintf(fCmd,"ps -h -p %d -o vsize,rssize",fPID);

  fX = new int[fMAXSIZE];
  fVSIZE = new int[fMAXSIZE];
  fRSSIZE = new int[fMAXSIZE];
  fTIME = new double[fMAXSIZE];
  fSize=0;
  fDisabled=false;
  fTimer=0;
}

//_____________________________________________________________________________
utiMemoryWatcher::~utiMemoryWatcher()
{
  delete[] fVSIZE;
  delete[] fRSSIZE;
  delete[] fX;
  delete[] fTIME;
  delete fTimer;
}

//_____________________________________________________________________________
void utiMemoryWatcher::watch(int x)
{
  if ( !fDisabled && fSize < fMAXSIZE ) {

    if ( fSize==0 ) {
      assert(fTimer==0);
      fTimer = new TStopwatch;
      fTimer->Start(true);
      fTimer->Stop();
    }

    static int vsize, rssize;

    static FILE* pipe = 0;

    pipe = popen(fCmd,"r");

    if ( pipe ) {
    
      fscanf(pipe,"%d %d",&vsize,&rssize);
    }

    int err = pclose(pipe);

    if ( err != -1 )
      {
	fX[fSize] = x ;
	fVSIZE[fSize] = vsize ;
	fRSSIZE[fSize] = rssize ;
	fTIME[fSize] = fTimer->CpuTime();
	fSize++;
      }
    else
      {
	std::cerr << __FILE__ << ":" << __LINE__ 
		  << " pclose failed" << std::endl;
      }

    fTimer->Start(true);

  }
  else {
    fDisabled=true;
    cerr << "utiMemoryWatcher::watch : I'm full !" << endl;
  }
}

//_____________________________________________________________________________
TGraph*
utiMemoryWatcher::graphVSIZE(void)
{
  TGraph* g = 0;

  if ( size() )
    {
      g = new TGraph(size());
      for (int i=0; i < g->GetN(); i++ ) {
	g->SetPoint(i,X(i),VSIZE(i));
      }
    }
  return g;
}

//_____________________________________________________________________________
TGraph*
utiMemoryWatcher::graphRSSIZE(void)
{
  TGraph* g = 0;
  if ( size() ) 
    {
      g = new TGraph(size());
      for (int i=0; i < g->GetN(); i++ ) {
	g->SetPoint(i,X(i),RSSIZE(i));
      }
    }
  return g;
}

//_____________________________________________________________________________
TGraph*
utiMemoryWatcher::graphTIME(void)
{
  TGraph* g = 0;
  if ( size() ) 
    {
      g = new TGraph(size());
      for (int i=0; i < g->GetN(); i++ ) {
	g->SetPoint(i,X(i),TIME(i));
      }
    }
  return g;
}

//_____________________________________________________________________________
TH2*
utiMemoryWatcher::frame(void)
{
  double xmin=1E30;
  double xmax=0;
  double ymin=1;
  double ymax=0;

  for (unsigned int i=0; i < size() ; i++ ) {
    if ( X(i) < xmin ) xmin = X(i);
    if ( X(i) > xmax ) xmax = X(i);

    double y = VSIZE(i)+RSSIZE(i);

    if ( y > ymax ) ymax = y;

    if ( VSIZE(i) < ymin ) ymin = VSIZE(i);
    if ( RSSIZE(i) < ymin ) ymin = RSSIZE(i);
  }

  TH2F* h = new TH2F("frame","",10,xmin,xmax,10,ymin*0.8,ymax*1.2);

  return h;
}

//_____________________________________________________________________________
void 
utiMemoryWatcher::write(void)
{
  if ( graphVSIZE() ) graphVSIZE()->Write("VSIZE",TObject::kOverwrite);
  if ( graphRSSIZE() ) graphRSSIZE()->Write("RSSIZE",TObject::kOverwrite);
  if ( graphTIME() ) graphTIME()->Write("TIME",TObject::kOverwrite);
}
