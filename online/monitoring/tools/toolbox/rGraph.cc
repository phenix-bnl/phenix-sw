#include "rGraph.h"
#include "TH1.h"

rGraph::rGraph(const int n, const int bins)
{
  multiplex = 1;
  current_bin = 0;
  
  NumberofChannels =n;
  array = new float[NumberofChannels];
  Reset();
}

rGraph::~rGraph()
{
  delete [] array;
}


int rGraph::Reset()
{
  int i;
  for (i=0; i< NumberofChannels; i++) array[i] = 0;
  current_channel = -1;
  return 0;
}


int rGraph::Add (const int k)
{
  int j;
  if ( current_channel < NumberofChannels -1) 
    {
      current_channel++;
      array[current_channel] =  k;
    }
  else
    {
      for (j=1; j< NumberofChannels; j++) array[j-1] = array[j];
      array[ NumberofChannels -1] = k;
    }
  return 0;
}


int rGraph::Add (const float f)
{
  int i;
  if ( current_channel < NumberofChannels -1) 
    {
      current_channel++;
      array[current_channel] = f;
    }
  else
    {
      for (i=1; i< NumberofChannels; i++) array[i-1] = array[i];
      array[ NumberofChannels -1] = f;
    }
  return 0;
}
     

int rGraph::Fill (TH1 * h) const
{
  int i; 
  for (i=0; i<  NumberofChannels; i++) 
    {
      h->SetBinContent(i+1, array[i]);
    }
  return 0;
}
