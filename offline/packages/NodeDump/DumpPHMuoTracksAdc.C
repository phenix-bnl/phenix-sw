#include "DumpPHMuoTracksAdc.h"

#include <PHMuoTracksAdc.h>
#include <PHMuoTrackAdc.h>

#include <PHIODataNode.h>

#include <boost/foreach.hpp>

#include <string>

using namespace std;

typedef PHIODataNode<PHMuoTracksAdc> MyNode_t;

DumpPHMuoTracksAdc::DumpPHMuoTracksAdc(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHMuoTracksAdc::process_Node(PHNode *myNode)
{
  PHMuoTracksAdc *phmuotracks = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phmuotracks = thisNode->getData();
    }
  if (phmuotracks)
    {
      *fout << "GetSize(): " << phmuotracks->GetSize() << endl;
      for (unsigned int i = 0; i < phmuotracks->GetSize(); i++)
        {
	  PHMuoTrackAdc* adc = phmuotracks->Get(i);
	  vector<unsigned char> strip_array;
	  vector <vector<unsigned short> > adc_array;
	  adc->GetAdc(strip_array,adc_array);
	  BOOST_FOREACH(unsigned char a, strip_array)
	    {
	      unsigned short b = a;
	      *fout << "strip: " <<  b << endl;
	    }
	  BOOST_FOREACH(unsigned short a, strip_array)
	    {
	      *fout << "adc: " << a << endl;
	    }
        }
    }
  return 0;
}

