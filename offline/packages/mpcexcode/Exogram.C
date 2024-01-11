#include <Exogram.h>
#include <TH2D.h>
#include "MpcExMapper.h"
#include "MpcExConstants.h"
#include "phool.h"
#include <iostream>
#include <string>
#include <sstream>
#include <cmath>
#include <algorithm>

using namespace std;

ClassImp(Exogram)

Exogram::Exogram() : TH3D()
{
}

Exogram::Exogram(const char* name, const char* title, 
		 Int_t nbinsx, const Float_t* xbins, 
		 Int_t nbinsy, const Float_t* ybins, 
		 Int_t nbinsz, const Float_t* zbins, bool P):
  TH3D(name, title, nbinsx, xbins, nbinsy, ybins, nbinsz, zbins)
{
  _MapTheBins(P);  
}


Exogram::Exogram(const char* name, const char* title, 
		 Int_t nbinsx, const Double_t* xbins, 
		 Int_t nbinsy, const Double_t* ybins, 
		 Int_t nbinsz, const Double_t* zbins, bool P):
  TH3D(name, title, nbinsx, xbins, nbinsy, ybins, nbinsz, zbins)
{
  _MapTheBins(P);
}

Exogram::Exogram(const char* name, const char* title, 
		 Int_t nbinsx, Double_t xlow, Double_t xup, 
		 Int_t nbinsy, Double_t ylow, Double_t yup, 
		 Int_t nbinsz, Double_t zlow, Double_t zup, bool P):
  TH3D(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup, nbinsz, zlow, zup)
{
  _MapTheBins(P);
}


void Exogram::FillEx(unsigned int key, Double_t w)
{
  if(_FillingMap.find(key) == _FillingMap.end())
    {
      cout<<PHWHERE<<key<<" is not a valid key in the Exogram map."<<endl;
      return;
    }
  
  vector<Int_t> cells = _FillingMap[key];

  for (unsigned int i=0; i<cells.size(); i++)
    {
      AddBinContent(cells[i],w);
    }
}

void Exogram::Discretize()
{
  //  This routine is just for testing and playing around.
  //  It should not be used with real data.
  //
  //  This routine assumes that one has used the normal histogram
  //  fill methods to insert fake showers with higher resolution
  //  than the device pixels.  It then opens the histogram,
  //  collects all the pixels within one readout, and replaces
  //  all the pixels with the summed value.
  
  
  for ( std::map<unsigned int,std::vector<Int_t> >::iterator iter = _FillingMap.begin(); iter != _FillingMap.end(); ++iter )
    {
      vector<Int_t> cells = iter->second;
      
      double sum=0;
      for (unsigned int i=0; i<cells.size(); i++)
	{
	  sum += GetBinContent(cells[i]);
	}
      for (unsigned int i=0; i<cells.size(); i++)
	{
	  SetBinContent(cells[i],sum);
	}
    }
}


void Exogram::DrawLayer(int layer, Option_t* option)
{
  TH2D *temp = GetLayer(layer);
  temp->Draw(option);
}

TH2D* Exogram::GetLayer(int layer){
  SetAxisRange(layer,layer,"Z");
  TH2D* temp = (TH2D*)Project3D("yx");
  std::ostringstream temp_name;
  temp_name << "proj_xy_" << layer;
  temp->SetName(temp_name.str().c_str());
  return temp;
}

TH2D* Exogram::MultiplyLayers(int nlayers)
{
  if( nlayers==0 ) nlayers = GetNbinsZ();
  SetAxisRange(0,0,"Z");
  TH2D* mult = (TH2D*)Project3D("yx");
  mult->SetName("mult");
  _SetMultiplyBinContent(mult);
  for( int il = 1; il < nlayers; il++ )
  {
    SetAxisRange(il,il,"Z");
    TH2D* temp = (TH2D*)Project3D("yx");
    _SetMultiplyBinContent(temp);
    mult->Multiply(temp);
  }
  SetAxisRange(0,GetNbinsZ(),"Z");
  return mult;
}

void Exogram::_SetMultiplyBinContent(TH2D* histo)
{
  for( int ix = 1; ix <= histo->GetNbinsX(); ix++ )
  {
    for( int iy = 1; iy <= histo->GetNbinsY(); iy++ )
    {
      double bin_content = histo->GetBinContent(ix,iy);
      bin_content++;
      histo->SetBinContent(ix,iy,bin_content);
    }
  }
}


void Exogram::_MapTheBins(bool Projective)
{
  //  This is a brute force analysis to define the mapping.
  //  We will perform the following steps:
  //    *  Divide the sensor itself into sub-pieces 
  //       whose size matches the histogram bin width.
  //    *  Loop over EVERY such sub piece.
  //    *  Record the bin index that matches the sub-piece.
  //    *  Remove repeats from the list of bin indices.
  //    *  Store the list in the _FillingMap<>.
  //
  //
  //  The bool argument decides whether to make the 
  //  Exogram space projective.  Projective space means
  //  that the Exogram is essentially a hough transform.
  //

  _FillingMap.clear();

  vector<Int_t> bins;

  MpcExMapper *mapper = MpcExMapper::instance();

  for (unsigned int a=0; a<MpcExConstants::NARMS; a++)
    {
      // New Zref for each arm...
      unsigned int test_key = mapper->generate_key(a,0,0);
      double Zref = mapper->get_z(test_key);

      for (unsigned int p=0; p<MpcExConstants::NPACKETS_PER_ARM; p++)
	{
	  for (unsigned int c=0; c<MpcExConstants::NMINIPADS_PER_PACKET; c++)
	    {

	      unsigned int key = mapper->generate_key(a,p,c);
	      double dx = GetXaxis()->GetBinWidth(5);
	      double dy = GetYaxis()->GetBinWidth(5);
	      double DX = mapper->get_minipad_x_width(key);
	      double DY = mapper->get_minipad_y_width(key);
	      double X = mapper->get_x(key);
	      double Y = mapper->get_y(key);
	      double Z = mapper->get_z(key);

	      double xmin = X-DX/2.0;
	      double xmax = X+DX/2.0;
	      double ymin = Y-DY/2.0;
	      double ymax = Y+DY/2.0;

	      // Calculate scale for Projective option...
	      unsigned short l = mapper->get_layer(key);
	      double scale = std::abs(Zref/Z);
	      if( scale < 0.9 ) {
		cout << "Scale error! Scale = " << scale << endl;
	      }

	      bins.clear();
	      for (double x=xmin+dx/2.0; x<xmax-dx/2.0; x+=dx)
		{
		  for (double y=ymin+dy/2.0; y<ymax-dy/2.0; y+=dy)
		    {
		      double xp = x;
		      double yp = y;
		      if (Projective)
			{
			  xp *= scale;
			  yp *= scale;
			}
		      Int_t TheBin = FindBin(xp,yp,l);
		      bins.push_back(TheBin);
		    }
		}

	      //  Quick way to remove any duplicate entries...
	      std::sort(bins.begin(), bins.end());
	      bins.erase(std::unique(bins.begin(), bins.end()), bins.end());
	      _FillingMap[key]=bins;
	    }
	}
    }
}

