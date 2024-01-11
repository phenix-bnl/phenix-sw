#include "HbdCrudeDisplay.h"
#include <iostream>
#include "TGraph.h"
#include "TText.h"
#include "TStyle.h"
using namespace std;

static map<int,float> pad_charge;
HbdCrudeDisplay::HbdCrudeDisplay() : TH2F("HbdCrudeDisplay","HbdCrudeDisplay",30,0.5,15.5,10,0.5,5.5)
{
  
  for (int i=0; i<HBD_PADS; i++)
    {
      pad_colors[i]=1;
    }

  dispd=0;

   float xg[4],yg[4];
   xg[0]=-0.50;
   xg[1]=9;
   xg[2]=9;
   xg[3]=-0.5;
   yg[0]=0.50;
   yg[1]=0.50;
   yg[2]=-7;
   yg[3]=-7;

  g1 = new TGraph(4,xg,yg); 
  g1->GetXaxis()->SetLabelSize(0);
  g1->GetYaxis()->SetLabelSize(0);

  ScaleMax = 2048;
  ScaleMin = 1;
  LogY     = 0;
  AutoScale= 0;

}
 
void HbdCrudeDisplay::SetCharge(unsigned int PadNumber, float Charge)
{
   pad_charge[PadNumber]=Charge;
}

void HbdCrudeDisplay::draw_hexagons(){

   gStyle->SetOptTitle(0);
   set_colors();
   float x[7],y[7];
   
  if(dispd){
    delete tt;
    for(int i=0; i<HBD_ROWS; i++){
      for(int j=0; j<HBD_COLS; j++){
	if(((j+1)%2==0)&& (i+1)>4)continue; //only 4 pads in even cols
	int padnumber = get_padnumber(i+1,j+1);
	if(padnumber>68 || padnumber<1)continue; //not a real pad
//	delete pads[padnumber-1];
      }
    }
  } 


  int k=0;
  float s=0.5, rowoffset, coloffset;

  g1->Draw("AP");

  for(int i=0; i<HBD_ROWS; i++){
     for(int j=0; j<HBD_COLS; j++){
	if(((j+1)%2==0)&& (i+1)>4)continue; //only 4 pads in even cols
	int padnumber = get_padnumber(i+1,j+1);
	if(padnumber>68 || padnumber<1)continue; //not a real pad
	rowoffset=3.0*s;
	if(j%2==0)coloffset=0;
	else coloffset=1.5*s;
	y[0]=-1.0*(i*rowoffset + coloffset);
	y[1]=-1.0*(s+i*rowoffset + coloffset);
	y[2]=-1.0*(1.5*s+i*rowoffset + coloffset);
	y[3]=-1.0*(s+i*rowoffset + coloffset);
	y[4]=-1.0*(i*rowoffset + coloffset);
	y[5]=-1.0*(-0.5*s+i*rowoffset + coloffset);
	y[6]=-1.0*(i*rowoffset + coloffset);
	x[0]=2.*s*j - 0.25*sqrt(3.0)*j;
	x[1]=s*2.*j - 0.25*sqrt(3.0)*j;
	x[2]=1.73*s/2. + 2.*s*j - 0.25*sqrt(3.0)*j;
	x[3]=1.73*s + 2.*s*j - 0.25*sqrt(3.0)*j;
	x[4]=1.73*s + 2.*s*j - 0.25*sqrt(3.0)*j;
	x[5]=1.73*s/2. + 2.*s*j - 0.25*sqrt(3.0)*j;
	x[6]=2.*s*j - 0.25*sqrt(3.0)*j;
	if(dispd==0){
	pads[padnumber-1] = new TPolyLine(7,x,y,"F");
	pads[padnumber-1]->SetFillStyle(1001);
	}
	pads[padnumber-1]->SetFillColor(pad_colors[padnumber-1]);
	pads[padnumber-1]->Draw();
	char name[10];
	sprintf(name,"  %d",padnumber);
	tt = new TText(x[1],y[3],name);
	tt->SetTextColor(0);
	tt->Draw();
	k++;
     }
  }

  dispd=1;

}
int HbdCrudeDisplay::get_padnumber(int row, int col){

   //odd columns have 5 pads and even ones have 4
   if(row>5 || row<1)return 0;
   if(col>15 || col<1)return 0;
   int even, offset;
   int odd = col%2;
   if(odd) even=0;
   else even =1;
   if(col%2) offset = (col-1)/2;
   else offset = col/2 -1;
   int padnumber = 9*offset + 5*even + row;
   return padnumber;
}

int HbdCrudeDisplay::get_row(int padnumber){
   int row = padnumber%9;
   if(row>5)row=row-5;
   if(row==0)row=4;
   return row;
}

int HbdCrudeDisplay::get_col(int padnumber){
   int pair = (int)floor(padnumber/9.0);
   int row = padnumber%9;
   int col;
   if(row>0&&row<6)col = pair*2+1;//odd rows
   else col= pair*2 + 2;//even rows
   return col;
}
   
void HbdCrudeDisplay::set_colors(){
  //
  //  Modified to allow fixed & log scales.
  //
  //                    TKH 5-26-2006
  //

  float max_charge=0;
  if (AutoScale)
    {
      map<int,float>::iterator ci;
      for(ci=pad_charge.begin(); ci!=pad_charge.end(); ++ci)
	{
	  if(ci->second > max_charge)max_charge = ci->second;
	}
    }
  else
    {
      max_charge = ScaleMax;
    }
   

   for(int i=0; i<HBD_PADS;i++)
     {
       if(pad_charge[i+1]>ScaleMin)
	 {
	   if (LogY)
	     {
	       pad_colors[i]= (int)(50 + 50*log(pad_charge[i+1]/ScaleMin)/log(ScaleMax/ScaleMin));
	     }
	   else
	     {
	       pad_colors[i]=50 + (int)floor(50*(pad_charge[i+1]-ScaleMin)/max_charge);
	     }
	 }
       else
	 {
	   pad_colors[i]=1;
	 }
     }
}
      
