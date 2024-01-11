////////////////////////////////////////////////////////////////////////////// 
// E.Kistenev         8/2/99 
// send comments to kistenev@bnl.gov 
////////////////////////////////////////////////////////////////////////////// 

#include "emcGlobals.h"

#include <TROOT.h>
#include <TSystem.h>
#include <TGClient.h>
#include <Rtypes.h>
#include <TFile.h>
#include <TKey.h>
#include <TH2.h>
#include <TLine.h>
#include <TMath.h>

#include <fstream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <cmath>
#include <ctime>

#include "Event.h"
#include "PHTimeStamp.h"

using namespace std;

//***************************************************************************


Int_t GF::Save(const char * file, 
	       Option_t * option,
	       const char * ftitle, Int_t compress)
{

  cout<<"SAVING into FILE "<<file<<endl;

  if (!strlen(option)) option = "RECREATE";

  TFile *hfile = new TFile(file,option,ftitle,compress); 
  gROOT->cd();
  TList *l = gROOT->GetList();
  TIter next(l);
  //  l->ls();
  TObject *obj;
  while((obj = (TObject*) next())) {
    if (obj->IsA() == TDirectory::Class()) {
      TDirectory * mdir = (TDirectory*)obj;
      //printf("found a dir %s\n",mdir->GetName());
      //gDirectory->pwd();
      char * newdir = new char[strlen(file) + strlen(":/") +1];
      strcpy(newdir,file);
      strcat(newdir,":/");
      //printf("going to cd to %s\n",newdir);
      gDirectory->cd(newdir);  //  change into file directory
      TDirectory * fdir = gDirectory->mkdir(mdir->GetName(),mdir->GetTitle());
      fdir->cd();
      if (fdir->IsWritable()) {
	mdir->GetList()->Write();  //write it into file dir in memory?
	fdir->Write();             //write it to file on dir.
      }
    } else {
      hfile->cd();
      obj->Write();  //Top level objects.
    }
  }
  hfile->Close();
  delete hfile;
  //  now is the real problem - getting rid of what we just created (file directories in memory)

  
  return 0;  //dummy value for now - needs work. MarkP
}

// **********************************************************************

PHTimeStamp * GF::getFileCreationDate(char * fName)
{
  //  //  test for existence of the file
  //   PHTimeStamp * t;
  //   TString command;
  //   command += "test -e ";
  //   command += fName;
  //   if(!system(command.Data())){
  //     //    cout<<"File "<<fName<<" found"<<endl;
  //     command = "ls -l --full-time ";
  //     command += fName;
  //     command += " > _fileAttributes_";
  //     system(command.Data());
  //     ifstream fa;
  //     fa.open("_fileAttributes_",ios::in);
  //     TString d;
  //     char c[200];
  //     for(int i=0; i<5; i++) fa>>c;
  //     while(fa>>c) {d += c; d+=" ";}
  //     t = new PHTimeStamp(0);
  //     //    t->set(((char *)d.Data()));
  //     t->set(d.Data());
  //     //    cout<<*t<<endl;
  //     command = "rm -f ";
  //     command +="_fileAttributes_";
  //     system(command.Data()); 
  //     //    cout<<"Att. file removed"<<endl;
  //     return t;
  //   } else {
  //     //    cout<<"File "<<fName<<" not found"<<endl;
  //     return 0;
  //   }

  long id,size,flags,modtime ;
  int failed = gSystem->GetPathInfo(fName,&id,&size,&flags,&modtime) ;
  if (!failed) {
    return new PHTimeStamp(modtime) ;
  }
  else {
    return 0 ;
  }


}


// **********************************************************************

void GF::getEventTimeStamp(Event * ev, char * fName, PHTimeStamp * & StartTime, PHTimeStamp *& when){
  //  if Start Time is not known - get it either from this event or from file creation date. In general - it is responcibility of the user to reset the StartTime to NULL when new file is opened.
  if(!StartTime) {
    PHTimeStamp    t0(2000,7,1,0,0,0);
    PHTimeStamp    tf;
    tf.setToSystemTime();
    //    PHTimeStamp * start = ev->getTimeStamp();
    PHTimeStamp * start = getGLV1TimeStamp(ev);
    // IF EVENT TIME STAMP IS NONSENCE - BUILD ONE FROM FILE CREATION DATE
    if(!start || (*start)<t0 || (*start)>tf){
      if(start) delete start;
      //  if new StartTime is instantiated here - it is to user to delete it
      if(fName) {
	start = GF::getFileCreationDate(fName);
      } else {
	start = new PHTimeStamp(0);
	start->setToSystemTime();
      }
    } 
    StartTime = start;
  }
  //  Now real event time
  //  when = ev->getTimeStamp();
  when = getGLV1TimeStamp(ev);
  if(!when) {
    when = new PHTimeStamp(0);
    time_t tics = StartTime->getTics()+ev->getEvtSequence();
    when->setTics(tics);
  }
}
// **********************************************************************
PHTimeStamp * GF::getGLV1TimeStamp(Event * ev)
{
  // Return a pointer to the timestamp of this event, taken
  // from glv1 packet.
  // The returned pointer is yours, thus it's up to you to delete it
  // at some point.

  Packet * p = ev->getPacket(14001);
  if (!p) return 0 ;
  PHTimeStamp *ts =  new PHTimeStamp(p->iValue(0,"YEAR"), p->iValue(0,"MONTH"),
			 p->iValue(0,"DATE"), p->iValue(0,"HOUR"),
			 p->iValue(0,"MIN"),  p->iValue(0,"SEC"));

  delete p;
  return ts;
}



// **********************************************************************


int GF::reducedMean(int items, float * e, float  & av, float & rms, float retain){
  if(!items) return 0;
  int * index = new int [items]; 
  TMath::Sort(items, e, index);
  int nz = items;
  for(int i = items-1;i>=0;i--){
    if(e[index[i]]!=0.) break;
    nz--;
  }
  if(nz<2) return 0;
  int start = 0;
  int end   = nz-1;
  if(nz>=3) {
    av = std::accumulate(e, e+nz, 0.0F)/nz;
    // FIXME: initialization to `int' from `double' !!!
    //    int minEnt = (nz<=4? nz-- : nz*0.7);
    // Just added the static_cast to pacify the compiler.
    // But should check the whole logic of this kind of assignement
    // L.A. June-16-2001
    int minEnt = static_cast<int>((nz<=4? nz-- : nz*retain));
    //    cout<<nz<<" "<<minEnt<<" "<<start<<" "<<index[start]<<" "<<end<<" "<<index[end]<<endl;
    while(nz>minEnt){
      if(abs(e[index[start]]-av)>abs(e[index[end]]-av)) start++; else end--;
      nz--;
    }
  }
  float sum  = 0.;
  float sum2 = 0.;
  float used = 0.;
  for (int i = start; i<end;i++){
    sum += e[index[i]];
    sum2+= e[index[i]]*e[index[i]];
    used +=1.;
  }
  if(used>0){
    av = sum/used;
    sum2 /=used;
    rms = sum2-av*av;
    rms = ((rms>0)? sqrt(rms) : 0.);
  } else {
    av = 0.;
    rms= 0.;
  }
  delete [] index;
  return (int)used;
}
// **********************************************************************


int GF::reducedMean(int items, double * e, double  & av, double & rms, float retain){
  if(!items) return 0;
  int * index = new int [items]; 
  TMath::Sort(items, e, index);
  int nz = items;
  for(int i = items-1;i>=0;i--){
    if(e[index[i]]!=0.) break;
    nz--;
  }
  if(nz<2) return 0;
  int start = 0;
  int end   = nz-1;
  if(nz>=3) {
    av = std::accumulate(e, e+nz, 0.0F)/nz;
    // FIXME: initialization to `int' from `double' !!!
    //    int minEnt = (nz<=4? nz-- : nz*0.7);
    // Just added the static_cast to pacify the compiler.
    // But should check the whole logic of this kind of assignement
    // L.A. June-16-2001
    int minEnt = static_cast<int>((nz<=4? nz-- : nz*retain));
    //    cout<<nz<<" "<<minEnt<<" "<<start<<" "<<index[start]<<" "<<end<<" "<<index[end]<<endl;
    while(nz>minEnt){
      if(abs(e[index[start]]-av)>abs(e[index[end]]-av)) start++; else end--;
      nz--;
    }
  }
  double sum  = 0.;
  double sum2 = 0.;
  float used = 0.;
  for (int i = start; i<end;i++){
    sum += e[index[i]];
    sum2+= e[index[i]]*e[index[i]];
    used +=1.;
  }
  if(used>0){
    av = sum/used;
    sum2 /=used;
    rms = sum2-av*av;
    rms = ((rms>0)? sqrt(rms) : 0.);
  } else {
    av = 0.;
    rms= 0.;
  }
  delete [] index;
  return (int)used;
}

//***********************************************************************
//  Version of the method of accessing files which includes recreation of the time stamp either using the data provided by user or the file creation date

char * GF::getNextFileName(ifstream & conffile, PHTimeStamp * & fTS)
{
  //  SCAN through config file until "newFile" identifier is found

  char   line[200];
  char * fName = new char [200];
  char   c;
  while(conffile>>line){
    if(!strcmp(line, "newFile")){
      //  read in the name of the file
      conffile>>fName;
    } else if (line[0]!='/') {
      // skip this line
      while(conffile.get(c)&&c!='\n');
      continue;
    } else {
      strcpy(fName,line);
    }
    //  read in creation date if known
    while(conffile.get(c)&&c==' ');
    fTS = new PHTimeStamp(2000,7,1,0,0,0);
    if(c!='\n') {
      TString d;
      d += c;
      while(conffile.get(c)&&c!='\n') {d += c;}
      fTS->set(d.Data());
    } else {
      //  File name without date
      delete fTS;
      fTS = GF::getFileCreationDate(fName);
    }
    return fName;
  }
  return 0;
}

//*************************************************************************

void GF::drawGrid(TH2 * h, TLine & l){
  if((h->GetNbinsX())%12||(h->GetNbinsY())%12) return;

  TAxis * x = ((TH1*)h)->GetXaxis();
  int xMin = (int)(x->GetXmin());
  int xMax = (int)(x->GetXmax());
  TAxis * y = ((TH1*)h)->GetYaxis();
  int yMin = (int)(y->GetXmin());
  int yMax = (int)(y->GetXmax());
  l.DrawLine(12,yMin,12,yMax);
  l.DrawLine(24,yMin,24,yMax);
  l.DrawLine(36,yMin,36,yMax);
  l.DrawLine(48,yMin,48,yMax);
  l.DrawLine(60,yMin,60,yMax);
  if(xMax>72){
    // PBGL
    l.DrawLine(72,yMin,72,yMax);
    l.DrawLine(84,yMin,84,yMax);
  }
  for (int yl = yMin+12; yl<yMax; yl+=12){
    l.DrawLine(xMin,yl,xMax,yl);
  }
  l.SetLineColor(2);
  if(xMax<96){
    // PBSC
  if(yMin<=36&&yMax>=36)  l.DrawLine(xMin,36,xMax,36);
  if(yMin<=72&&yMax>=72)  l.DrawLine(xMin,72,xMax,72);
  if(yMin<=108&&yMax>=108)l.DrawLine(xMin,108,xMax,108);
  if(yMin<=144&&yMax>=144)l.DrawLine(xMin,144,xMax,144);
  if(yMin<=180&&yMax>=180)l.DrawLine(xMin,180,xMax,180);
  } else {
    // PBGL
    if(yMin<=48&&yMax>=48)  l.DrawLine(xMin,48,xMax,48);
  }
  l.SetLineColor(1);
}

// //*************************************************************************

// void GF::drawGrid(TH2 * h, TLine & l){
//   if((h->GetNbinsX())%12||(h->GetNbinsY())%12) return;

//   TAxis * y = ((TH1*)h)->GetYaxis();
  


//   int yMin = (int)y->GetXmin();
//   int yMax = (int)y->GetXmax();
//   l.DrawLine(12,yMin,12,yMax);
//   l.DrawLine(24,yMin,24,yMax);
//   l.DrawLine(36,yMin,36,yMax);
//   l.DrawLine(48,yMin,48,yMax);
//   l.DrawLine(60,yMin,60,yMax);
//   if(yMin<=36&&yMax>=36){
//     l.DrawLine(0,12,72,12);
//     l.DrawLine(0,24,72,24);
//   }
//   if(yMin<=72&&yMax>=72){
//     l.DrawLine(0,48,72,48);
//     l.DrawLine(0,60,72,60);
//   }
//   if(yMin<=96&&yMax>=96){
//     l.DrawLine(0,84,72,84);
//     l.DrawLine(0,96,72,96);
//   }
//   if(yMin<=132&&yMax>=132){
//     l.DrawLine(0,120,72,120);
//     l.DrawLine(0,132,72,132);
//   }
//   if(yMin<=168&&yMax>=168){
//     l.DrawLine(0,156,72,156);
//     l.DrawLine(0,168,72,168);
//   }
//   if(yMin<=216&&yMax>=216){
//     l.DrawLine(0,192,72,192);
//     l.DrawLine(0,204,72,204);
//   }
//   l.SetLineColor(2);
//   if(yMin<=36&&yMax>=36)  l.DrawLine(0,36,72,36);
//   if(yMin<=72&&yMax>=72)  l.DrawLine(0,72,72,72);
//   if(yMin<=108&&yMax>=108)l.DrawLine(0,108,72,108);
//   if(yMin<=144&&yMax>=144)l.DrawLine(0,144,72,144);
//   if(yMin<=180&&yMax>=180)l.DrawLine(0,180,72,180);
//   l.SetLineColor(1);
// }


//*************************************************************************

point::point(float XX, float YY, bool OK)
  : x(XX), y(YY), ok(OK)
{
  w       = 1.;
  ws      = w;
  x1s     = XX;
  x2s     = XX*XX;
  y1s     = YY;
  y2s     = YY*YY;
  dev     = 0.;
  entries = 1;
  rmsX    = 0.;
  rmsY    = 0.;
  updated = true;
}
point::point(float XX, float YY, float WW, bool OK)
  : x(XX), y(YY), w(WW), ok(OK)
{
  ws      = WW;
  y1s     = YY*WW;
  y2s     = YY*YY*WW;
  x1s     = XX*WW;
  x2s     = XX*XX*WW;
  dev     = 0.;
  entries = (int)OK;
  rmsX    = 0.;
  rmsY    = 0.;
  updated = true;
}
void point::addEntry(float XX, float YY, float WW){
  ws     += WW;
  x1s    += XX*WW;
  x2s    += XX*XX*WW;
  y1s    += YY*WW;
  y2s    += YY*YY*WW;
  entries+= 1;
  updated = false;
}
void point::update(){
  if(ws>0.&&entries>1) {
    x   = x1s/ws;
    y   = y1s/ws;
    rmsX= (x2s/ws-x*x);
    rmsX= (rmsX>0.)? sqrt(rmsX) : 0.;
    rmsY= (y2s/ws-y*y);
    rmsY= (rmsY>0.)? sqrt(rmsY) : 0.;
    w   = ws/entries;
    ok  = true;
  }
  updated = true;
}
void point::reset(){
  w       = 0.;
  ws      = 0.;
  x1s     = 0.;
  x2s     = 0.;
  y1s     = 0.;
  y2s     = 0.;
  dev     = 0.;
  entries = 0;
  rmsX    = 0.;
  rmsY    = 0.;
  ok      = false;
  updated = false;
}
void point::print(){
  cout<<"<Point> X = "<<X()<<"+-"<<RMSX()<<" Y = "<<Y()<<"+-"<<RMSY()<<" Entries = "<<entries<<" W = "<<W()<<" STATUS = "<<ok<<endl;
}

float point::X(){return x;}
float point::Y(){if(entries>1&&!updated) update(); return y;}
bool  point::QA(){return ok;}
void  point::setQA(bool fOK){ok=fOK;}
void  point::setW(float WW){w = WW;}
void  point::setDev(float d)   {dev    = d;}
float point::W(){if(entries>1&&!updated) update(); return w;}
float point::D(){return dev;}
float point::RMSX(){if(entries>1&&!updated) update(); return rmsX;}
float point::RMSY(){if(entries>1&&!updated) update(); return rmsY;}

//*************************************************************************
single::single(int Item, int Towerid, TString Id, TString Name)
  : item(Item), towerid(Towerid), id(Id), name(Name)
{
  fitted = false;
}
void  single::push_back(point next){points.push_back(next);}
int   single::size(){return points.size();}
point & single::at(int i){return points[i];}
vector<point> & single::getPoints(){return  points;}
void  single::erase(){
  if(!points.size()) return;
  points.erase(points.begin(),points.end()-1);
}
void  single::erase(int i){
  if(!(points.size()>i)) return;
  points.erase(points.begin()+i);
}
void  single::fitLine(){
  //  we assume that extra ponts could be added or some points could be erased since last time this set was fitted
  Double_t sxy, sx, sy, sx2, ent;
  sxy = 0.;
  sx  = 0.;
  sy  = 0.;
  sx2 = 0.; 
  ent = 0.;
  vector<point>::iterator p;
  for(p=points.begin(); p != points.end(); ++p){
    if(p->QA()){
      ent+= 1.;
      sy += p->Y();
      sx += p->X();
      sxy+= p->X()*p->Y();
      sx2+= p->X()*p->X();
    }
    //	std::cout<<"fitLine "<<item<<" "<<p->QA()<<" "<< p->X()<<" "<<p->Y()<<std::endl;
  }
  sl = 0.;
  cr = 0.;
  if(ent>2) {
    Double_t det = (ent*sx2-sx*sx);
    if(det!=0) {
      sl = (ent*sxy-sx*sy)/det;
      cr = (sx2*sy-sx*sxy)/det;
    }
  }
  //  compute and store residuals
  for(p=points.begin(); p != points.end(); ++p){
    if(p->QA()){
      float d = p->Y()-(cr+sl*p->X());
      p->setDev(d);
    }
    //	std::cout<<"fitLine "<<item<<" "<<p->QA()<<" "<< p->X()<<" "<<p->Y()<<std::endl;
  }
  fitted = true;
}

void  single::cleanAndFit(){
  //  find maximum residual, reject that point, redo the fit
  if(!fitted) fitLine();
  int maxRej = size()/2;
  //  std::cout<<"Next "<<size()<<" Max rej "<<maxRej<<std::endl;
  std::vector<point>::iterator p;
  while(maxRej){
    float savedSl = sl;
    float savedCr = cr;
    std::vector<point>::iterator ptoreject;
    float resmax = 0.;
    for(p=points.begin(); p != points.end(); ++p){
      if(p->QA()&&fabs(p->D())>resmax){
	resmax    = fabs(p->D());
	ptoreject = p; 
      }
    }
    if(resmax>0.){
      ptoreject->setQA(false);
      fitLine();
      //  less then 2.5% change  to both parameters 
      //      std::cout<<"Reject "<<ptoreject->X()<<" "<<resmax<<"  Sl "<<savedSl<<" "<<sl<<" CR "<<savedCr<<" "<<cr<<std::endl;
      if(fabs(savedSl-sl)<0.2 &&(savedCr+cr)!=0. && fabs((savedCr-cr)/(savedCr+cr))<0.01) return;
      maxRej--;
    } else return;
  }
}

float single::slope(){
  if(!fitted) fitLine();
  return sl;
}
float single::crossing(){
  if(!fitted) fitLine();
  return cr;
}
void  single::print(){
  cout<<"<single> Id "<<id<<" Name "<<name<<" SIZE "<<points.size()<<endl;
}


//*************************************************************************
