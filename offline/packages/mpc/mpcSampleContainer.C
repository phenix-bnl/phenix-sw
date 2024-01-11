#include <mpcSampleContainer.h>
#include <mpcSample.h>
#include <recoConsts.h>

#include <PHCompositeNode.h>
#include <phool.h>

#include <cmath>

#include <TGraph.h>
#include <TSpline.h>
#include <TPad.h>
#include <TH1.h>
#include <TF1.h>
#include <TFile.h>
#include <TProfile.h>
#include <TDirectory.h>

using namespace std;

ClassImp(mpcSampleContainer)

mpcSampleContainer::mpcSampleContainer()
{
  //cout << "In mpcSampleContainer()" << endl;
}

mpcSampleContainer::~mpcSampleContainer()
{
  //cout << "In ~mpcSampleContainer()" << endl;
}

void mpcSampleContainer::Reset()
{
}

