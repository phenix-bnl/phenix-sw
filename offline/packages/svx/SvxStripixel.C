// =======================================
/// \file SvxStripixel.C
/// \brief Implementation of template class for a generic stripixel detector
///
/// (initialization of static parameters)
/// Created  by V. L. Rykov on 18-Mar-2004
/// Cleanup by M. McCumber on 14-Feb-2013
// =======================================


#include "SvxStripixel.h"

//================================================
// Parameters for SvxPixel1 sensor (pixel sensors)
//================================================

// see header file for definitions

template <> const int          SvxStripixel<1>::sensorType = 1;
template <> const unsigned int SvxStripixel<1>::nSection   = 7;
template <> const unsigned int SvxStripixel<1>::nReadout   = 1;
template <> const float        SvxStripixel<1>::adcgain    = 0.002;
template <> const int          SvxStripixel<1>::adctop     = 1;
template <> const int          SvxStripixel<1>::adcthresh  = 1;

template <> const int   SvxStripixel<1>::secTYPE[][2] = 
  {{2},{3},{1},{3},{1},{3},{2}};
template <> const double SvxStripixel<1>::secXpos[][2] =
  {{0.},{0.},{0.},{0.},{0.},{0.},{0.}};
template <> const double SvxStripixel<1>::secZpos[][2] =
  {{-2.12125},{-1.4},{-0.7},{0.},{0.7},{1.4},{2.12125}};
template <> const bool  SvxStripixel<1>::xcntRvrs[][2] =
  {{false},{false},{false},{false},{false},{false},{false}};
template <> const bool  SvxStripixel<1>::zcntRvrs[][2] =
  {{false},{false},{false},{false},{false},{false},{false}};
template <> const int   SvxStripixel<1>::chanOffs[][2] =
  {{0},{0},{0},{0},{0},{0},{0}};
template <> const int   SvxStripixel<1>::chanUplim[][2] =
  {{7935},{511},{7679},{511},{7679},{511},{7935}};
template <> const int   SvxStripixel<1>::xSlope[][2] =
  {{1},{1},{1},{1},{1},{1},{1}};
template <> const int   SvxStripixel<1>::zSlope[][2] =
  {{256},{256},{256},{256},{256},{256},{256}};
template <>       int   SvxStripixel<1>::chanCutOff[][2] =
  {{7936},{512},{7680},{512},{7680},{512},{7936}};

//=====================================================
// Parameters for SvxStrip11 sensor (stripixel sensors)
//=====================================================

// see header file for definitions

template <> const int          SvxStripixel<11>::sensorType = 11;
template <> const unsigned int SvxStripixel<11>::nSection   = 2;
template <> const unsigned int SvxStripixel<11>::nReadout   = 2;
// D. McGlinchey 13 Jan 2015 - 
//    Change gain from 0.0038 based on simulation/data comparisons
template <> const float        SvxStripixel<11>::adcgain    = 0.0019;
template <> const int          SvxStripixel<11>::adctop     = 255;
template <> const int          SvxStripixel<11>::adcthresh  = 1;

template <> const int   SvxStripixel<11>::secTYPE[][2] = 
  {{10,10},{10,10}};
template <> const double SvxStripixel<11>::secXpos[][2] =
  {{.0000,.0000},{-.0000,-.0000}};
template <> const double SvxStripixel<11>::secZpos[][2] =
  {{-1.50000,-1.50000},{1.50000,1.50000}};
template <> const bool  SvxStripixel<11>::xcntRvrs[][2] =
  {{false,false},{false,false}};
template <> const bool  SvxStripixel<11>::zcntRvrs[][2] =
  {{false,false},{false,false}};
template <> const int   SvxStripixel<11>::chanOffs[][2] = 
  {{0,0},{0,0}};
template <> const int   SvxStripixel<11>::chanUplim[][2] =
  {{383,383},{383,383}};
template <> const int   SvxStripixel<11>::xSlope[][2] = 
  {{1,1},{1,1}};
template <> const int   SvxStripixel<11>::zSlope[][2] = 
  {{0,1},{0,1}};
template <>       int   SvxStripixel<11>::chanCutOff[][2] =
  {{384,413},{384,413}};

