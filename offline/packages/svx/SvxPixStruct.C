// ====================
// FILE: SvxPixStruct.C
// ====================
// ****************************************************************************
// Implementation of template class for a pixel structure:
// (initialization of static geometry constants)
// ---
// Created by V. L. Rykov on 12-Dec-2003
// ****************************************************************************

#include "SvxPixStruct.h"

// Geometry for the SvxPixel sensor (ALICE hybrid silicon pixel chips)
// """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
// Chip #1 (inner)
template <> const double SvxPixStruct<1>::siIonPot   = 0.277777778e9;    // electron-hole pairs/GeV
template <> const int    SvxPixStruct<1>::nXpitch    = 256;
template <> const double SvxPixStruct<1>::xPitch     = 0.005;
template <> const int    SvxPixStruct<1>::nZpitch    = 30;
template <> const double SvxPixStruct<1>::zPitch     = 0.0425;
template <> const double SvxPixStruct<1>::xhalfWidth = 0.5*nXpitch*xPitch;
template <> const double SvxPixStruct<1>::zhalfWidth = 0.5*nZpitch*zPitch;
// Chip #2 (edge)
template <> const double SvxPixStruct<2>::siIonPot   = 0.277777778e9;    // electron-hole pairs/GeV
template <> const int    SvxPixStruct<2>::nXpitch    = 256;
template <> const double SvxPixStruct<2>::xPitch     = 0.005;
template <> const int    SvxPixStruct<2>::nZpitch    = 31;
template <> const double SvxPixStruct<2>::zPitch     = 0.0425;
template <> const double SvxPixStruct<2>::xhalfWidth = 0.5*nXpitch*xPitch;
template <> const double SvxPixStruct<2>::zhalfWidth = 0.5*nZpitch*zPitch;
// Chip #3 (connection)
template <> const double SvxPixStruct<3>::siIonPot   = 0.277777778e9;    // electron-hole pairs/GeV
template <> const int    SvxPixStruct<3>::nXpitch    = 256;
template <> const double SvxPixStruct<3>::xPitch     = 0.005;
template <> const int    SvxPixStruct<3>::nZpitch    = 2;
template <> const double SvxPixStruct<3>::zPitch     = 0.0625;
template <> const double SvxPixStruct<3>::xhalfWidth = 0.5*nXpitch*xPitch;
template <> const double SvxPixStruct<3>::zhalfWidth = 0.5*nZpitch*zPitch;

// Geometry for the SvxStripSpixel (standard strippixel, spiral design)
// """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
template <> const double SvxPixStruct<10>::siIonPot   = 0.277777778e9;    // electron-hole pairs/GeV
template <> const int    SvxPixStruct<10>::nXpitch    = 384;
template <> const double SvxPixStruct<10>::xPitch     = 0.008;
template <> const int    SvxPixStruct<10>::nZpitch    = 30;
template <> const double SvxPixStruct<10>::zPitch     = 0.1;
template <> const double SvxPixStruct<10>::xhalfWidth = 0.5*nXpitch*xPitch;
template <> const double SvxPixStruct<10>::zhalfWidth = 0.5*nZpitch*zPitch;

// Geometry for the SvxStripIpixel (strippixel with interleaving pads)
// """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
template <> const double SvxPixStruct<20>::siIonPot   = 0.277777778e9;    // electron-hole pairs/GeV
template <> const int    SvxPixStruct<20>::nXpitch    = 192;
template <> const double SvxPixStruct<20>::xPitch     = 0.016;
template <> const int    SvxPixStruct<20>::nZpitch    = 30;
template <> const double SvxPixStruct<20>::zPitch     = 0.1;
template <> const double SvxPixStruct<20>::xhalfWidth = 0.5*nXpitch*xPitch;
template <> const double SvxPixStruct<20>::zhalfWidth = 0.5*nZpitch*zPitch;

