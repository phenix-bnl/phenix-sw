// $Id: TFvtxAlignmentCorrection.cxx,v 1.4 2014/03/03 03:16:07 jinhuang Exp $

/*!
\file    TFvtxAlignmentCorrection.cxx
\brief   Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
\author  Zhengyun You
\version $Revision: 1.4 $
\date    $Date: 2014/03/03 03:16:07 $
*/

#include "TFvtxAlignmentCorrection.h"
#include <MUTOO.h>
#include <FVTXGEOM.h>

#include <set>
#include <fstream>
#include <sstream>
#include <TTree.h>
#include <TFile.h>

#include <MILLEPEDE.h>

#include <iostream>
using namespace std;
using namespace MILLEPEDE;



//________________________________________________________
void TFvtxAlignmentCorrection::initialize( const char* file,  const char* filename)
{

    // get TTree from root file
    TFile *tf = new TFile( file );
    TTree *tree = (TTree*)tf->Get("misalignment");
    TFvtxAlignmentCorrection::initialize(tree,filename);


};

//________________________________________________________
void TFvtxAlignmentCorrection::initialize( TTree* tree, const char* filename)
{

    MUTOO::PRINT( cout, "TFvtxAlignmentCorrection::initialize - this class is obsolete. "
        "Please use macro for output offline/packages/fvtxoo/alignment/macros/FvtxGlobalAlign2Text.C" );

    // MuonGlobalAlign::make_backup( filename);

    cout <<"backup made" << endl;
    ofstream out( filename, ios::out );
    cout <<"file writing in" << endl;

    // get branches address here for fvtx
    int arm = 0;
    int cage = 0;
    int station = 0;
    int sector = 0;
    int half = 0;
    int nb_tracks = 0;
    int is_fvtx = 1;
    int detector_id = kUNKNOWN_DETECTOR;

    tree->SetBranchAddress( "arm", &arm );
    tree->SetBranchAddress( "cage", &cage );
    tree->SetBranchAddress( "station", &station );
    tree->SetBranchAddress( "sector", &sector );
    tree->SetBranchAddress( "half", &half );
    tree->SetBranchAddress( "nb_tracks", &nb_tracks );
//    tree->SetBranchAddress( "is_fvtx",     &is_fvtx);
    if (tree->FindBranch("detector_id"))
      tree->SetBranchAddress("detector_id",     &detector_id);


    // delta x
    double delta_x_millepede = 0;
    double error_w = 0;
    double delta_w_millepede = 0;
    tree->SetBranchAddress( "delta_w_millepede", &delta_w_millepede );
    tree->SetBranchAddress( "delta_x_millepede", &delta_x_millepede );
    tree->SetBranchAddress( "error_w",    &error_w);

    // delta y
    double delta_y_millepede = 0;
    tree->SetBranchAddress( "delta_y_millepede", &delta_y_millepede );

    // delta phi
    double delta_phi_millepede = 0;
    double error_phi = 0;
    tree->SetBranchAddress( "delta_phi_millepede", &delta_phi_millepede );
    tree->SetBranchAddress("error_phi",  &error_phi);

    // delta z
    double delta_z_millepede = 0;
    double error_z = 0;
    tree->SetBranchAddress( "delta_z_millepede", &delta_z_millepede );
    tree->SetBranchAddress("error_z",    &error_z);

    // get the entries of the tree
    for( int i = 0; i <tree->GetEntries(); i++ ) {

        tree->GetEntry( i );

        if ( ((detector_id == kUNKNOWN_DETECTOR)and(is_fvtx)) or (detector_id == kFVTX_WEDGE) )
        {

            TFvtxAlignmentCorrection::TFvtxAlignmentCorrection_fvtx par_fvtx(
                arm,
                cage,
                station,
                sector,
                half,
                delta_x_millepede,
                delta_y_millepede,
                delta_z_millepede,
                delta_phi_millepede,
                delta_w_millepede,
                error_w,
                error_z,
                error_phi,
                nb_tracks );

            _fvtx_parameters.insert( par_fvtx);

        } else {

        }
    }

    if( _fvtx_alignment_enabled )
    {
        for (arm=0; arm<FVTXGEOM::NumberOfArms;arm++)
        {
            if(arm==0) {

                out << endl;
                out << "// ------------- FVTX ------------"<< endl;
                out << endl;
                out <<"// **************** SOUTH ****************"<<endl;

            } else {

                out << endl;
                out << "// **************** NORTH ****************" << endl;

            }

            for ( cage =0; cage <FVTXGEOM::NumberOfCages; cage++)
            {
                for ( station =0; station <FVTXGEOM::NumberOfStations; station++)
                {
                    /*
                    retrieve the four misalignment_fvtx parameters of interest
                    calculate average delta_z
                    */
                        for( sector = 0; sector <FVTXGEOM::NumberOfSectors;sector ++)
                        {
                            // output parameters
                            double delta_x = 0;
                            double delta_y = 0;
                            double delta_phi = 0;

                            // get values for each half sector
                            TFvtxAlignmentCorrection_fvtx tmp0 = get_fvtx_parameters(arm, cage, station, sector, 0);
                            TFvtxAlignmentCorrection_fvtx tmp1 = get_fvtx_parameters(arm, cage, station, sector, 1);

                            // calculate medium value for each sector
                            ostringstream errorStream;
                            if( tmp0._nb_tracks >50 && tmp1._nb_tracks > 50)
                            {
                                if(fabs(tmp0._error_w) < 0.15  && fabs(tmp1._error_w) <0.15)
                                {

                                    delta_x = tmp1._delta_x;
                                    delta_y = tmp1._delta_y;

                                } else {

                                    delta_x = 0;
                                    delta_y = 0;
                                    errorStream <<"x,y error too big: "<< tmp0._error_w<< ", " << tmp1._error_w << " ";
                                }

                                if((fabs(tmp0._error_phi) < 0.0015)  && fabs(tmp1._error_phi) < 0.0015 )
                                {

                                    delta_phi = (tmp0._delta_phi + tmp1._delta_phi)/2;

                                } else {

                                    delta_phi = 0;
                                    errorStream <<"phi error too big: "<< tmp0._error_phi << ", " << tmp1._error_phi << " ";

                                }

                            } else {

                                delta_x = 0;
                                delta_y = 0;
                                delta_phi = 0;
                                errorStream  << "nb_tracks too small:"<< tmp0._nb_tracks << ", " << tmp1._nb_tracks << " ";

                            }

                            // print out
                            if(sector==0 )
                            {
                                out << endl;
                                out << "// arm"<< tmp0._arm
                                    << " cage " << tmp0._cage
                                    << " station"<< tmp0._station
                                    << " /sector" << tmp0._sector
                                    <<" /x"  << " /y"  << " /phi" << endl;
                                }

                                out << tmp0._arm << " " << tmp0._cage << " " << tmp0._station << " "<< tmp0._sector << " "
                                    << " "<< delta_x << " "<< delta_y << " "
                                    << delta_phi;

                                string error( errorStream.str() );
                                if( !error.empty() ) out << " // " << error;
                                out << endl;

                        }

                }
            }
        }
    }

    out.close();

}

//________________________________________________________
TFvtxAlignmentCorrection::TFvtxAlignmentCorrection_fvtx TFvtxAlignmentCorrection::get_fvtx_parameters( int arm, int cage, int station, int sector, int half )
{
    TFvtxAlignmentCorrection_fvtx tmp(arm, cage, station, sector, half);
    fvtx_set::iterator iter( _fvtx_parameters.find (tmp));
    return (iter == _fvtx_parameters.end() ) ? tmp:*iter;
}

//_________________________________
string TFvtxAlignmentCorrection::make_backup( const string& filename_align )
{
    if( access( filename_align.c_str(), R_OK ) ) return filename_align;

    string backup;
    unsigned int ver=0;

    do{
        ostringstream what;
        what << filename_align << "." << ver;
        backup = what.str();
        ver++;
    } while  ( !access( backup.c_str(), R_OK ) );

    ostringstream what;
    what << "cp " << filename_align << " " << backup;
    system( what.str().c_str() );

    cout << "MuonGlobalAlign::make_backup - file \"" << backup << "\" created.\n";
    return backup;
}
