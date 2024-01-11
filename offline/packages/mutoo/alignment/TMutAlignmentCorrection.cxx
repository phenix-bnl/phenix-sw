// $Id: TMutAlignmentCorrection.cxx,v 1.7 2014/12/01 16:43:03 jinhuang Exp $

/*!
\file    TMutAlignmentCorrection.cxx
\brief   Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
\author  Catherine Silvestre
\version $Revision: 1.7 $
\date    $Date: 2014/12/01 16:43:03 $
*/

#include "TMutAlignmentCorrection.h"
#include <MUTOO.h>
#include <MUIOO.h>

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
void TMutAlignmentCorrection::initialize( const char* file,  const char* filename)
{

    // get TTree from root file
    TFile *tf = new TFile( file );
    TTree *tree = (TTree*)tf->Get("misalignment");
    TMutAlignmentCorrection::initialize(tree,filename);


};

//________________________________________________________
void TMutAlignmentCorrection::initialize( TTree* tree, const char* filename)
{

    MUTOO::PRINT( cout, "TMutAlignmentCorrection::initialize" );

    // MuonGlobalAlign::make_backup( filename);

    cout <<"backup made" << endl;
    ofstream out( filename, ios::out );
    cout <<"file writing in" << endl;

    // get branches address here for mutr
    int arm = 0;
    int station = 0;
    int gap =0;
    int cathode =0;
    int octant = 0;
    int half = 0;
    int nb_tracks = 0;
    int is_muid = 0;
    int detector_id = kUNKNOWN_DETECTOR;
    tree->SetBranchAddress( "arm", &arm );
    tree->SetBranchAddress( "station", &station );
    tree->SetBranchAddress( "gap", &gap );
    tree->SetBranchAddress( "cathode", &cathode );
    tree->SetBranchAddress( "octant", &octant );
    tree->SetBranchAddress( "half", &half );
    tree->SetBranchAddress( "nb_tracks", &nb_tracks );

    if (tree->FindBranch("is_muid"))
      tree->SetBranchAddress("is_muid",     &is_muid);
    if (tree->FindBranch("detector_id"))
      tree->SetBranchAddress("detector_id",     &detector_id);

    // get branches address here for muid
    int plane = 0;
    int panel =0;
    int orientation =0;
    tree->SetBranchAddress( "plane", &plane );
    tree->SetBranchAddress( "panel", &panel );
    tree->SetBranchAddress( "orientation", &orientation );

    // delta x
    double delta_x = 0;
    double delta_x_millepede = 0;
    double error_w = 0;
    double delta_w = 0;
    double delta_w_millepede = 0;
    tree->SetBranchAddress( "delta_w", &delta_w );
    tree->SetBranchAddress( "delta_x", &delta_x );
    tree->SetBranchAddress( "delta_w_millepede", &delta_w_millepede );
    tree->SetBranchAddress( "delta_x_millepede", &delta_x_millepede );
    tree->SetBranchAddress("error_w",    &error_w);

    // delta y
    double delta_y = 0;
    double delta_y_millepede = 0;
    tree->SetBranchAddress( "delta_y", &delta_y );
    tree->SetBranchAddress( "delta_y_millepede", &delta_y_millepede );

    // delta phi
    double delta_phi = 0;
    double delta_phi_millepede = 0;
    double error_phi = 0;
    tree->SetBranchAddress( "delta_phi", &delta_phi );
    tree->SetBranchAddress( "delta_phi_millepede", &delta_phi_millepede );
    tree->SetBranchAddress("error_phi",  &error_phi);

    // delta z
    double delta_z = 0;
    double delta_z_millepede = 0;
    double error_z = 0;
    tree->SetBranchAddress( "delta_z", &delta_z );
    tree->SetBranchAddress( "delta_z_millepede", &delta_z_millepede );
    tree->SetBranchAddress("error_z",    &error_z);

    // get the entries of the tree
    for( int i = 0; i <tree->GetEntries(); i++ ) {

        tree->GetEntry( i );

        if ( ((detector_id == kUNKNOWN_DETECTOR)and(!is_muid)) or (detector_id == kMUTR) )
        {

            TMutAlignmentCorrection::TMutAlignmentCorrection_mutr par_mutr(
                arm,
                station,
                octant,
                gap,
                cathode,
                half,
                delta_x_millepede + delta_x,
                delta_y_millepede + delta_y,
                delta_z_millepede + delta_z,
                delta_phi_millepede + delta_phi,
                delta_w_millepede + delta_w,
                error_w,
                error_z,
                error_phi,
                nb_tracks );

            _mutr_parameters.insert( par_mutr);

        }


        if ( ((detector_id == kUNKNOWN_DETECTOR)and(is_muid)) or (detector_id == kMUID) )
          {
            TMutAlignmentCorrection::TMutAlignmentCorrection_muid par_muid(
                arm,
                plane,
                panel,
                orientation,
                delta_x_millepede + delta_x,
                delta_y_millepede + delta_y,
                delta_z_millepede + delta_z,
                delta_phi_millepede + delta_phi,
                delta_w_millepede + delta_w,
                error_w,
                error_z,
                error_phi,
                nb_tracks );

            _muid_parameters.insert( par_muid);

        }
    }

    if( _mutr_alignment_enabled )
    {
        for (arm=0; arm<MUTOO::NumberOfArms;arm++)
        {
            if(arm==0) {

                out << endl;
                out << "// ------------- MUTR ------------"<< endl;
                out << endl;
                out <<"// **************** SOUTH ****************"<<endl;

            } else {

                out << endl;
                out << "// **************** NORTH ****************" << endl;

            }

            for ( station =0; station <MUTOO::NumberOfStations; station++)
            {
                for(  gap = 0; gap<MUTOO::NumberOfGaps; gap++ )
                {

                    // needed to remove station3 gap3
                    if( station == 2 && gap == 2 ) continue;

                    /*
                    retrieve the four misalignment_mutr parameters of interest
                    calculate average delta_z
                    */
                    for( cathode = 0; cathode<MUTOO::NumberOfCathodePlanes; cathode++ )
                    {
                        for( octant = 0; octant <MUTOO::NumberOfOctants;octant ++)
                        {
                            // output parameters
                            double delta_x = 0;
                            double delta_y = 0;
                            double delta_phi = 0;

                            // get values for each half octant
                            TMutAlignmentCorrection_mutr tmp0 = get_mutr_parameters(arm, station, octant, gap, cathode, 0);
                            TMutAlignmentCorrection_mutr tmp1 = get_mutr_parameters(arm, station, octant, gap, cathode, 1);

                            // calculate medium value for each octant
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
                            if(octant==0 )
                            {
                                out << endl;
                                out << "// arm"<< tmp0._arm
                                    <<  " station"<< tmp0._station
                                    << " /octant" <<" gap"<< tmp0._gap
                                    <<" cathode"<< tmp0._cathode
                                    <<" /x"  << " /y"  << " /phi" << endl;
                                }

                                out <<"cathode "<< tmp0._arm << " " << tmp0._station << " "<< tmp0._octant << " "
                                    << tmp0._gap << " "
                                    << tmp0._cathode << " "<< delta_x << " "<< delta_y << " "
                                    << delta_phi;

                                string error( errorStream.str() );
                                if( !error.empty() ) out << " // " << error;
                                out << endl;

                        }

                        if( cathode==1 && _z_alignment_enabled )
                        {
                            for( octant = 0; octant <MUTOO::NumberOfOctants;octant ++)
                            {
                                // output parameters
                                double delta_z_cath = 0;
                                double delta_z[16];
                                int index = 0;

                                ostringstream errorStream;
                                for( half = 0; half<MUTOO::NumberOfHalfOctants; half++ )
                                {

                                    // get z anode value for 2 cathodes
                                    TMutAlignmentCorrection_mutr tmp2 = get_mutr_parameters (arm, station, octant, gap, 0, half);
                                    TMutAlignmentCorrection_mutr tmp3 = get_mutr_parameters (arm, station, octant, gap,  1, half);
                                    // calculate medium value for each octant
                                    index = half+2*octant;
                                    delta_z[index] = (tmp2._delta_z + tmp3._delta_z)/2;

                                }

                                TMutAlignmentCorrection_mutr tmp0 = get_mutr_parameters (arm, station, octant, gap, cathode, 0);
                                TMutAlignmentCorrection_mutr tmp1 = get_mutr_parameters (arm, station, octant, gap, cathode, 1);

                                if( (tmp0._nb_tracks >50 && fabs(tmp0._error_z) < 0.5) && (tmp1._nb_tracks >50 && fabs(tmp1._error_z) < 0.5) )
                                {
                                    delta_z_cath = (delta_z[0+2*octant]+delta_z[1+2*octant])/2;

                                } else {

                                    delta_z_cath = 0 ;

                                }

                                if(octant ==0)
                                {
                                    out << endl;
                                    out << "// ANODE -- arm"<< tmp0._arm<<  " station"<< tmp0._station << " octant" <<tmp0._octant
                                        <<" gap"<< tmp0._gap
                                        <<" anode /z" << endl;
                                }
                                if(delta_z_cath==0)
                                {
                                    errorStream
                                        << "z:  nb_tracks too small:"<< tmp0._nb_tracks << ", " << tmp1._nb_tracks
                                        <<" || error too big "<< tmp0._error_z<< ", " << tmp1._error_z;
                                    }

                                    out
                                        <<"anode "<< tmp0._arm << " " << tmp0._station << " "<< tmp0._octant << " " << tmp0._gap << " "
                                        <<delta_z_cath;

                                    string error( errorStream.str() );
                                    if( !error.empty() ) out << " // " << error;
                                    out << endl;

                            }
                        }
                    }
                }
            }
        }
    }

    // muid
    if( _muid_alignment_enabled )
    {
        out << endl;
        out << "// ---------------- MUID -------------------" << endl;
        out << endl;

        for (int arm=0; arm<MUIOO::MAX_ARM;arm++)
        {
            if(arm==0)
            {
                out << endl;
                out <<"// **************** SOUTH *****************"<<endl;
            }
            else
            {
                out << endl;
                out << "// ****************** NORTH *****************" << endl;
            }

            for ( int plane =0; plane <MUIOO::MAX_PLANE; plane++)
            {
                for ( int panel = 0; panel< MUIOO::MAX_PANEL; panel++ )
                {
                    // output parameters
                    double delta_phi = 0;
                    double delta_x = 0;
                    double delta_y = 0;
                    double delta_z_muid = 0;

                    TMutAlignmentCorrection_muid tmp4 = get_muid_parameters (arm, plane, panel, 1 );
                    TMutAlignmentCorrection_muid tmp5 = get_muid_parameters (arm, plane, panel, 0 );

                    ostringstream errorStream;

                    // get misalignment medium value from all orientations

                    if( (tmp4._nb_tracks >50 && tmp5._nb_tracks >50) )
                    {
                        if( fabs(tmp4._error_w) < 0.55 && fabs(tmp5._error_w) < 0.55)
                        {
                            delta_x = tmp4._delta_x;
                            delta_y = tmp5._delta_y;

                        } else {

                            delta_x = 0;
                            delta_y = 0;
                            errorStream <<"error too big: "<< tmp4._error_w << ", "<< tmp5._error_w << " ";
                        }

                        if( fabs(tmp4._error_phi) < 0.0025  && fabs(tmp5._error_phi) < 0.0025)
                        {

                            delta_phi = (tmp4._delta_phi + tmp5._delta_phi)/2;

                        } else {

                            delta_phi = 0;
                            errorStream <<"phi error too big: "<< tmp4._error_phi << ", "<< tmp5._error_phi << " ";

                        }

                        if( fabs(tmp4._error_z) < 2 && fabs(tmp5._error_z) < 2 )
                        {

                            delta_z_muid =(tmp4._delta_z + tmp5._delta_z)/2;

                        } else {

                            delta_z_muid = 0;
                            errorStream << "z error too big: "<< tmp4._error_z  << ", " << tmp5._error_z << " ";

                        }

                    } else {

                        delta_x = 0;
                        delta_y = 0;
                        delta_phi = 0;
                        delta_z_muid = 0;

                        errorStream << "nb_tracks too small:"<< tmp4._nb_tracks << ", " << tmp5._nb_tracks << " ";
                    }

                    // print out
                    if (panel==0)
                    {
                        out << endl;
                        out
                            << "// arm"<< tmp4._arm<<  " plane"<< tmp4._plane
                            << " /panel" <<" /x"  << " /y"  << " /z"<< " /phi" << endl;
                    }

                    out
                        <<"muid_panel "<< tmp4._arm << " " << tmp4._plane << " "<< tmp4._panel << " "
                        << delta_x << " "<< delta_y << " "<< delta_z_muid << " "<< delta_phi;

                    string error( errorStream.str() );
                    if( !error.empty() )
                    { out << " // " << error; }

                    out << endl;
                }
            }
        }
    }

    out.close();

}

//________________________________________________________
TMutAlignmentCorrection::TMutAlignmentCorrection_mutr TMutAlignmentCorrection::get_mutr_parameters( int arm, int station, int octant, int gap, int  cathode,int half )
{
    TMutAlignmentCorrection_mutr tmp(arm, station, octant, gap, cathode, half);
    mutr_set::iterator iter( _mutr_parameters.find (tmp));
    return (iter == _mutr_parameters.end() ) ? tmp:*iter;
}

//________________________________________________________
TMutAlignmentCorrection::TMutAlignmentCorrection_muid TMutAlignmentCorrection::get_muid_parameters( int arm, int plane, int panel, int orientation )
{
    TMutAlignmentCorrection_muid tmp(arm, plane, panel, orientation);
    muid_set::iterator iter( _muid_parameters.find (tmp));
    return (iter == _muid_parameters.end() ) ? tmp:*iter;
}

//_________________________________
string TMutAlignmentCorrection::make_backup( const string& filename_align )
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
