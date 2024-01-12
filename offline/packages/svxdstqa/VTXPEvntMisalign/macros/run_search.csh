#!/bin/csh

@ run_number = $1
@ lad_number = $2
echo "run number = "$run_number
#user must set outdir and infile_tree manually for each dataset
set outdir = "/gpfs02/phenix/vtx/subsys/vtx/zrowan/misaligncheck_misaligned/"$run_number
set infile_tree = "/gpfs04/phenix/crs/agg/run15/run15pp_200GeV_MisalignCheck_ana519_agg/MT_SVXPBBC/MT_SVXPBBC_run15pp_200GeV_MisalignCheck_ana519-0000"$run_number"-9000.root"
set outtree = $outdir"/outtree_"$run_number"_"$lad_number".root"
set outascii = $outdir"/outascii_"$run_number"_"$lad_number".txt"
set outimage_dir = $outdir"/images"

@ nEvt = 0

# copy infile to this scratch dir
mkdir $_CONDOR_SCRATCH_DIR/$run_number

set scr_intree = $_CONDOR_SCRATCH_DIR"/"$run_number"/intree.root"
set scr_outtree = $_CONDOR_SCRATCH_DIR"/"$run_number"/outtree.root"
set scr_outascii = $_CONDOR_SCRATCH_DIR"/"$run_number"/outascii.txt"

cp $infile_tree $scr_intree

# Make directory for the images in scratch
set scr_imagedir = $_CONDOR_SCRATCH_DIR"/"$run_number"/images"
mkdir $scr_imagedir 


###############################
# Run the search software #
###############################

# echo root -b -q look_eventoffset.C\(\"${scr_intree}\",\"${scr_outtree}\",\"${scr_imagedir}\",${run_number},${lad_number}\)
# root -b -q look_eventoffset.C\(\"${scr_intree}\",\"${scr_outtree}\",\"${scr_imagedir}\",${run_number},${lad_number}\)
echo root -b -q look_eventoffset_byladder.C\(\"${scr_intree}\",\"${scr_outtree}\",\"${scr_imagedir}\",${run_number},${lad_number}\)
root -b -q look_eventoffset_byladder.C\(\"${scr_intree}\",\"${scr_outtree}\",\"${scr_imagedir}\",${run_number},${lad_number}\)


####################
# Make ASCII Files #
####################

echo root -b -q write_misalign_text_new_format.C+\(\"${scr_outtree}\",\"${scr_outascii}\"\)
root -b -q write_misalign_text_new_format.C+\(\"${scr_outtree}\",\"${scr_outascii}\"\)


#####################################
# Copy all files to final locations #
#####################################

mkdir -p $outdir
mkdir -p $outimage_dir
cp $scr_imagedir/* $outimage_dir
cp $scr_outtree $outtree
cp $scr_outascii $outascii



exit 0
