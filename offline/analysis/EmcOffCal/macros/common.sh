export ANALYSIS_DIR=/direct/phenix+spin/phnxsp01/manion/EMC_Iterv2
export TREE_DIR=/direct/phenix+analysis/phnxreco/run11emccal/EMC_Calib/
export LIST_RUN=$ANALYSIS_DIR/runlist.txt
export LIST_DST_ALL=$ANALYSIS_DIR/dst.txt

export WARNMAP_DIR=$ANALYSIS_DIR/map
export WARNMAP_FILE_NAME=$WARNMAP_DIR/warnmap.txt

export PI0_HIST_DIR=$ANALYSIS_DIR/calib

export MACRO_DIR=/direct/phenix+u/workarea/manion/source/EMC_Calib/macros

export MY_INSTALL_DIR=/direct/phenix+u/workarea/manion/build/EMC_Calib/
if echo "$LD_LIBRARY_PATH" | grep -qv "$MY_INSTALL_DIR/lib" ; then
    export LD_LIBRARY_PATH="$MY_INSTALL_DIR/lib:$LD_LIBRARY_PATH"
fi

function SetPi0HistEnv {
    local -r NUMBER=$1

    export PI0_HIST_ROOT_FILE_NAME=$PI0_HIST_DIR/hist_$NUMBER.root
    export PI0_HIST_TABLE_FILE_NAME_TYPE=$PI0_HIST_DIR/table_type_$NUMBER.txt
    export PI0_HIST_TABLE_FILE_NAME_SECTOR=$PI0_HIST_DIR/table_sector_$NUMBER.txt
    export PI0_HIST_TABLE_FILE_NAME_TOWER=$PI0_HIST_DIR/table_tower_$NUMBER.txt
	
    if [ $NUMBER -gt 0 ] ; then
	export COEF_FILE_NAME_IN=$PI0_HIST_DIR/coefficient_iter$(( NUMBER - 1 )).txt
	export COEF_FILE_NAME_SUPERMOD_IN=$PI0_HIST_DIR/coefficient_supermod_iter$(( NUMBER - 1 )).txt

	export UNCALIB_LIST_FILE_NAME_IN=$PI0_HIST_DIR/uncalib_iter$(( NUMBER - 1 )).txt
    else
	export COEF_FILE_NAME_IN=$PI0_HIST_DIR/coefficient.txt
	export COEF_FILE_NAME_SUPERMOD_IN=$PI0_HIST_DIR/coefficient_supermod.txt

	export UNCALIB_LIST_FILE_NAME_IN=$PI0_HIST_DIR/uncalib.txt
    fi
    export COEF_FILE_NAME_OUT=$PI0_HIST_DIR/coefficient_iter$NUMBER.txt
    export COEF_FILE_NAME_SUPERMOD_OUT=$PI0_HIST_DIR/coefficient_supermod_iter$NUMBER.txt

    export UNCALIB_LIST_FILE_NAME_OUT=$PI0_HIST_DIR/uncalib_iter$NUMBER.txt
}
