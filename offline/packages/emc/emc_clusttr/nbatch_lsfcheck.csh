#!/usr/local/bin/tcsh
#
# Check script
#
#  Usage: nbatch_lsfcheck.csh outlist
#
#

if( $# != 1) then
    echo "usage : $0 outlist "
    exit
endif

source /opt/phenix/bin/phenix_setup.csh
set lsf_host_list = tmp_nbatch_lsfcheck.list
set outlist = $1

#bhosts phenix | awk '{if($2=="ok") print $1}' > ${lsf_host_list}
bhosts phenix | awk '/^rc/{ print $1}' > ${lsf_host_list}
#bhosts phenix | awk '{if($1!="HOST_NAME") print $1}' > ${lsf_host_list}

echo "------------ nbatch_lsfcheck.csh -------- "
#cat ${lsf_host_list}
rm -f ${outlist}
touch ${outlist}

@ num = `wc ${lsf_host_list} | awk '{print $1}' `
echo " --- processing hosts " ${num}

while( $num > 0 )
    set lsf_host = `awk '{if (NR=='$num') print $1}' ${lsf_host_list}`
    echo " --- scanning the hosts "$num " : " ${lsf_host}
    @ home_size = ` lsrun -m ${lsf_host} df /home | awk '/^\/dev/{print $4}' `
    set afs_area = ` lsrun -m ${lsf_host} ls -d /afs/rhic/phenix`
    set setup_file = ` lsrun -m ${lsf_host} ls /opt/phenix/bin/phenix_setup.csh `
    echo "                home_size = " ${home_size}
    echo "                afs_area = " ${afs_area}
    echo "                setup_file = " ${setup_file}
    if( ${home_size} > 100000 && ${afs_area} == "/afs/rhic/phenix" && ${setup_file} == "/opt/phenix/bin/phenix_setup.csh" ) then
	echo -n ${lsf_host}" " >> ${outlist}
	echo " OK"
    else
	echo " FAILED"
    endif
    @ num = $num - 1
end

rm -f ${lsf_host_list}


#
