##############################################################################
# 
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of the hl_region High-Level HDF5 APIs.  The full copyright 
# notice, including terms governing use, modification, and redistribution, 
# is contained in the file COPYING, which can be found at the root of the
# source code distribution tree and in the documentation directory (doc/html/). 
# If you do not have access to this file, you may request a copy of 
# "the hl_region High-Level HDF5 APIs copyright and license statement" from 
# help@hdfgroup.org.
# 
##############################################################################
#! /bin/sh
#

# Since we don't set up CC variable, the follwoing next lines
# didn't work.
#case $CC in
#*/*)    H5DUMP=`echo $CC | sed -e 's/\/[^/]*$/\/h5dump/'`;
#        test -x $H5DUMP || H5DUMP=h5dump;;
#*)      H5DUMP=h5dump;;
#esac

# Use h5dump from the install directory

H5DUMP=$HDF5_INSTALL_DIR/bin/h5dump
echo $H5DUMP

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


exout() {
    echo '*******************************'
    echo '*  Output of example program  *'
    echo '*******************************'
    echo
    $*
}

dumpout() {
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}

dumpout2() {
    echo
    echo
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}


topics="ex_bitfield ex_lite_copy_region ex_lite_read_region ex_ref_to_all ex_regref_copy_reference ex_regref_create ex_regref_read"


return_val=0


for topic in $topics
do
    fname=$topic
    $ECHO_N "Testing fortran/examples/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    status=$?
    if test $status -eq 1
    then
        echo "  Unsupported feature"
    else
        dumpout2 $fname.h5 >>tmp.test
        rm -f $fname.h5
        cmp -s tmp.test ./$fname.test
        status=$?
        if test $status -ne 0
        then
	    \diff tmp.test ./$fname.test > tmp.diff
            # test to see if the only difference is because of big-endian and little-endian
	    echo " "
	    NumOfFinds=`grep -c "DATATYPE" tmp.diff`
	    NumOfFinds=`expr $NumOfFinds \* 2`
	    NumOfLines=`wc -l <tmp.diff`
	    if test $NumOfLines -gt $NumOfFinds 
		then
		echo "  FAILED!"
		return_val=`expr $status + $return_val`
            else
		echo "  *Inconsequential differance*    Passed"
	    fi
	    #\rm -f tmp.diff
	    echo " "
        else
            echo "  Passed"
        fi
    fi
done

rm -f tmp.test tmp.diff
echo "$return_val tests failed in fortran/examples"
exit $return_val
