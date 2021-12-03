!
!##############################################################################
!# 
!# Copyright by The HDF Group.
!# All rights reserved.
!#
!# This file is part of the hl_region High-Level HDF5 APIs.  The full copyright 
!# notice, including terms governing use, modification, and redistribution, 
!# is contained in the file COPYING, which can be found at the root of the
!# source code distribution tree and in the documentation directory (doc/html/). 
!# If you do not have access to this file, you may request a copy of 
!# "the hl_region High-Level HDF5 APIs copyright and license statement" from 
!# help@hdfgroup.org.
!# 
!##############################################################################

!
!  This example shows how to read and write bitfield
!  datatypes to a dataset.  The program first writes bit
!  fields to a dataset with a dataspace of DIM0xDIM1, then
!  closes the file.  Next, it reopens the file, and extracts
!  the bit field using a given starting bit and number
!  of bits in the bit-field. The values are returned
!  as a base-10 integer.
!
!  Main illustrative function: H5LTread_bitfield_value
!
PROGRAM main

  USE ISO_C_BINDING
  USE H5LT ! module of H5LT 
  USE HDF5 ! module of HDF5 library
  USE H5HL_REGION ! module for hl region library
  
  IMPLICIT NONE
  CHARACTER(LEN=14) :: filename = "ex_bitfield.h5"
  INTEGER, PARAMETER :: DIM0 = 7 ! dataset dimensions
  INTEGER, PARAMETER :: DIM1 = 4
  INTEGER, PARAMETER :: num_flags = 4

  
  INTEGER, DIMENSION(1:3) :: dim_qf_data =(/DIM0,DIM1,num_flags/) ! Dimensions of qf_data
  INTEGER, DIMENSION(DIM0,DIM1,num_flags) :: qf_data  ! READ buffer
  INTEGER, DIMENSION(1:4) :: offset= (/1,3,5,7/)  ! Starting bits to be extracted from element 
  INTEGER, DIMENSION(1:4) :: length= (/2,2,2,2/)  ! Number of bits to be extracted for each value 
  INTEGER(hid_t) :: file, space ! Handles
  INTEGER(hid_t) :: qf_dset
  INTEGER, PARAMETER ::  rank = 2
  INTEGER(hsize_t), DIMENSION(1:2) :: dims = (/DIM0,DIM1/), maxdims
  INTEGER(hid_t) :: file_id, sid, did
  CHARACTER(LEN=1), DIMENSION(1:DIM0,1:DIM1) :: wdata ! WRITE buffer
  INTEGER :: i, j
  INTEGER :: hdferr
  INTEGER, PARAMETER :: int_kind_1 = SELECTED_INT_KIND(1)
  INTEGER(int_kind_1) :: x
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize data.  We will manually fill four 2-bit integers into
  ! each unsigned char data element.
  !
  DO i = 1, DIM0
     DO j = 1, DIM1
        IF(BIT_SIZE(x) .NE. 8) x = 0
        CALL MVBITS(INT(j*i-2*i-j+2,KIND(wdata)),0,2,x,0)
        CALL MVBITS(INT(j-1,KIND(wdata)),0,2,x,2)
        CALL MVBITS(INT(i-1,KIND(wdata)),0,2,x,4)
        CALL MVBITS(INT(j+i-2,KIND(wdata)),0,2,x,6)
        wdata(i,j) = TRANSFER(x,wdata(i,j))
     END DO
  END DO
  !
  ! Create file with default file access and file creation properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr)
  !
  ! Write the data.
  !
  ! Create the data space for the dataset. 

  CALL H5Screate_simple_f(rank, dims, sid, hdferr)

  ! Create the dataset.

  CALL h5dcreate_f(file_id, "Granule 1", H5T_STD_U8LE, sid, did, hdferr)

  CALL h5dwrite_f(did, H5T_STD_U8LE, wdata, dims, hdferr)

  ! End access to the dataset and release resources used by it.
  CALL h5dclose_f(did, hdferr)

  ! Terminate access to the data space.
  CALL h5sclose_f(sid, hdferr)

  !
  ! close the resources.
  !
  CALL H5Fclose_f(file_id, hdferr)
  !
  ! Open file.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr)
  ! 
  ! Open the data set
  !
  CALL H5Dopen_f(file,"Granule 1", qf_dset, hdferr)
  !
  ! Get dataspace and allocate memory for read buffer. Quality flags dataset
  ! has the same dimensionality as corresponding product dataset;
  ! we are using its dimensions for illustration purposes only.
  !
  CALL H5Dget_space_f(qf_dset, space, hdferr)
  CALL H5Sselect_all_f(space, hdferr)
  
  !
  ! For each element read the value that takes first two bits and 
  ! store it in a char buffer. This selects all the elements (H5S_ALL)
  !

  CALL H5LTread_bitfield_value_f(qf_dset, num_flags, offset, length, space, qf_data, dim_qf_data, hdferr)

  CALL H5Sclose_f(space, hdferr)
  !  Print out the bit field
  DO i = 1, DIM0
     WRITE( *, '(A)', ADVANCE = "NO") "["
     DO j = 1, DIM1
        WRITE( *, '(A)', ADVANCE = "NO") " {"
        WRITE( *, '(20i2)', ADVANCE = "NO") qf_data(i,j,1:num_flags)
        WRITE( *, '(A)', ADVANCE = "NO") "} "
     ENDDO
     WRITE(*,'(A)',ADVANCE="YES") "]"
  ENDDO
  CALL H5Dclose_f(qf_dset, hdferr)
  CALL H5Fclose_f(file,hdferr)

END PROGRAM main
     
