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
!    This example creates a file and writes a two dimensional integer dataset
!    to it. Then it creates a region reference to a hyperslab.
!
!    It then reopens the file, copies data from a dataset (pointed to by a region
!    reference) to a new location and creates a new reference to it.
!    Main illustrative function: H5LRcopy_references_f
!    

PROGRAM main

  USE ISO_C_BINDING
  USE HDF5 ! module of HDF5 library
  USE H5LT ! module of H5LT
  USE H5HL_REGION ! module for hl region library

  IMPLICIT NONE

  CHARACTER(LEN=27), PARAMETER :: filename = "ex_regref_copy_reference.h5"
  CHARACTER(LEN=3), PARAMETER :: dsetname = "DS" ! dataset name 
  CHARACTER(LEN=3), PARAMETER :: dsetname_ref = "REF" ! dataset name of region references
  
  INTEGER, PARAMETER :: DIM0 = 5 ! dataset dimensions
  INTEGER, PARAMETER :: DIM1 = 4
  INTEGER, PARAMETER :: DIM_REF = 1


  INTEGER, PARAMETER :: rank=2 ! dataset rank
  INTEGER, PARAMETER :: rank_ref=1 ! region reference rank


  INTEGER(hid_t) :: file_id         ! file identifier
  INTEGER(hid_t) :: space_id_ref    ! region reference dataspace identifier
  INTEGER(hid_t) :: dset_id_ref     ! region reference dataset identifier
  INTEGER(hsize_t), DIMENSION(1:rank) :: dims =  (/DIM0, DIM1/) ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1:rank_ref) :: dims_ref =  (/DIM_REF/) ! region reference dimensions
  INTEGER(hsize_t), DIMENSION(1:4) :: block_coord_dest =(/2, 3, 4, 4 /)  ! destination hyperslab BLOCK coordinates 
  
  INTEGER(size_t) ::  numelem_size ! number of elements in hyperslab
  TYPE(hdset_reg_ref_t_f), DIMENSION(1:DIM_REF) :: ref ! buffer of region reference
  INTEGER, DIMENSION(1:DIM0, 1:DIM1) :: DATA ! DATA
  INTEGER :: i, j
  INTEGER, DIMENSION(1:DIM0, 1:DIM1) :: rdata ! reading buffers
  INTEGER, DIMENSION(1:3,1:2), TARGET :: rdata2
  TYPE(hdset_reg_ref_t_f) ::  ref_new ! region reference
  INTEGER(hsize_t), DIMENSION(1:4) :: block_coord =(/ 1, 1, 3, 2/) ! hyperslab coordinates defining region references
  CHARACTER(LEN=80), DIMENSION(1:1) :: path   ! paths to the DATA for the region references
  INTEGER :: rank_out
  INTEGER(size_t) :: numelem   ! number of coordinate blocks or selected elements
  TYPE(C_PTR) :: f_ptr
  INTEGER :: hdferr
 
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)

  path(1) = dsetname
  rank_out = 0
  numelem = 1

!*********************************************************  
!   This writes data to the HDF5 file.  
! ********************************************************

  ! 
  ! Data  and output buffer initialization. 
  !
  WRITE(*,'(/,"FULL 2D DATA:")')
  DO i= 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        DATA(i,j) = 10*(i-1)+j-1
        WRITE(*,'(1x,I3)', ADVANCE="NO") DATA(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  !
  ! Create file with default file access and file creation properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr)

  !
  ! Create and write the dataset.
  !
  CALL H5LTmake_dataset_int_f(file_id,  dsetname, 2, dims, data, hdferr)

  !
  ! Create a region reference with hyperslab coordinates: (1,1)-(3,2)
  !
  CALL H5LRcreate_region_references_f(file_id, numelem, path, block_coord, ref, hdferr)
  !
  ! Write the region references to the file
  !
  CALL H5Screate_simple_f(1, dims_ref, space_id_ref, hdferr)
  CALL H5Dcreate_f(file_id, dsetname_ref, H5T_STD_REF_DSETREG, space_id_ref,dset_id_ref, hdferr)
  CALL H5Dwrite_f(dset_id_ref, H5T_STD_REF_DSETREG, ref, dims_ref, hdferr)

  !
  ! Close/release resources.
  !
  CALL H5Sclose_f(space_id_ref, hdferr)
  CALL H5Dclose_f(dset_id_ref, hdferr)
  CALL H5Fclose_f(file_id, hdferr)

  !*************************************************************  
  ! This copies data from a dataset to a new location and creates
  ! a new reference to it.
  !************************************************************/  

  !
  ! Reopen the file to read selections back.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr)

  !
  ! copy the dataset pointed to by ref[0] into the /DS data
  ! defined by the coordinates block_coord_dest. ref_new is the region
  ! reference to this new hyperslab.
  !
  
  CALL H5LRcopy_reference_f(file_id, ref(1), filename, "/DS", block_coord_dest, ref_new, hdferr)

  ! read the copied to data set and print it
  CALL H5LTread_dataset_f(file_id,"/DS",H5T_NATIVE_INTEGER, rdata, dims, hdferr)

  WRITE(*,'(/,"FULL 2D DATA AFTER H5LRCOPY_REFERENCE:[(",I0,",",I0,")-(",I0,",",I0,")] --> [(",I0,",",I0,")-(",I0,",",I0,")])")') &
       block_coord(1:4), block_coord_dest(1:4)

  DO i = 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        WRITE(*,'(I4)', ADVANCE="NO") rdata(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  WRITE(*,'(/,"DATA POINTED TO BY NEW REGION REFERENCE")')

  ! print the data pointed to by the new region reference
  f_ptr = C_LOC(rdata2(1,1))

  CALL H5LRread_region_f(file_id, ref_new, H5T_NATIVE_INTEGER, numelem_size, f_ptr, hdferr)

  DO i = 1, block_coord_dest(3) - block_coord_dest(1) + 1
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, block_coord_dest(4) - block_coord_dest(2) + 1
        WRITE(*,'(I4)', ADVANCE="NO") rdata2(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  CALL H5Fclose_f(file_id, hdferr)

END PROGRAM main



