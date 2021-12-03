!
!#############################################################################
! 
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
!    This examples creates a file and writes a two dimensional real dataset
!    to it.
!
!    It then reopens the file, and copies a region described by blocks 
!    to another region described by another block.
!    Main illustrative function: H5LTcopy_region
 
PROGRAM main

  USE ISO_C_BINDING
  USE H5LT ! module of H5LT library
  USE HDF5 ! module of HDF5 library
  USE H5HL_REGION ! module for hl region library

  CHARACTER(LEN=22) :: filename = "ex_lite_copy_region.h5"
  CHARACTER(LEN=2)  :: DSETNAME = "DS" ! dataset name
  
  INTEGER, PARAMETER :: DIM0 = 6 ! dataset dimensions
  INTEGER, PARAMETER :: DIM1 = 7

  INTEGER, PARAMETER :: RANK = 2 ! dataset RANK

  INTEGER(HID_T) :: file_id ! file identifier
  INTEGER(HSIZE_T), DIMENSION(1:RANK) :: dims = (/DIM0, DIM1/) ! dataset dimension

  INTEGER, DIMENSION(DIM0,DIM1) :: DATA ! data
  INTEGER :: i, j
  
  INTEGER(hsize_t), DIMENSION(1:4) :: block_coord_src =(/1,1,2,3/)  ! hyperslab coordinates to copy from
  INTEGER(hsize_t), DIMENSION(1:4) :: block_coord_dest=(/3,4,4,6/) ! destinations block coordinates to copy data to

  INTEGER, DIMENSION(1:DIM0,1:DIM1) :: rdata ! read data buffer
    
  INTEGER :: hdferr

!*********************************************************  
!   This writes data to the HDF5 file.  
!********************************************************* 
 
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)

  ! 
  ! Data  and output buffer initialization. 
  !
  WRITE(*,'(/,"FULL 2D ARRAY:")')
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
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F,  file_id, hdferr)
  !
  ! Create and write the dataset.
  !
  CALL H5LTmake_dataset_int_f(file_id, DSETNAME, 2, dims, DATA, hdferr)

  !
  ! Close/release resources.
  !
  CALL H5Fclose_f(file_id, hdferr)

!*************************************************************  
!
!  This copies a region described by blocks to another region 
!  described by another block
!
!*************************************************************

  CALL H5LTcopy_region_f(filename, "/DS", block_coord_src, &
       filename, "/DS", block_coord_dest, hdferr)

  ! Read and print the destination region
  
  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr)

  CALL H5LTread_dataset_f(file_id,"/DS",H5T_NATIVE_INTEGER, rdata, dims, hdferr);

  CALL H5Fclose_f(file_id, hdferr)

  WRITE(*,'(/,"2D DATA AFTER H5LTCOPY_REGION: (",i1,",",i1,")-(",i1,",",i1,") --> (",i1,",",i1,")-(",i1,",",i1,")")') &
       block_coord_src(1:4),block_coord_dest(1:4)

  DO i = 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        WRITE(*,'(1X,I3)', ADVANCE="NO") rdata(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

END PROGRAM main



