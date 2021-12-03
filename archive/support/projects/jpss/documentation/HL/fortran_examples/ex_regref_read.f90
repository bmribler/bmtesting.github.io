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
!    This example shows how to create, store and read data associated 
!    with region references.
!
!    It creates a file and writes a two dimensional integer dataset
!    to it. Then it creates a region reference to a subset hyperslab region of 
!    the data.
!
!    It then reopens the files, obtains information about the data associated
!    with the region reference, creates an array to store the data, and then
!    reads the hyperslab slab subset of the data and prints it to the screen.
! 
!    Additionally, it copies data pointed to by one of the region refences' into
!    a section of the data and writes the resulting data set to the screen. 
!
!    Main illustrative functions: H5LRget_region_info_f, H5LRread_region_f, H5LRcopy_region_f

PROGRAM main

  USE ISO_C_BINDING
  USE HDF5 ! module of HDF5 library
  USE H5LT ! module of H5LT
  USE H5HL_REGION

  IMPLICIT NONE

  CHARACTER(LEN=17), PARAMETER :: filename = "ex_regref_read.h5"
  CHARACTER(LEN=3) , PARAMETER :: dsetname = "/DS"        ! dataset name
  CHARACTER(LEN=4) , PARAMETER :: dsetname2 = "/DS2"        ! dataset name
  CHARACTER(LEN=6) , PARAMETER :: dsetname_ref = "MY_REF" ! ataset name of region references

  INTEGER, PARAMETER :: DIM0=5, DIM1=4 ! dataset dimensions
  INTEGER, PARAMETER :: DIM_REF=2 

  INTEGER, PARAMETER :: rank = 2 ! dataset rank 
  INTEGER, PARAMETER :: rank_ref = 1 ! region reference rank

  INTEGER(hid_t) :: file_id        ! file identifier
  INTEGER(hid_t) :: space_id_ref   ! region reference dataspace identifier
  INTEGER(hid_t) :: dset_id_ref    ! region reference dataset identifier
  INTEGER(hsize_t), DIMENSION(1:rank) ::  dims = (/DIM0, DIM1/) ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1:rank_ref) :: dims_ref = (/DIM_REF/) ! region reference dimensions
  INTEGER :: hdferr
  INTEGER, DIMENSION(DIM0, DIM1) :: data ! data
  INTEGER, DIMENSION(DIM0, DIM1) :: rdata1 ! data
  INTEGER i,j
  INTEGER(size_t) :: nlength ! size of the buffer to store the path in
  INTEGER ::  rank_out       ! the number of dimensions of the dataset pointed by region reference
  INTEGER(hid_t) :: dtype    ! datatype of the dataset pointed by the region reference
  INTEGER(hsize_t), DIMENSION(:), ALLOCATABLE :: buf     !  CONTAINS the description of the region pointed by region reference
  INTEGER(hsize_t), DIMENSION(1:2) :: rdims
  CHARACTER(LEN=80) :: name    ! full path that a region reference points to
  INTEGER(size_t) :: numelem   ! number of coordinate blocks or selected elements
  CHARACTER(LEN=80), DIMENSION(1:2) :: path  ! paths to the data for the region references

  INTEGER :: sel_type ! type of selection (hyperslab or point)

  INTEGER, DIMENSION(:,:), ALLOCATABLE, TARGET :: rdata !  buffer to read data into

  TYPE(hdset_reg_ref_t_f), DIMENSION(1:DIM_REF) :: ref ! region references
  INTEGER(hsize_t), DIMENSION(1:8) :: block_coord = (/1,1,3,2,1,1,3,2/) ! hyperslab coordinates defining region references
  INTEGER(hsize_t), DIMENSION(1:4) :: block_coord_dest = (/3,3,5,4/) ! hyperslab coordinates in the destination dataset 

  TYPE(C_PTR) :: f_ptr

  path(1) = dsetname
  path(2) = dsetname2

  numelem = 2

!*********************************************************  
!   This writes data to the HDF5 file.  
!*********************************************************
     
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)
  ! 
  ! Data initialization. 
  !
  WRITE(*,'(A)') "FULL 2D ARRAY:"

  DO i = 1, dim0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, dim1
        DATA(i,j) = 10*(i-1)+(j-1)
        WRITE(*,'(I4)', ADVANCE="NO") DATA(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO
  !
  ! Create file with default file access and file creation properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F,  file_id, hdferr)
  !
  ! Create and write datasets.
  !
  CALL H5LTmake_dataset_int_f(file_id,  dsetname, 2, dims, DATA, hdferr)
  CALL H5LTmake_dataset_int_f(file_id,  dsetname2, 2, dims, DATA, hdferr)
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
!
!  This reads the hyperslab pointed to by a region reference
!
! ************************************************************

  !
  ! Reopen the file to read selections back.
  !

  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr)

  nlength = 80
  rank_out = 0
  numelem = 0

  ! Obtain information about the data associated with the region reference

  CALL H5LRget_region_info_f(file_id, ref(1), hdferr, &
       LENGTH=nlength, RANK=rank_out, SEL_TYPE=sel_type, NUMELEM=numelem)

  ! Allocate space for hyperslab block:
  !              | NUMBLOCKS |   RANK    | NUMBER COORDINATE POINTS |
  ALLOCATE( buf(    numelem  * rank_out  *         2                ) )

  ! Get region reference information, hyperslab coordinates returned in buf
  CALL H5LRget_region_info_f(file_id, ref(1), hdferr, &
       LENGTH=nlength, PATH=name, RANK=rank_out, DTYPE=dtype, SEL_TYPE=sel_type, NUMELEM=numelem, BUF=buf)

  rdims(1) = buf(3) - buf(1) + 1
  rdims(2) = buf(4) - buf(2) + 1

  !
  ! Allocate space for integer data. 
  !
  ALLOCATE( rdata(1:rdims(1),1:rdims(2)) )

  ! Read a region of the data using a region reference and print it.

  f_ptr = C_LOC(rdata(1,1))

  CALL H5LRread_region_f(file_id, ref(1), dtype, numelem, f_ptr, hdferr)
 
  WRITE(*,'(/,"REGION 2D HYPERSLAB BY CORNER COORDINATES (",i1,",",i1,")-(",i1,",",i1,"):")') buf(1:4)

  DO i = 1, rdims(1)
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, rdims(2)
        WRITE(*,'(I4)', ADVANCE="NO") rdata(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  DEALLOCATE(rdata)

  ! copy a region reference's data into a block of data
  CALL H5LRcopy_region_f(file_id, ref(1), filename, "/DS2", block_coord_dest, hdferr)

  ! print new data after the region reference was copied 
  CALL H5LTread_dataset_f(file_id,"/DS2",H5T_NATIVE_INTEGER, rdata1, dims, hdferr)
    
  WRITE(*,'(/,"AFTER COPYING REG REF TO BLOCK COORDINATES (",i1,",",i1,")-(",i1,",",i1,"):")') block_coord_dest(1:4)
  DO i = 1, dim0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, dim1
        WRITE(*,'(I4)', ADVANCE="NO") rdata1(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  CALL H5Fclose_f(file_id, hdferr)

END PROGRAM main



