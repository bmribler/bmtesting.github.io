---
title: Software+Changes+from+Release+to+Release+for+HDF5+1.8
redirect_from: 
  - display/HDF5/Software+Changes+from+Release+to+Release+for+HDF5+1.8
---

# Software Changes from Release to Release in HDF5 1.8

This document is designed to help developers maintain HDF5-based applications as they are used with successive releases of HDF5. To that end, this document lists the changes at each release that a maintenance developer needs to be aware of:

New or changed features or tools
Syntax and behavioral changes in the existing application programming interface (the API)
Certain types of changes in configuration or build processes
Bugfixes and performance enhancements in the C library are automatically picked up by the C++ and Fortran libraries. The Java library will pick up the same bugfixes and enhancements at its next subsequent release as it is always built with the latest released version of the C library.

The release notes also list changes made to the library, but these notes tend to be more at a more detail oriented level. The release notes may include new features, bugs fixed, supported configuration features, platforms on which the library has been tested, and known problems. The release note files are listed below and can be found at the top level of the HDF5 source code tree in the release_docs directory of the distributed HDF5 source code.

|                          |                                                              |
| ------------------------ | ------------------------------------------------------------ |
| RELEASE.txt | Technical notes regarding the current release of the HDF5 library |
| HISTORY-1_8.txt | Technical notes for previous 1.8 releases of the HDF5 library by release |
| HISTORY-1_0-1_8_0_rc3.txt | Technical notes for releases prior to the 1.8.0 release of the HDF5 library by release |

#### Are you porting an existing application from HDF5 Release 1.6.x to HDF5 Release 1.8.x?
If you are porting an existing application from any HDF5 version in the Release 1.6.x series to any version in the Release 1.8.x series, refer to "What's New in HDF5 1.8.0".  This document, with the documents linked in its introduction, provides a guide to the changes introduced in the transition from the 1.6.x series to HDF5 Release 1.8.0 and discussions of various compatibility issues.

## Release 1.8.23 of February 2023 versus Release 1.8.22
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.22 to Release 1.8.23.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

None.

In the C High Level Interface

None.

In the C++ Wrapper

None.

Compatibility Notes and Report and Comments
The interface compatibility report provides a full list of the changed symbols.

## Release 1.8.22 of February 2021 versus Release 1.8.21
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.21 to Release 1.8.22.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following are new C functions in this release:

|                          |                                                              |
| ----------------- | ------------------------------------------------------------ |
| H5P_GET_FAPL_HDFS | Gets the information of the given Read-Only HDFS virtual file driver |
| H5P_GET_FAPL_ROS3 | Gets the information of the given Read-Only S3 virtual file driver |
| H5P_SET_FAPL_HDFS | Sets up Read-Only HDFS virtual file driver |
| H5P_SET_FAPL_ROS3 | Sets up Read-Only S3 virtual file driver |

In the C High Level Interface

None.

In the C++ Wrapper

None.

Compatibility Notes and Report and Comments
The interface compatibility report provides a full list of the changed symbols.

## Release 1.8.21 of May 2018 versus Release 1.8.20
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.20 to Release 1.8.21.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

None.

In the C High Level Interface

None.

In the C++ Wrapper

Many changes were made to improve the C++ API model. Please review the RELEASE.txt file or the compatibility report below for complete information on the C++ changes in this release.

Compatibility Notes and Report and Comments
The interface compatibility report provides a full list of the changed symbols.

Release 1.8.20 of November 2017 versus Release 1.8.19
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.19 to Release 1.8.20.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

None.

In the C High Level Interface

None.

In the C++ Wrapper

Many changes were made to improve the C++ API model. Please review the RELEASE.txt file or the compatibility report below for complete information on the C++ changes in this release.

Compatibility Notes and Report and Comments
Compatibility report for Release 1.8.20 versus Release 1.8.19

The following interface compatibility report provides a full list of the changed symbols:

API compatibility report for the HDF5 Library between 1.8.19 and 1.8.20 versions on x86_64

Release 1.8.19 of June 2017 versus Release 1.8.18
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.18 to Release 1.8.19.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following new C functions were added:

H5Dget_chunk_storage_size

H5PLappend

H5PLget

H5PLinsert

H5PLprepend

H5PLremove

H5PLreplace

H5PLsize
 

In the C High Level Interface

The following new C function was added:

H5DOread_chunk
 

In the C++ Wrapper

Many changes were made to improve the C++ API model. Please review the RELEASE.txt file or Compatibility report below for complete information on the C++ changes in this release.

Compatibility Notes and Report and Comments
Compatibility report for Release 1.8.19 versus Release 1.8.18

The following interface compatibility report provides a full list of the changed symbols:

API compatibility report for the HDF5 Library between 1.8.18 and 1.8.19 versions on x86_64

Release 1.8.18 of November 2016 versus Release 1.8.17
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.17 to Release 1.8.18.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

None.

In the C++ Interface (main library wrappers)

The macro H5_NO_NAMESPACE was deprecated.

A number of new member functions were added or removed.  See RELEASE.txt for detail. 

Compatibility Notes and Report and Comments
Compatibility report for Release 1.8.18 versus Release 1.8.17

The following interface compatibility report provides a full list of the changed symbols:

API compatibility report for the HDF5 Library between 1.8.17 and 1.8.18 versions on x86_64
 

## Release 1.8.17 of May 2016 versus Release 1.8.16

This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.16 to Release 1.8.17.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following new C functions and environment variable enable setting a relative path for raw data files associated with datasets using external storage:

Functions:

H5Pset_efile_prefix
H5Pget_efile_prefix
 

Environment variable:   

HDF5_EXTFILE_PREFIX

In the C High-Level Interface

The following new functions have been added in the Packet Table (H5PT) high-level interface:

H5PTcreate
H5PTget_dataset
H5PTget_type

H5PTcreate replaces H5PTcreate_fl, which is deprecated and may be removed in a future release.

The following functions were previously removed from the library distribution but are once again publicly available.

Reinstated in its original form:

H5PTis_varlen

An updated form of H5PTfree_vlen_readbuff reinstated as:

H5PTfree_vlen_buff

In the C++ Interface (main library wrappers)

New member function added in DSetCreatPropList:

DSetCreatPropList::setNbit() was added to set up N-bit compression for a dataset.

New overloaded const member functions added in ArrayType:

The following functions were added to provide const versions; the non-const versions have been marked deprecated.

 
    ArrayType::getArrayNDims() const
    ArrayType::getArrayDims() const
                    
The datamembers ArrayType::rank and ArrayType::dimensions have been removed. This is an implementation detail and should not affect applications.

In the C++ High-Level Interface

New constructor added in FL_PacketTable:

An overloaded constructor has been added to FL_PacketTable. The function now takes a property list identifier to provide flexibility on creation properties such as compression.

 
    FL_PacketTable(hid_t fileID, const char* name, hid_t dtypeID,
                   hsize_t chunkSize = 0, hid_t plistID = H5P_DEFAULT)
                    
New member functions added in PacketTable:

Two accessor wrappers were added to class PacketTable.

PacketTable::GetDataset() returns the identifier of the dataset associated with the packet table.

PacketTable::GetDatatype() returns the identifier of the datatype that the packet table uses.

New member functions added with a char* argument:

Overloaded functions have been added to provide a const char* argument. The existing versions of these functions will be deprecated in future releases.

This also removes the following warning:

 
    deprecated conversion from string constant to "char*" [-Wwrite-strings]
                    
Ifdef blocks removed:

The #ifdef VLPT_REMOVED blocks have been removed from the packet table library source code except for the following functions:

VL_PacketTable::IsVariableLength() was moved to PacketTable.

VL_PacketTable::FreeReadBuff() is now PacketTable::FreeBuff().

Compatibility Notes and Report
Changed shared library interface version numbers (soname)

For HDF5 Release 1.8.17, the shared object version numbers have changed as follows.

Library

Release
1.8.16     

Release
1.8.17     

Reason for Change

HDF5 C Library

10.1.0

10.2.0

Added APIs

HDF5 High-level C Library  

10.0.2

10.1.0

Added APIs

C++ Library

11.0.0

12.0.0

Changes in existing APIs

High-level C++ Library

11.0.0

11.1.0

Added APIs

All others

10.0.2

10.0.3

Code changes that did not result in interface changes

Compatibility report for Release 1.8.17 versus Release 1.8.16

The following interface compatibility report provides a full list of the changed symbols:

API compatibility report for the HDF5 Library between 1.8.16 and 1.8.17 versions on x86

Release 1.8.16 of November 2015 versus Release 1.8.15
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.15 to Release 1.8.16.

### New and Changed Functions, Classes, Subroutines, Wrappers, and Macros

#### In the C Interface (main library)

New HDF5 Plugin Interface (H5PL)

A new HDF5 Plugin Interface (H5PL), enabling programmatic control of dynamically loaded plugins, was introduced at HDF5 Release 1.8.15. This interface was only partially documented at that time, but more complete documentation is now available:

* Dynamic Plugins in HDF5 in Advanced Topics in HDF5
* H5PL: Plugin Interface in the HDF5 Reference Manual
  * H5PLset_loading_state
  * H5PLget_loading_state

The following new C function has been added:

This function reports whether the linked version of the HDF5 Library was built with the thread-safety feature enabled.

* H5is_library_threadsafe
The following flag has been removed:

H5F_ACC_DEBUG flag for H5Fopen/create
 
The flag H5F_ACC_DEBUG was used to provide extra debugging information for HDF Library developers. This flag and the underlying functionality have been removed from the library due to disuse. The symbol remains defined since it was visible in H5Fpublic.h, but it has been set to zero (0) and has no effect anywhere in the library.

* H5Fopen

#### In the C++ Interface

The following features have been added:

New class H5::ObjCreatPropList for the object creation property list class
The new class H5::ObjCreatPropList is derived from H5::PropList and is a baseclass of H5::DSetCreatPropList. Additional property list classes will be added to the library in future releases and some of those will be derived from H5::ObjCreatPropList.

New wrappers for the C functions H5P[s/g]et_attr_phase_change and H5P[s/g]et_attr_creation_order
The following wrappers were added to the class H5::ObjCreatPropList for the respective C functions:

| C Function                 | C++ Wrapper                                             |
| -------------------------- | ------------------------------------------------------- |
| H5Pset_attr_phase_change   | H5::ObjCreatPropList::setAttrPhaseChange                |
| H5Pget_attr_phase_change   | H5::ObjCreatPropList::getAttrPhaseChange                |
| H5Pset_attr_creation_order | H5::ObjCreatPropList::setAttrCrtOrder                   |
| H5Pget_attr_creation_order | H5::ObjCreatPropList::getAttrCrtOrder                   |

### Library Configuration

Use of thread-safety with the HDF5 High-level Library has been declared unsupported in the build tools: Autotools and CMake.

The global lock used by the thread-safety mechanism is not raised to the high-level library level, making it possible that the library state could change if a context switch were to occur in a high-level library call. Because of this, the combination of thread-safety and the high-level library is officially unsupported by The HDF Group.

Though this combination has never been supported in HDF5, this was not enforced by the build systems. With this release, the combination has been explicitly flagged as unsupported in Autotools and CMake, the build systems used with HDF5.

The new unsupported declaration will cause an Autotools configure step to fail if --enable-threadsafe and --enable-hl are combined unless additional options are specified (see below). Since the high-level library is built by default, this means that these extra configuration options will need to be used any time --enable-threadsafe is selected.

To use Autotools to build an application with --enable-threadsafe, use one of the following approaches:

* Use --disable-hl to disable the high-level library. (Recommended)
* Use --enable-unsupported to build the high-level library with the thread-safety feature.

To use CMake to build an application with HDF5_ENABLE_THREADSAFE=ON, use one of the following approaches:

* Use HDF5_BUILD_HL_LIB=OFF to disable the high-level library. (Recommended)
* Use HDF5_ALLOW_UNSUPPORTED=ON to build the high-level library with the thread-safety feature.

Since this combination is unsupported, applications using it must be carefully tested for potential thread-safety failures.

### Compatibility Notes and Report

#### Decoupled shared object version numbers (soname)

Shared object version numbers, soname, for HDF5 wrapper libraries, such as the C++ and Fortran libraries, have been decoupled from the shared object version number for the HDF5 library. In this and future releases, each library's soname will be maintained on an individual basis according to the interface changes specific to each wrapper library.

#### Changed shared library interface version numbers (soname)

For HDF5 Release 1.8.16, the shared object version numbers have changed as follows.

| Library                   | Release 1.8.15  | Release 1.8.16  | Reason for Change |
| ------------------------- | --------------- | --------------- | ----------------- |
| HDF5 Library  | 10.0.1 | 10.1.0 | Added APIs
| C++ Library | 10.0.1 | 11.0.0 | Changes in existing APIs
| High-level C++ Library | 10.0.1 | 11.0.0 | Changes in existing APIs
| All others | 10.0.1 | 10.0.2 | Code changes that did not result in interface changes

### Compatibility report for Release 1.8.16 versus Release 1.8.15

The following interface compatibility report provides a full list of the changed symbols:

API compatibility report for the HDF5 Library between 1.8.15 and 1.8.16 versions on x86

## Release 1.8.15 of May 2015 versus Release 1.8.14

This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.14 to Release 1.8.15.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following new C functions have been added:

Memory allocation functions that use the library's allocator.

H5allocate_memory
H5resize_memory
The following functions enable programmatic control of the loading of dynamic plugin filters in user applications:

H5PLset_loading_state
H5PLget_loading_state
For preliminary documentation, see New HDF5 APIs Provide Programmatic Control of Dynamic Plugins.
Final documentation is anticipated in July 2015.
In the C++ Interface

The following features have been added:

New wrappers for the C functions H5Pset_libver_bounds and H5Pget_libver_bounds
The following wrappers have been added to class H5::FileAccPropList. The first sets the bounds on versions of the HDF5 Library format to be used when creating or writing objects; the second retrieves the current settings:

 
    void setLibverBounds(H5F_libver_t libver_low, H5F_libver_t libver_high) const;
 
    void getLibverBounds(H5F_libver_t& libver_low, H5F_libver_t& libver_high) const;
                    
New wrappers to retrieve the version of an object header
The following wrappers have been added to class H5::CommonFG to retrieve the object header version of an object in a file or group, given the object's name.

 
    unsigned childObjVersion(const char* objname) const;
 
    unsigned childObjVersion(const H5std_string& objname) const;
                    
A new DataType constructor
A new DataType constructor takes a PredType object and invokes H5Tcopy to generate another datatype identifier from a predefined datatype.

The following features have been changed:

Combined H5File::getObjCount overloaded methods
The following methods:

 
    ssize_t getObjCount(unsigned types) const;
 
    ssize_t getObjCount() const;
                   
have been combined into the following:

 
    ssize_t getObjCount(unsigned types = H5F_OBJ_ALL) const;
                   
Deprecated constructors
The following two constructors of the classes AbstractDs, IdComponent, H5Location, and H5Object are no longer appropriate after the data member id was moved from IdComponent to the sub-classes in previous releases:

 
    <Classname>(const hid_t h5_id);
 
    <Classname>(const <Classname>& original);
                    
The copy constructors have been no-ops and are removed in this HDF5 Release 1.8.15. The other constructors will be removed in HDF5 Release 1.10 and are expected to be removed from HDF5 Release 1.8.17 if their removal causes no problems.

In the Fortran Interface

The following Fortran 2003 API has been added:

h5tenum_insert_f

Operation of the new Fortran 2003 version of this API is more consistent with the operation of the C API than was the Fortran 90 version.

Users are strongly encouraged to use the Fortran 2003 API.

Several global variables have been added in the Fortran API. These global variables are equivalent in definition to the comparable C definitions:

 
H5G_UDLINK_F
H5G_SAME_LOC_F
H5O_TYPE_UNKNOWN_F
H5O_TYPE_GROUP_F
H5O_TYPE_DATASET_F
H5O_NAMED_DATATYPE_F
H5O_TYPE_NTYPES_F   
For parallel applications

A fix for issues encountered upon calling MPI_Finalize without having closed everything in an HDF5 file

An attribute destroy callback has been attached to MPI_COMM_SELF that shuts down the HDF5 Library when MPI_COMM_SELF is destroyed, that is, on MPI_Finalize. This should fix several issues that users see when they forget to close HDF5 objects before calling MPI_Finalize().

Compatibility Report and Comments
Compatibility report for Release 1.8.15 versus Release 1.8.14

API compatibility report for the HDF5 Library between 1.8.14 and 1.8.15 versions on x86

Changed shared library interface version number (soname)

In the C++ interface, two overloaded methods have been combined into a single method and one constructor has been deprecated.

Due to these changes, the shared library interface version number (soname) has been increased. For a full list of the changed symbols, see the interface compatibility report linked immediately above.

## Release 1.8.14 of November 2014 versus Release 1.8.13
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.13 to Release 1.8.14.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the main library C Interface

No new C functions have been added.

The behavior of the following C function has been changed:

The function H5Pset_core_write_tracking has been modified to enforce the limitation that the value of the page_size parameter cannot equal 0 (zero):

H5Pset_core_write_tracking
The following performance optimization has been implemented in the Parallel HDF5 library:

The algorithm for writing chunk fill values for new datasets has been optimized to write out all chunk fill values collectively in a single MPI-IO call. This was previously done independently by rank 0 (zero) one block at a time. This optimization should produce a significant performance improvement when creating chunked datasets in parallel, most notably when chunk dimensions are relatively small. This optimization will be evident in use of the following function:

H5Dcreate
In the high-level library C Interface

High-level HDF5 interfaces now ensure that strings are null-terminated and will not overflow a buffer.

In several high-level functions, the type from the file was previously re-used as the memory type and no check is made to ensure that a string was actually null-terminated.

All high-level functions now check for NULL pointers; verify that a received a string is null-terminated; and verify that the string will not overflow the buffer. The minimum length of the buffers is now used in strncmp to avoid overflow.

In the main library C++ Interface

No new C++ methods have been added.

The following changes have been made in the C++ interface:

Changed defaults

Default value of class member id:
The data member id in several classes that represent HDF5 objects are now initialized to H5I_INVALID_ID.
Default value of a PropList member id:
The data member id in class PropList is now initialized to H5P_DEFAULT.
In HDF5 Release 1.8.13 and earlier, the above were initialized to 0 (zero).
Changed file access flags

The flag H5F_ACC_CREAT has been removed from the C++ interface.
In the main library Fortran Interface

No new Fortran subroutines have been added.

Changed Fortran INTEGER parameter types:

Types of the following Fortran parameter constants have been changed from INTEGER to INTEGER(HID_T):

 
    H5FD_CORE            H5FD_MULTI
    H5FD_FAMILY          H5FD_SEC2
    H5FD_LOG             H5FD_STDIO
    H5FD_MPIO       
Other internal int types have been changed to hid_t, but this change is expected to be transparent to the user.

Compatibility Report and Comments
Compatibility report for Release 1.8.14 versus Release 1.8.13

API compatibility report for the HDF5 Library between 1.8.13 and 1.8.14 versions on x86

Changed shared library interface version number (soname)

To increase the maintainability of HDF5, an architectural change has been made that required renaming several public symbols in H5Ppublic.h.

Due to these changes, the shared library interface version number (soname) has been increased. For a full list of the changed symbols, see the interface compatibility report linked immediately above.

 

## Release 1.8.13 of May 2014 versus Release 1.8.12
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.12 to Release 1.8.13.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following new C functions have been added:

To free memory allocated by HDF5 function calls:

H5free_memory
This function should be used to free memory that has been allocated by HDF5 API calls such as H5Tget_member_name and H5Pget_class_name.
The primary value for this call is on Windows systems, where it is common for application code and the HDF5 Library to be using different C run-time libraries (CRT). Using this new call ensures that the same CRT handles both the allocation and free.

This function can also be useful in any case where the library uses a different memory manager than the application, such as when a debug memory manager is in use or when the HDF5 Library is wrapped for use in a managed language like Python or Java.

New configuration property for the CORE driver:

H5Pset_core_write_tracking
H5Pget_core_write_tracking
With this property, the CORE driver, H5Pset_fapl_core, can be configured to track dirty regions in the file and write out only the changed regions on flush or close operations. Additionally, a page aggregation size can be set that will aggregate small writes into larger writes. For example, setting a 1 mebibyte (1 MiB) page aggregation size will logically partition the the in-memory file into 1 MiB pages that will be written out in their entirety if even a single byte is dirtied.
The following C functions have been modified:

H5Rget_name
If the name parameter is passed as NULL, then the size parameter is ignored and the function returns the size of the buffer required to read the name (not including the NULL terminator).
H5Zfilter_avail
This function has been updated to correctly identify dynamically loaded filters. If a filter is not found among registered filters, the function now checks for dynamically loaded filters.
In the C++ Interface

The following new features have been added:

Wrappers to the class H5Object to get an object's name:

 
    ssize_t getObjName(char *obj_name, size_t buf_size = 0)
    ssize_t getObjName(H5std_string& obj_name, size_t len = 0)
    H5std_string getObjName()
Wrappers to the class H5CommonFG to get a child object's type from a group or file:

 
    H5O_type_t childObjType(const H5std_string& objname)
    H5O_type_t childObjType(const char* objname)
    H5O_type_t childObjType(hsize_t index,
        H5_index_t index_type=H5_INDEX_NAME,
        H5_iter_order_t order=H5_ITER_INC, const char* objname=".")
Wrappers to the class DSetMemXferPropList for setting or getting a transform property list:

 
    DSetMemXferPropList(const char* expression);
    void setDataTransform(const char* expression)
    void setDataTransform(const H5std_string& expression)
    ssize_t getDataTransform(char* exp, size_t buf_size=0)
    H5std_string getDataTransform()
Wrapper to CompType for setting the size of a compound datatype:

 
    void setSize(size_t size)
Overloaded functions to provide prototypes that declare constant arguments const:

 
    DataSet::fillMemBuf
    DataSet::getVlenBufSize
    DataSpace::extentCopy
    DataType::commit
    FileAccPropList::setSplit
    H5File::getVFDHandle
Additional overload to the class H5Location to get a comment as a char*:

 
    ssize_t getComment(const char* name, size_t buf_size, char* comment) 
Additional overloads to the class Attribute to get an attribute's name for convenience:

 
    ssize_t getName(char* attr_name, size_t buf_size=0)
    ssize_t getName(H5std_string& attr_name, size_t buf_size=0)
In the Fortran Interface

The following new Fortran subroutines have been added:

Fortran2003 subroutines:

h5pset_file_image_f
h5pget_file_image_f

 

h5fget_file_image_f (Added in Release 1.8.11, but not documented at that time.)

The following Fortran subroutine has been changed:

h5pset_chunk_cache_f
The values of the constants H5D_CHUNK_CACHE_NSLOTS_DFLT_F and H5D_CHUNK_CACHE_NBYTES_DFLT_F have been changed to INTEGER(KIND=size_t).
Other New Features
Parallel I/O support:

Parallel I/O support has been enhanced to allow collective I/O on point selections.

Thread safety (Windows only)

The thread-safe HDF5 library no longer leaks thread-local storage resources on Windows with Win32 threads. This cleanup can only be performed when the library is built as a shared library, so a thread-safe HDF5 static library with Win32 threads is no longer supported.

This change affects only Windows environments.

Also see [Build and Configuration](#Build-and-Configuration) below.

Command-line Tools with Changed Behavior
h5repack

h5repack has been updated to correctly handle dynamically loaded filters.

### Build and Configuration
Thread safety

When configuring a thread-safe HDF5 Library with --enable-threadsafe, it is no longer necessary to specify --with-pthreads if the Pthreads library is in a standard location.

### Removed Functions and Wrappers
The MPI-POSIX driver has been removed. The following C functions and the corresponding Fortran subroutines and C++ wrappers therefore are no longer included in the HDF5 distribution:

H5Pset_fapl_mpiposix
H5Pget_fapl_mpiposix
Applications performing parallel I/O should use the MPI-IO driver, H5Pset_fapl_mpio.

## Release 1.8.12 of November 2013 versus Release 1.8.11
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.11 to Release 1.8.12.

New and Changed Functions, Classes, Subroutines, Wrappers, and Macros
In the C Interface (main library)

The following C functions have been modified:

H5Zunregister now checks for open objects using a filter before allowing the filter to be unregistered.
H5Dscatter and H5Dgather signatures have changed slightly.
The src_buff parameter type is now 'const void *'  (previously: 'void *').
In the C++ Interface

The following new C++ class has been added:

H5::H5Location

The class H5::H5Location has been added as a base class of H5File and H5Object. It represents the location concept in the C library, where an identifier of an HDF5 object or an HDF5 file being passed in as a loc_id parameter.

The following member functions and their overloaded versions have been moved from various subclasses into the base class H5Location:

createAttribute()
openAttribute()
attrExists()
renameAttr()
removeAttr()
flush()
getFileName()
getNumAttrs()

        

getObjType()
iterateAttrs()
setComment()
getComment()
removeComment()
reference()
dereference()
getRegion()

The following wrappers for C functions have been added:

H5Location::getRefObjType

Corresponding C function: H5Rget_obj_type2

Retrieves the type of the object that an object reference points to.

H5Location::attrExists

Corresponding C function: H5Aexist

Determines whether a given attribute exists.

H5Location::setComment

Two overloaded versions of H5Location::setComment have been added to take "." in lieue of the name of the object.

In the classes DataSet, DataType, and Group, the following constructors have changed as follows:

These constructors
    DataSet(H5Object& obj, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataSet(H5File& h5file, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataSet(Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
have changed to
    DataSet(const H5Location& loc, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataSet(const Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
These constructors
    DataType(H5Object& obj, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataType(H5File& h5file, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataType(Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
have changed to
    DataType(const H5Location& loc, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    DataType(const Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
                                                                                        
These constructors
    Group(H5Object& obj, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    Group(H5File& h5file, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    Group(Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
have changed to
    Group(const H5Location& loc, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
    Group(const Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
New Command-line Tool Options and Option Parameters
h5dump

h5dump has a new option for identifying objects by a full or partial path:
   -NP
   --any-path=P
 

The h5dump attributes-only option, -A or --only-attr, now accepts an optional parameter value of 0 (zero):
   -A 0
   --only-attr=0
Specifying this value has the effect of reversing the role of the option to display everything except attributes.
 

h5repack

The h5repack option -f, --filter has a new parameter, UD, to enable the use of user-defined filters:
   -f UD=<filter_parameters>
   --filter=UD=<filter_parameters>
 

h5stat

h5stat has three new options for setting print thresholds:
   For attributes:
      -aN
      --attribute=N
 
   For links in a group:
      -lN
      --links=N
 
   For dimension sizes:
      -mN
      --dims=N

Build and Configuration
CMake and Autotools configuration

A new configuration option can be used to change the default plugin path:

   

Autotools configure option

    

--with-default-plugindir=location

 

cmake option

    

-DH5_DEFAULT_PLUGINDIR:PATH=location

CMake configuration

CMake builds of HDF5 now create *.so.{lt_version} files with the same version as configure.

Autotools configure flag

The configure flag FFLAGS has been renamed to FCFLAGS.

Compile Scripts with Changed Behavior
h5cc and h5pcc

h5c++

h5fc and h5pfc

These compile scripts have been modified to change the default behavior to link to shared libraries in environments where HDF5 has been configured with --disable-static.

Command-line Tools with Changed Behavior
h5dump

h5dump output now displays
    USER_DEFINED_FILTER
where it previously displayed UNKNOWN_FILTER.
 

h5dump now correctly exports subsetted data to a file using the -o, --output option.

## Release 1.8.11 of May 2013 versus Release 1.8.10
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.10 to Release 1.8.11.

New and Changed Functions and Subroutines
The following new high-level C function has been added:

H5DOwrite_chunk
This function writes a data chunk directly into a file, bypassing hyperslab selection, data conversion, and the filter pipeline. The user must be careful with the function and clearly understand the I/O process of the library. See [Direct Chunk Write](Direct Chunk Write) for additional information.

H5DOwrite_chunk is the first function in HDF5's new high-level [HDF5 Optimized Functions](HDF5 Optimized Functions) library.

The following new C functions have been added:

H5Dscatter
H5Dgather
These functions scatter data to and gather data from a selection within a memory buffer.

The following new Fortran subroutines have been added:

Fortran90 subroutines:

h5odecr_refcount_f
h5oexists_by_name_f
h5oget_comment_f
h5oget_comment_by_name_f
h5oincr_refcount_f
h5oopen_by_idx_f
h5oset_comment_f
h5oset_comment_by_name_f
 

h5oclose_f was introduced in HDF5 Release 1.8.8 but remained undocumented until this release.
 

Fortran2003 subroutines:

h5oget_info_f
h5oget_info_by_idx_f
h5ovisit_by_name_f

### New Feature: Dynamically Loaded Filters
HDF5 now detects and dynamically loads external filters, reducing the need for an application to register custom filters or for a developer building on HDF5 to compile a custom filter into a customized HDF5 distribution.

See [HDF5 Dynamically Loaded Filters](HDF5 FDynamically Loaded Filter) (PDF only) for further information.

### New Command-line Tool Option
h5dump and h5import

h5dump has a new option to output the DDL structure of a file or object:
   -O\<filename\>
   --ddl=\<file_name\>
[-O above is dash-uppercase-o.]

h5import can now take the output of
   h5dump -O\<ddl_filename\> -o\<data_filename\>
directly as input files.
See [Using h5dump to create input for h5import](Using h5dump to create input for h5import) in the h5import reference manual entry.

### New Configuration and Build Tool

cmakehdf5

This new build command script is similar to buildhdf5 and is available in the bin/ directory of the distributed source code.

For HDF5, the CMake minimum is now 2.8.10.

### Function, Subroutines, and Feature with Changed Syntax or Behavior
The return value of the following high-level C function has changed:

H5DSget_scale_name
This function has been changed to not include the NULL terminator in the returned size of of the dimension scale name (bug fix).

In the Fortran subroutines

h5pset_external_f and
h5pget_external_f
the type of the offset parameter has changed to INTEGER(KIND=OFF_T) to support 8-byte integers, matching the C type definition of off_t.

The HDF5 Library now supports data conversion from enumeration to integer and floating-point numeric datatypes.

### Command-line Tools with Changed Behavior

h5diff

When comparing two files or objects, h5diff now recognizes a unique extra attribute attached to an object as a difference and returns with an exit code of 1.

Previously, h5diff did not recognize the extra attribute as a difference.

h5ls

The content of the h5ls --help output has been revised.

h5dump

h5dump has been updated to display the compression ratio for user-defined filters.

In h5dump output, UNKNOWN_FILTER has been changed to USER_DEFINED_FILTER.

### Removed Functions and Wrappers
The following C functions and the corresponding C++ wrappers have been removed:

H5Pset_dxpl_multi
H5Pget_dxpl_multi
 
H5::DSetMemXferPropList:setMulti
H5::DSetMemXferPropList:getMulti

## Release 1.8.10 of November 2012 versus Release 1.8.9
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.9 to Release 1.8.10.

New and Changed Functions and Subroutines
The following new C function has been added:

H5Pget_mpio_no_collective_cause
The following new Fortran subroutine has been added:

h5pget_mpio_actual_io_mode_f
The syntax of the following high-level C function has changed:

H5TBAget_fill
This function's return type has been changed to h5tri_t. A return value of 1 indicates that a fill value is present, 0 indicates that no fill value is present, and a negative value indicates an error. (The former return type was herr_t.)
Note that H5TBAget_fill and a companion function H5TBAget_title, both in the HDF5 Table (H5TB) high-level API, are undocumented. These functions will be added to the HDF5 Reference Manual shortly after HDF5 Release 1.8.10.

Changed Command-line Tool
h5import has been updated to accept an input file created by h5dump.

## Release 1.8.9 of May 2012 versus Release 1.8.8
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.8 to Release 1.8.9.

New and Changed Functions
A new high level C function and Fortran subroutine were added to HDF5 Lite (H5LT) to determine whether an HDF5 path is correct and resolves to a valid HDF5 object:

H5LTpath_valid
h5ltpath_valid_f
 

The new file image operations feature consists of the following functions and their associated callbacks, struct, and ENUM. This feature enables an application to load a file into memory (as a file image) and to operate on that file image without the overhead of disk I/O. This feature provides the following functions:

H5LTopen_file_image
H5Fget_file_image
 

 

H5Pset_file_image
H5Pget_file_image
 

H5Pset_file_image_callbacks
H5Pget_file_image_callbacks
For the callbacks, struct, and ENUM, see H5Pset_file_image_callbacks.

Use of this feature is described in [HDF5 File Image Operations](PDF).
 

The following functions were modified or added to the HDF5 Library to allow an application to control the operation of copying committed datatypes with H5Ocopy:

Function with changed interface:

H5Pset_copy_object
New flag for use with H5Pset_copy_object:

H5O_COPY_MERGE_COMMITTED_DTYPE_FLAG
Function with changed behavior:

H5Ocopy
This function's behavior is changed, but only with the use of the new flag, H5O_COPY_MERGE_COMMITTED_DTYPE_FLAG.
New functions and callback function:

H5Pset_mcdt_search_cb
H5Pget_mcdt_search_cb
H5O_mcdt_search_cb_t
 

H5Padd_merge_committed_dtype_path
H5Pfree_merge_committed_dtype_paths
Use of this capability is described in [Copying Committed Datatypes with H5Ocopy](PDF).
 

The following C functions and Fortran subroutines were added to provide a mechanism to ensure sequential consistency in parallel environments using MPI I/O:

H5Fset_mpi_atomicity
H5Fget_mpi_atomicity
 

h5fset_mpi_atomicity_f
h5fget_mpi_atomicity_f
Use of this capability is described in [Enabling a Strict Consistency Semantics Model in Parallel HDF5](PDF).
 

The following function was added at Release 1.8.5 but was not documented at that time. It is mentioned here for those who may have missed its introduction:

H5Oexists_by_name
New Fortran Subroutines
The following Fortran subroutines have been added in this release:

h5ltpath_valid_f
h5ocopy_f
h5fset_mpi_atomicity_f
h5fget_mpi_atomicity_f
Where a subroutine is part of a new HDF5 feature, that feature is described above.

Fortran Subroutine with Changed Behavior
The content of the buffer returned by the following Fortran subroutine has changed as noted:

h5ltget_attribute_string_f
If the returned buffer requires padding, h5ltget_attribute_string_f now employs space padding; this buffer was previously returned with a C NULL terminator.
Command-line Utilities with New Options and/or Changed Behavior
h5repack has one new option and improved performance under certain circumstances:

-Mn, --metadata_block_size=n
Sets the metadata block size in the output file.
 
Performance is improved when repacking large chunked datasets (for example, when dataset size exceeds 128 MB).
 

h5stat has one new option:

-S, --summary
Displays a summary of file space usage for a file.
 

h5dump has one new option, a new capability, and updated output:

--no-compact-subset
Disables compact subsetting to allow the left-hand square bracket character ( [ ) in a dataset name.
 
The files parameter now accommodates wildcards. With this change, h5dump command-line options can be applied across multiple files without listing the files individually.
 
h5dump output has been updated for this release to improve compliance with the HDF5 DDL specification and to improve readablilty. This output is now fully compliant with the HDF5 DDL specification, but these changes may affect some user scripts:
Whitespace has been modified.
Display errors, such as misplaced brackets, have been fixed.
When printing superblock content, user block content is now properly displayed within the superblock.

## Release 1.8.8 of November 2011 versus Release 1.8.7
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.7 to Release 1.8.8.

New C Functions
Two new parallel I/O functions enable an application to query whether or how a collective I/O operation actually completed:

H5Pget_mpio_actual_chunk_opt_mode
H5Pget_mpio_actual_io_mode
Updated Fortran Interface, including Fortran 2003 Features
The HDF5 Fortran interface has been extended and upgraded to Fortran 2003 for this release.

See [Configure and Build Changes](#Configure-and-Build-Changes) below for the configure options to use when building HDF5 with Fortran 2003 enabled.

Features enabled in this upgrade are described in New Features in the HDF5 Fortran Library: Adding Support for the Fortran 2003 Standard (PDF only). In general, the links below are to technical descriptions (reference manual entries, for example) of specific interfaces.

Terminology: If the Fortran compiler in use is Fortran 2003 compliant and the installed HDF5 Library was configured using both the --enable-fortran and --enable-fortran2003 flags, the resulting HDF5 Fortran Library is referred to below as a Fortran 2003 environment. Without either of the Fortran 2003 compliant compiler or the --enable-fortran2003 flag, the resulting HDF5 Fortran Library is referred to below as a Fortran 90 environment.

New Fortran 2003-only subroutines
New subroutines available only in Fortran 2003 environments:

h5dvlen_reclaim_f
h5eset_auto_f
h5literate_by_name_f
h5literate_f
h5ovisit_f
h5oget_info_by_name_f
h5tconvert_f
Subroutines with Fortran 90 versions and extended features in Fortran 2003 environments
Subroutines in this section have extended support in Fortran 2003 environments, but are new or unchanged in Fortran 90 environments.

All existing Fortran 90 subroutines, including those not mentioned below, are available in both Fortran 90 and Fortran 2003 environments.

Subroutines with support for the derived type C_PTR in Fortran 2003 environments:

h5pget_f
h5pget_fill_value_f
h5pinsert_f
h5pregister_f
h5pset_f
h5pset_fill_value_f
h5rcreate_f
h5rdereference_f
h5rget_name_f
h5rget_object_type_f
Subroutines with support for the derived type C_PTR and a changed signature in Fortran 2003 environments:

h5aread_f
h5awrite_f
h5dread_f
h5dwrite_f
Subroutine with additional optional parameters in Fortran 2003 environments:

h5pcreate_class_f
New and updated Fortran 90 subroutines
New Fortran 90 subroutines:

h5pset_nbit_f
h5pset_scaleoffset_f
Updated Fortran 90 subroutines:

h5open_f
h5close_f *
h5check_version_f
h5get_libversion_f
h5garbage_collect_f
h5dont_atexit_f
* h5close_f has been modified in this release to close only the HDF5 Fortran Library interface; it no longer closes the underlying HDF5 Library.

New Fortran utility functions and derived types
New Fortran utility functions:

h5offsetof         Available only in Fortran 2003 environments.
h5kind_to_type     Available in either Fortran environment.
New Fortran derived types in Fortran 2003 environments:

hvl_t              Described in New Features.
h5o_info_t         See h5ovisit_f.
New high level Fortran interface
New Fortran subroutines in the HDF5 Dimension Scale high-level interface, H5DS:

h5dsset_scale_f
h5dsattach_scale_f
h5dsdetach_scale_f
h5dsis_attached_f
h5dsis_scale_f
h5dsset_label_f
h5dsget_label_f
h5dsget_scale_name_f
h5dsget_num_scales_f
These subtroutines are available in both Fortran 90 and Fortran 2003 environments.

C Functions with Changed Capability
H5Tcreate now supports fixed-length and variable-length string types.

Driver Changes on Windows Systems
Changed behavior:

HDF5's Windows driver has been modified in this release to be a wrapper of the POSIX-compliant SEC2 driver. This change should be transparent to all user applications.

New default driver:

As of this release, the default driver on Windows systems is changed to SEC2; it had previously been the Windows driver.

If you wish to use the Windows driver in an application, it remains fully available. In such cases, the driver must be set explicitly with a H5Pset_fapl_windows call.

Command-line Utilities with Changed Behavior
h5jam now checks the file that is to be inserted into the user block and does not allow the insertion if it is an HDF5 file. This is necessary because an HDF5 file in the user block will make the original HDF5 portion of the file inaccessible.

h5diff with the --delta option reports two NaN values as equal regardless of the type of NaN (as described in the reference manual entry).

h5diff, when comparing a file to itself, now reports no differences.

Configure and Build Changes
configure now uses the same flags and symbols in its tests as are used to build the library.

To build HDF5 with the Fortran 2003 interfaces requires the use of two configure options:
    --enable-fortran  
    --enable-fortran2003  
Note that building the Fortran 2003 interfaces requires a Fortran 2003-capable compiler and that --enable-fortran2003 cannot be used alone.

The --enable-h5dump-packed-bits configure option has been removed. The code that this option enabled is now always compiled into h5dump.

When building on Linux systems, the HDF5 Library now links with the bsd_compat library.

Parallel Library and Configuration Changes
The parallel HDF5 library now always compiles code to take advantage of MPI functionalities that allow for the use of complex derived datatypes during IO and that allow for collective IO when one or more processes do not contribute to the operation.

This was previously conditionally compiled into the library and controlled by configure via the hdf5_cv_mpi_special_collective_io_works and hdf5_cv_mpi_complex_derived_datatype_works cache values. These values were preset in the config/\* files based on the compiler and/or system or could be set on the configure line. Support for these cache values has been removed and they no longer have an effect on the compilation of the use of these MPI functionalities.


## Release 1.8.7 of May 2011 versus Release 1.8.6
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.6 to Release 1.8.7.

New C Functions and Macros
Three new functions enable the caching of files opened through external links. This cache can improve the performance of external links in heavy use cases:

H5Pset_elink_file_cache_size
H5Pget_elink_file_cache_size
H5Fclear_elink_file_cache
 
Two new macros enable compile-time inclusion or exclusion of code depending on the version of the HDF5 Library against which an application is to be linked:

H5_VERSION_GE
H5_VERSION_LE
C Functions with Changed Capability
HDF5 now allows the size of any dataspace dimension to be 0 (zero). This was previously allowed only if the maximum size of the dimension was unlimited. Though their signatures have not changed, the range of valid parameter values is affected for the following functions:

H5Screate_simple
H5Sset_extent_simple
H5Pset_external
Note that no data can be written to a dataset in which the size of any dimension is set to zero.
 
In the logging file driver, implementation of the following flags has been completed: H5FD_LOG_TIME_OPEN, H5FD_LOG_TIME_READ, H5FD_LOG_TIME_WRITE, and H5FD_LOG_TIME_SEEK
Two new flags have been added: H5FD_LOG_NUM_TRUNCATE and H5FD_LOG_TIME_STAT.

H5Pset_fapl_log
 
HDF5 now blocks committing a datatype that cannot hold any data. This currently includes compound datatypes with no fields and enumerated datatypes with no members.

H5Tcommit1
H5Tcommit2
H5Tcommit_anon
HDF5 has long blocked creating a dataset or attribute with such a datatype.

Fortran Subroutines with Changed Capability
Several h5ltmake_dataset\*\_f and h5ltread_dataset\*\_f subroutines have been modified to allow them to create or read arrays of up to seven dimensions. They had previously been limited to three dimensions.

    

h5ltmake_dataset_f
h5ltmake_dataset_int_f
h5ltmake_dataset_float_f
h5ltmake_dataset_double_f
    

h5ltread_dataset_f
h5ltread_dataset_int_f
h5ltread_dataset_float_f
h5ltread_dataset_double_f
 
h5tbget_field_info_f has a new maxlen_out parameter. This parameter is optional.

Command-line Utilities with New Options
h5diff has a new verbose with levels option to display additional attribute information:

-vn, --verbose=n
 
h5dump has one new option to enable reporting of the HDF5 Library's error stack:

--enable-error-stack
Command-line Utility with Changed Option Name
h5ls has one changed option name:

--enable-error-stack replaces --error.
The --error option is deprecated but remains available.

Command-line Utilities with Changed Behavior
Attribute comparison with h5diff has been corrected to compare all attributes with common names when comparing objects. Previously, h5diff compared attributes correctly only when two objects had the same number of attributes and all attribute names were identical; if there was either a value or name difference between two attributes, subsequent attributes were not compared.

h5diff --use-system-epsilon now employs the formula '|a - b|' instead of '|a - b| / b'.

h5copy now accepts the same file as the source and target file; this enables object copying within a file.

h5dump now displays reference types as described in DDL in BNF for HDF5:

    

H5T_REFERENCE {H5T_STD_REF_OBJ}

Object reference

 

H5T_REFERENCE {H5T_STD_REF_DSETREG}  

Dataset region reference

Configure and Build Changes
Configure now generates Makefiles that build in silent make mode by default. In this mode, compile and link lines are significantly simplified for clarity. You can opt out of this default with either of the following approaches:

Use the --disable-silent-rules flag at configure time to override the default and view actual compile and link lines during the build.
Use the make command with the V=1 option to run a verbose make.
The new configure option --enable-unsupported can be used to stop configure from preventing the use of unsupported configure option combinations. For example, use of this option enables building HDF5 with the unsupported and untested configurations of C++ in a parallel environment or Fortran with a threadsafe library.

WARNING:
Use   --enable-unsupported   at your own risk!
This option may result in a library that does not compile or does not run as expected.
Parallel HDF5
mpicc and mpif90 are now the default C and Fortran compilers on Linux systems when --enable-parallel is specified but no $CC or $FC is defined.

## Release 1.8.6 versus Release 1.8.5, February 2011
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.5 to Release 1.8.6.

C Functions with Additional Capability
H5Tset_order and H5Tget_order can now be used with all datatypes. See the related note regarding H5T_ORDER_MIXED immediately below.

ENUM with New Member
H5T_order_t, an ENUM used to set or indicate datatype endianness, includes a new member:

     H5T_ORDER_MIXED

The function H5Tget_order returns this value when called on a compound datatype whose members are not all of the same endianness.

Command-line Utilities with New Options
h5diff has one new option:

--exclude-path
 

h5ls and h5diff have two new options:

--follow-symlinks
--no-dangling-links
Command-line Utilities with Changed Behavior
h5diff now compares member objects and groups recursively when two files or groups are specified to be compared.

h5ls now displays object and attribute names exactly as stored in the file rather than interpreting them for output. For example, if an attribute is named \ttemp in the file, h5ls will output that name as \ttemp rather than converting the leading \t to a tab.

h5repack can now convert a dataset to compact layout.

Compiler Script Arguments and Environment Variables
h5cc, h5fc, h5c++, h5pcc, and h5pfc arguments have been expanded to enable the user to add compiler and linker flags that will take precedence over the flags from the HDF5 build.

The arguments CPPFLAGS, CFLAGS, LDFLAGS, and LIBS have been duplicated with the prefix H5BLD_ to put the flags and paths from the HDF5 build in the correct places and to allow the script user to add entries in CPPFLAGS, CFLAGS, LDFLAGS, and LIBS that will take precedence over those from the HDF5 build. Changes to these entries can be made persistent by editing CFLAGSBASE, CPPFLAGSBASE, LDFLAGSBASE, and LIBSBASE near the top of the script. If the change is to be temporary, the user can set HDF5_CFLAGS, HDF5_CPPFLAGS, HDF5_LDFLAGS, or HDF5_LIBS in the environment.

The new order of arguments for these scripts is as follows:

 
        $CLINKER 
        $H5BLD_CPPFLAGS 
        $CPPFLAGS 
        $H5BLD_CFLAGS 
        $CFLAGS 
        $LDFLAGS 
        $clibpath 
        $link_objs 
        $LIBS 
        $link_args 
        $shared_link
            
Thread Safety on Windows
This release adds support for thread safety on Windows using the Windows threads library. See the file RELEASE.txt in the distributed HDF5 source code (hdf5/release_docs/RELEASE.txt) for details.

Parallel HDF5
The Parallel HDF5 build process has changed to use mpiexec instead of mpirun.

## Release 1.8.5 versus Release 1.8.4, June 2010
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.4 to Release 1.8.5.

New C Function
The following function was added to the HDF5 Library at this release, but was not documented at that time:

H5Oexists_by_name
(This note added in November 2011, shortly following Release 1.8.8.)

C Functions with Additional Capability
The following functions have been modified to accommodate filters set on a group creation property list:

H5Pall_filters_avail
H5Pget_filter1
H5Pget_filter2
H5Pget_filter_by_id1
H5Pget_filter_by_id2
H5Pget_nfilters

    

H5Pmodify_filter
H5Premove_filter
H5Pset_deflate
H5Pset_filter
H5Pset_fletcher32

Related to H5Zregister, the calling protocol for the can_apply and set_local callback functions has been extended to accommodate the use of filters on group creation property lists.

New C++ Member Function and Overloaded Member Functions
The following C++ member function is new in this release:

DataSet::getInMemDataSize

  

To simplify getting a dataset's data size in memory.

 

The overloaded forms of the following C++ member functions are new in this release:

CommonFG::getObjnameByIdx

  

Overloaded to take char\* for name.

CommonFG::getObjTypeByIdx

  

Overloaded to return type name as a char\*.

Command-line Utility with New Options
h5diff has two new options:

--follow-links
--no-dangling-links
 

Fortran Subroutines with Changed Interface
The link type parameter values associated with the following Fortran subroutines have changed in this release:

h5lget_info_f
h5lget_info_by_idx_f

Valid values for the parameter link_type have changed to the following:
     H5L_TYPE_HARD_F
     H5L_TYPE_SOFT_F
     H5L_TYPE_EXTERNAL_F
     H5L_TYPE_ERROR_F
These values previously began with the string 'H5L_LINK_'.

CMake
This release includes an adaptation enabling HDF5 builds with CMake. This early-stage adaptation is distributed to provide an opportunity for early access and experimentation; the CMake-related files should not be viewed as final and may change before the next release.

While the initial development effort has been focused on Microsoft Windows platforms, the HDF5 Library has been successfully built with CMake on Linux and Mac OS X.

See the file CMake.txt in the distributed HDF5 source code (hdf5/release_docs/CMake.txt) for details.

## Release 1.8.4 versus Release 1.8.3, November 2009
This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.8.3 to Release 1.8.4.

Embedded HDF5 Library Settings Information
Library settings are now embedded in an HDF5 executable so that an "orphaned" executable can be queried (via the Unix strings command, for example) to display the library settings used to build it. The embedded settings information is the same as that normally found in the libhdf5.settings file.

This capability may be disabled with the use of the --disable-embedded-libinfo flag during configure.

New Fortran Subroutine and Constant
The following Fortran subroutine is new in this release:

h5oopen_by_addr_f

The following Fortran constant, corresponding to the C constant H5T_ORDER_NONE, has been added in this release:

H5T_ORDER_NONE_F

New C++ Member Functions
New C++ member functions are provided to determine a dataset's or attribute's data size in memory:

size_t DataSet::getInMemDataSize() const
size_t Attribute::getInMemDataSize() const

The size in each case is in bytes.

Function with Changed Interface or Behavior
H5check_version now displays embedded library information (see above) if a version mismatch is detected.

The function has also been changed to suppress the warning message entirely if $HDF5_DISABLE_VERSION_CHECK is set to 2 or higher.

Fortran Subroutines with Changed Syntax
The following Fortran subroutines have changed syntax in this release:

h5lget_info_f
The type of the parameter address has changed from INTEGER to INTEGER(HADDR_T).
(This note added on 13 January 2010.)

h5lget_info_by_idx_f
h5lget_info_by_idx_f (loc_id, group_name, index_field, order, n, &
     link_type, f_corder_valid, corder, cset, address, val_size, &
     hdferr, lapl_id)

The parameters link_type, address, and val_size have been added; the parameter data_size has been removed.

Tools with Changed Interface or Behavior
h5diff has one new option:

--use-system-epsilon

With this option, h5diff returns a difference if and only if the difference between two data values exceeds the system value for epsilon (that is, if |a-b| > epsilon).

The tool's default behavior has also changed; without this option, the tool now checks for strict equality. Prior default behavior was to use an epsilon internally defined in HDF5.

This option has no short form.

h5dump has one new option and the behavior of another set of options has changed:

-R or --region

With this option, h5dump prints the data pointed to by a region reference. Without this option, h5dump prints only the reference.

Usage of the subsetting parameters has changed. All of START, STRIDE, COUNT, and BLOCK now have default values. If those defaults are satisfactory, h5dump no longer requires that all four be specifiespecified when subsetting a dataset.

Changed Library Configuration
The following configuration flag is new with this release:

--disable-embedded-libinfo

This flag disables the embedding of HDF5 Library settings in an HDF5 executable.

## Release 1.8.3 versus Release 1.8.2, May 2009
This section lists API-level changes and changes in behavior in the transition from HDF5 Release 1.8.2 to Release 1.8.3.

New Functions
C functions:

The following C functions are new for this release and are documented in the HDF5 Reference Manual.

    

H5Pset_elink_cb     

    

herr_t H5Pset_elink_cb( hid_t lapl_id, H5L_elink_traverse_t func, void \*op_data )
See entry.

    

H5Pget_elink_cb     

    

herr_t H5Pget_elink_cb( hid_t lapl_id, H5L_elink_traverse_t \*func, void \*\*op_data )
See entry.

    

H5Pset_elink_acc_flags     

    

herr_t H5Pset_elink_acc_flags( hid_t lapl_id, unsigned flags )
See entry.

    

H5Pget_elink_acc_flags     

    

herr_t H5Pget_elink_acc_flags( hid_t lapl_id, unsigned \*flags )
See entry.

    

H5Pset_chunk_cache     

    

herr_t H5Pset_chunk_cache( hid_t dapl_id, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0 )
See entry.

    

H5Pget_chunk_cache     

    

herr_t H5Pget_chunk_cache( hid_t dapl_id, size_t \*rdcc_nslots, size_t \*rdcc_nbytes, double \*rdcc_w0 )
See entry.

    

H5Dget_access_plist     

    

hid_t H5Dget_access_plist( hid_t dset_id )
See entry.

    

H5Iis_valid     

    

htri_t H5Iis_valid( hid_t id )
See entry.

Fortran Subroutines:

The following subroutines are new in this release:

h5dget_access_plist_f  
h5iis_valid_f  
h5pset_chunk_cache  
h5pget_chunk_cache  

Function with Changed Behavior
H5set_free_list_limits

The HDF5 Library now dynamically creates separate factory free lists to manage blocks of a fixed size; these blocks were previously managed within the blockfree lists. Factory free lists use the same settings as specified in H5set_free_list_limits for block free lists.

While the API has not changed, users may see both a performance boost and increased memory usage.

Tools with Changed Interface or Behavior
h5diff

The default behavior of h5diff is now to detect and compare NaNs. Use the -N or --nan option (below) to turn this behavior off and avoid the performance penalty that NaN detection and comparison entalis. (NaN detection and comparison became the default in in HDF5 Release 1.6.8.)

This tool has two new options:

     -c, --compare      Lists objects that are not comparable.

     -N, --nan          Avoids NaNs detection.

h5dump

h5dump now correctly specifies an XML DTD or a schema URL.

h5dump binary output now defaults to NATIVE.

The new -b, --binary option can be used to specify alternate binary formats.

h5repack

When a user does not specify a chunk size, h5repack now defines the default chunk size to be the same same as the size of the hyperslab used to read the chunks.

## Release 1.8.2 versus Release 1.8.1, November 2008
This section lists API-level changes and changes in behavior in the transition from HDF5 Release 1.8.1 to Release 1.8.2.

### New Functions

#### C functions:

The following C functions are new for this release and are documented in the HDF5 Reference Manual.

    

H5Pset_elink_fapl     

    

herr_t H5Pset_elink_fapl( hid_t lapl_id, hid_tfapl_id )
See entry.

 

H5Pget_elink_fapl     

 

hid_t H5Pget_elink_fapl( hid_t lapl_id )
See entry.

#### Fortran Subroutine:

The following subroutine is new in this release:
     H5Tget_native_type_f

#### New C++ Wrappers

A number of C++ wrappers were added/removed.  See RELEASE.txt for details.
                
### Functions with Changed Syntax
The following function syntaxes have changed; the affected return values and parameter are noted as underscored red text.

#### C functions:

H5Fget_obj_count

ssize_t H5Fget_obj_count( hid_t file_id, unsigned int types )

H5Fget_obj_ids

ssize_t H5Fget_obj_ids( hid_t file_id, unsigned int types,
        size_t max_objs, hid_t *obj_id_list )

#### Changed Fortran Subroutines
The optional parameter mounted has been added to the following subroutines:

 
h5gget_info_f
h5gget_info_by_idx_f
h5gget_info_by_name_f     
Changed Data Structure
H5G_info_t

A mounted filed has been added to the H5G_info_t struct.

### Tools with Changed Interface or Behavior
h5diff

h5diff now returns 1 if the files differ in structure, i.e., if the file graphs differ by any object.

The tool's error return code has been changed to 2.

h5repack and h5diff

The standard command-line syntax of both tools has been changed to parallel that of h5dump. The new syntax of each is as follows:

     h5repack [OPTIONS] file1 file2

     h5diff [OPTIONS] file1 file2 [object1 [object2]]

In each case, the old syntax remains available (though undocumented) to avoid breaking existing scripts.

h5repack

The following new options control the addition of user block data to an existing HDF5 file:

-u U
--ublock=U

    

Specifies the name of the file containing the user block data to be added.


-b B
--block=B

    


Specifies the size of the block to be added.

The following new options set parameters governing the tool's use of H5Pset_alignment:

-t T
--threshold=T

    

Specifies the threshold value for H5Pset_alignment.


-a A
--alignment=A

    


Specifies the alignment value for H5Pset_alignment.

h5repack now supports multiple instances of the '-f' or '--filter' option, allowing the use of multiple I/O filters with an object.

With no change in the command-line syntax, h5repack now retains an existing userblock when repacking an HDF5 file.

h5import

The floating point format flag TEXTFPE, for scientific notation, has been deprecated. The flag TEXTFP, which accepts scientific notation, should be used instead.

h5import now imports string data with the STR keyword in the INPUT-CLASS configuration field.

h5dump and h5ls

These tools now traverse external links when performing recursive traversals.

h5dump

Binary output now defaults to NATIVE with the '-b' option, optionally accepting NATIVE, FILE, BE, and LE.

The new '-m T' or '--format T' option enables a user-defined formatting string for printing floating-point numbers.

With no change in the command-line syntax, h5dump now prints a compression ratio when compression filters are in use.

### Deprecated Functions
The following functions are deprecated as of this release:

        H5Pset_preserve
        H5Pget_preserve 
The original role of these functions is now part of the core HDF5 Library functionality.

### Changed File Installation
The file libhdf5_fortran.settings is no longer installed with the HDF5 Library; the original content of that file is now included in libhdf5.settings.

### Changed Library Configuration
It is no longer necessary to manually set an SZIP library path in LD_LIBRARY_PATH when building HDF5 with shared SZIP libraries; configure will determine the correct path from the --with-szlib flag, prepending it to and exporting LD_LIBRARY_PATH.

Note that if an SZIP library path is set in a user-supplied LD_LIBRARY_PATH, in an existing script, for example, that does not necessarily need to be changed; configure will prepend the SZIP library path extracted from --with-szlib to the user-supplied LD_LIBRARY_PATH.

 

## Release 1.8.1 versus Release 1.8.0
HDF5 Release 1.8.1 is a minor release with several bugfixes and optimizations and limited new features.

The most significant new feature is an expanded set of Fortran90 APIs, enabling essentially all new C library features of Release 1.8.0.

### New Functions and Tools
#### C functions:

There are no new C functions for Release 1.8.1.

#### Fortran subroutines:

The following Fortran subroutines are new for Release 1.8.1 and are documented in the HDF5 Reference Manual.

H5A

 
h5acreate_f
h5acreate_by_name_f
h5adelete_by_idx_f
h5adelete_by_name_f
h5aexists_f
h5aexists_by_name_f
h5aget_create_plist_f
h5aget_info_f
h5aget_info_by_idx_f
                  
 

 
h5aget_info_by_name_f
h5aget_name_by_idx_f
h5aget_storage_size_f
h5aopen_f
h5aopen_by_idx_f
h5aopen_by_name_f
h5arename_f
h5arename_by_name_f
                  
H5D

 
h5dcreate_anon_f
h5dcreate_f
                  
 

 
h5dopen_f
h5dset_extent_f
                  
H5G

 
h5gcreate_anon_f
h5gcreate_f
h5gget_create_plist_f
h5gget_info_f
                  
 

 
h5gget_info_by_idx_f
h5gget_info_by_name_f
h5gopen_f
                  
H5L

 
h5lcopy_f
h5lcreate_external_f
h5lcreate_hard_f
h5lcreate_soft_f
h5ldelete_f
h5ldelete_by_idx_f
                  
 

 
h5lexists_f
h5lget_info_f
h5lget_info_by_idx_f
h5lget_name_by_idx_f
h5lmove_f
                  
H5O

 
h5olink_f
                  
 

 
h5oopen_f
                  
H5P

 
h5pget_attr_creation_order_f
h5pget_attr_phase_change_f
h5pget_char_encoding_f
h5pget_copy_object_f
h5pget_create_inter_group_f
h5pget_data_transform_f
h5pget_elink_prefix_f
h5pget_est_link_info_f
h5pget_fapl_direct_f
h5pget_link_creation_order_f
h5pget_link_phase_change_f
h5pget_local_heap_size_hint_f
h5pget_nlinks_f
h5pget_obj_track_times_f
h5pget_shared_mesg_index_f
h5pget_shared_mesg_nindexes_f
                  
 

 
h5pset_attr_creation_order_f
h5pset_attr_phase_change_f
h5pset_char_encoding_f
h5pset_copy_object_f
h5pset_create_inter_group_f
h5pset_data_transform_f
h5pset_elink_prefix_f
h5pset_est_link_info_f
h5pset_fapl_direct_f
h5pset_libver_bounds_f
h5pset_link_creation_order_f
h5pset_link_phase_change_f
h5pset_local_heap_size_hint_f
h5pset_obj_track_times_f
h5pset_nbit_f
h5pset_nlinks_f
h5pset_scaleoffset_f
                  
H5R

 
h5rget_name_f
                  
 

 
                  
H5S

 
h5sdecode_f
h5sencode_f
                  
 

 
h5sextent_equal_f
                  
H5T

 
h5tcommit_anon_f
h5tcommit_f
h5tcommitted_f
h5tcompiler_conv_f
                  
 

 
h5tdecode_f
h5tencode_f
h5tget_create_plist_f
h5topen_f
                  
#### C++ wrappers:

There are no new C++ wrappers for Release 1.8.1.

### Functions and Tools with Changed Syntax
C functions with changed syntax:

There are no C functions with changed syntax in Release 1.8.1.

Fortran subroutines with changed syntax:

In the following Fortran subroutines, spos is a new parameter indicating the position of the sign bit:
     h5tset_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)
     h5tget_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)

Changed C++ wrappers:

The class AbstractDS was moved out of H5Object.
The class Attribute now multiply inherits from IdComponent and AbstractDs; the class DataSet now multiply inherits from H5Object and AbstractDs.

In addition, data member IdComponent::id was moved into subclasses: Attribute, DataSet, DataSpace, DataType, H5File, Group, and PropList.

IdComponent::dereference was incorrect; it was changed from:
     void IdComponent::dereference(IdComponent& obj, void* ref)
to:
     void H5Object::dereference(H5File& h5file, void* ref)
     void H5Object::dereference(H5Object& obj, void* ref)
Tools with changed syntax:

The command-line syntax of h5repack was changed in Release 1.8.0 to the following:
     h5repack [OPTIONS] in_file out_file
Since this would be incompatible with existing user scripts, the original syntax has be re-enabled in this release:
     h5repack -i in_file -o out_file [OPTIONS]

h5repack now understands either syntax.

An h5repack option to set the minimum number of links in indexed format was introduced in Release 1.8.0 as '-d min_indexed_links' or '--indexed=min_indexed_links'. In Release 1.8.1, the '-i' form has been changed to '-d' as follows:
     -d min_indexed_links
or
     --indexed=min_indexed_links
The command-line syntax of h5diff was changed in Release 1.8.0 to the following:
     h5diff [OPTIONS] file1 file2 [object1 [object2 ] ]
Tools with new options:

h5dump
-m T  or  --format T   Sets format of floating point output

### Changed Behavior or Output
Changed library behavior:

A 4GB limit on the size of dataset chunks is now properly enforced when a dataset is created.

The HDF5 Library now works more effectively with external links. Some valid external links that previously failed will now be properly recognized. See H5Lcreate_external.

C functions with changed behavior:

The behavior of H5Lcreate_external has changed as described immediately above.

Tools with changed behavior and changed output:

The h5dump -p option now prints the compression ratio if a compression filter is associated with a dataset.

h5dump now supports external links, displaying the target objects (datasets, groups, and named datatypes).

### Tools with changed output:

#### Changed Library Configuration
The following configuration flag is new with this release:

--disable-sharedlib-rpath

#### Removed Patch Requirement
The patch that was required for Cygwin with HDF5 Release 1.8.0 has been rolled into this release and no longer needs to be applied by the user.

 

## Release 1.8.0 versus Release 1.6.x

HDF5 Release 1.8.0 is a major release with several new features and a variety of bugfixes, optimizations, and portability enhancements.

See the document [What's New in HDF5 1.8.0](What's New in HDF5 1.8.0) for a high-level overview of the new functionality in this release. The following pages assume a familiarity with that overview.

This section provides details of the API-level changes that have been made in the transition to HDF5 Release 1.8.0 from the Release 1.6.x series.

API Compatibility Macros with Associated Renamed and New Functions
Release 1.8.0 introduces API compatibility macros designed to facilitate application migration. These macros are intended to facilitate developer management of a clean, step-by-step migration from an older HDF5 Library to the new release; they can also be used to enable older applications to use the new library without requiring that the application be rewritten. See API Compatibility Macros in HDF5 for a full description.

Macros are identified in reference manual function indexes by an italic typeface.

The new macros and the new and old functions to which they may be mapped are as follows:

H5Acreate     

H5Acreate1 
H5Acreate2 

Renamed from H5Acreate
New

H5Aiterate     

H5Aiterate1 
H5Aiterate2 

Renamed from H5Aiterate
New

H5Dcreate     

H5Dcreate1 
H5Dcreate2 

Renamed from H5Dcreate
New

H5Dopen     

H5Dopen1 
H5Dopen2 

Renamed from H5Dopen
New

H5Eclear     

H5Eclear1 
H5Eclear2 

Renamed from H5Eclear
New

H5Eget_auto     

H5Eget_auto1 
H5Eget_auto2 

Renamed from H5Eget_auto
New

H5Eprint     

H5Eprint1 
H5Eprint2 

Renamed from H5Eprint
New

H5Epush     

H5Epush1 
H5Epush2 

Renamed from H5Epush
New

H5Eset_auto     

H5Eset_auto1 
H5Eset_auto2 

Renamed from H5Eset_auto
New

H5Ewalk     

H5Ewalk1 
H5Ewalk2 

Renamed from H5Ewalk
New

H5Gcreate     

H5Gcreate1 
H5Gcreate2 

Renamed from H5Gcreate
New

H5Gopen     

H5Gopen1 
H5Gopen2 

Renamed from H5Gopen
New

H5Pget_filter     

H5Pget_filter1 
H5Pget_filter2 

Renamed from H5Pget_filter
New

H5Pget_filter_by_id     

H5Pget_filter_by_id1    
H5Pget_filter_by_id2 

Renamed from H5Pget_filter_by_id
New

H5Pinsert     

H5Pinsert1 
H5Pinsert2 

Renamed from H5Pinsert
New

H5Pregister     

H5Pregister1 
H5Pregister2 

Renamed from H5Pregister
New

H5Rget_obj_type     

H5Rget_obj_type1 
H5Rget_obj_type2 

Renamed from H5Rget_obj_type
New

H5Tarray_create     

H5Tarray_create1 
H5Tarray_create2 

Renamed from H5Tarray_create
New

H5Tcommit     

H5Tcommit1 
H5Tcommit2 

Renamed from H5Tcommit
New

H5Tget_array_dims     

H5Tget_array_dims1 
H5Tget_array_dims2 

Renamed from H5Tget_array_dims
New

H5Topen     

H5Topen1 
H5Topen2 

Renamed from H5Topen
New


(H5Dcreate and H5Dopen added to the above list in November 2010, prior to Release 1.8.6.)

New Functions and Tools
C functions:

The following C functions are new for Release 1.8.0 and, unless otherwise noted, are documented in the HDF5 Reference Manual.

Recursive iteration

 

H5Lvisit     

herr_t H5Lvisit( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data )
See entry.

H5Lvisit_by_name     

herr_t H5Lvisit_by_name( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data, hid_t lapl_id )
See entry.

H5Ovisit     

herr_t H5Ovisit( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data )
See entry.

H5Ovisit_by_name     

herr_t H5Ovisit_by_name( hid_t loc_id, const char *object_name, H5_index_t index_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t lapl_id )
See entry.

"Format bounds" property
Release-to-release format compatibility

 

H5Pset_libver_bounds     

herr_t H5Pset_libver_bounds( hid_t fapl_id, H5F_libver_t low, H5F_libver_t high )
See entry.

H5Pget_libver_bounds     

herr_t H5Pget_libver_bounds( hid_t fapl_id, H5F_libver_t *low, H5F_libver_t *high )
See entry.

Configurable compact-or-indexed
groups (compact and large groups)

 

H5Pset_link_phase_change     

herr_t H5Pset_link_phase_change( hid_t gcpl_id, unsigned max_compact, unsigned min_dense )
See entry.

H5Pget_link_phase_change     

herr_t H5Pget_link_phase_change( hid_t gcpl_id, unsigned *max_compact, unsigned *min_dense )
See entry.

H5Pset_est_link_info     

herr_t H5Pset_est_link_info( hid_t gcpl_id, unsigned est_num_entries, unsigned est_name_len )
See entry.

H5Pget_est_link_info     

herr_t H5Pget_est_link_info( hid_t gcpl_id, unsigned *est_num_entries, unsigned *est_name_len )
See entry.

H5Pset_nlinks     

herr_t H5Pset_nlinks( hid_t lapl_id, size_t nlinks )
See entry.

H5Pget_nlinks     

herr_t H5Pget_nlinks( hid_t lapl_id, size_t *nlinks )
See entry.

User-defined links

 

H5Lcreate_ud     

herr_t H5Lcreate_ud( hid_t link_loc_id, const char *link_name, H5L_type_t link_type, const char *udata, size_t udata_size, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Lregister     

herr_t H5Lregister( const H5L_class_t * link_class )
See entry.

H5Lunregister     

herr_t H5Lunregister( H5L_type_t link_cls_id )
See entry.

H5Oincr_refcount     

herr_t H5Oincr_refcount( hid_t object_id )
See entry.

H5Odecr_refcount     

herr_t H5Odecr_refcount( hid_t object_id )
See entry.

External links

 

H5Lcreate_external     

herr_t H5Lcreate_external( const char *file_name, const char *object_name, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Lunpack_elink_val     

herr_t H5Lunpack_elink_val( char *ext_linkval, size_t link_size, const char **filename, const char **obj_path )
See entry.

H5Pset_elink_prefix     

herr_t H5Pset_elink_prefix( hid_t lapl_id, const char *prefix )
See entry.

H5Pget_elink_prefix     

herr_t H5Pget_elink_prefix( hid_t lapl_id, const char **prefix )
See entry.

Link creation order
tracking and indexing

 

H5Pset_link_creation_order     

herr_t H5Pset_link_creation_order( hid_t ocpl_id, unsigned *crt_order_flags )
See entry.

H5Pget_link_creation_order     

herr_t H5Pget_link_creation_order( hid_t ocpl_id, unsigned *crt_order_flags )
See entry.

H5Gget_create_plist     

hid_t H5Gget_create_plist( hid_t group_id )
See entry.

H5Gget_info_by_idx     

herr_t H5Gget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, H5G_info_t *group_info, hid_t lapl_id )
See entry.

H5Tget_create_plist     

hid_t H5Tget_create_plist( hid_t dtype_id )
See entry.

Link API (H5L), an all-new interface
(includes user-defined and external links)

 

H5Lcopy     

herr_t H5Lcopy( hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, hid_t lcpl_id hid_t lapl_id )
See entry.

H5Lcreate_external     

herr_t H5Lcreate_external( const char *file_name, const char *object_name, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Lcreate_hard     

herr_t H5Lcreate_hard( hid_t obj_loc_id, const char *obj_name, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Lcreate_soft     

herr_t H5Lcreate_soft( const char *target_path, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Lcreate_ud     

herr_t H5Lcreate_ud( hid_t link_loc_id, const char *link_name, H5L_type_t link_type, const char *udata, size_t udata_size, hid_t lcpl_id, hid_t lapl_id )
See entry.

H5Ldelete     

herr_t H5Ldelete( hid_t loc_id, const char *name, hid_t lapl_id )
See entry.

H5Ldelete_by_idx     

herr_t H5Ldelete_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
See entry.

H5Lexists     

htri_t H5Lexists( hid_t loc_id, const char *name, hid_t lapl_id )
See entry.

H5Lget_info     

herr_t H5Lget_info( hid_t link_loc_id, const char *link_name, H5L_info_t *link_buff, hid_t lapl_id )
See entry.

H5Lget_info_by_idx     

herr_t H5Lget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, H5L_info_t *link_val, hid_t lapl_id )
See entry.

H5Lget_name_by_idx     

ssize_t H5Lget_name_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id )
See entry.

H5Lget_val     

herr_t H5Lget_val( hid_t link_loc_id, const char *link_name, void *linkval_buff, size_t size, hid_t lapl_id )
See entry.

H5Lget_val_by_idx     

ssize_t H5Lget_val_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, char *value_buf, size_t size, hid_t lapl_id )
See entry.

H5Lis_registered     

htri_t H5Lis_registered( H5L_type_t link_cls_id )
See entry.

H5Literate     

herr_t H5Literate( hid_t loc_id, H5_index_t index_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data )
See entry.

H5Literate_by_name     

herr_t H5Literate( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data, hid_t *lapl_id )
See entry.

H5Lmove     

herr_t H5Lmove( hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, hid_t lcpl, hid_t lapl )
See entry.

H5Lregister     

herr_t H5Lregister( const H5L_class_t * link_class )
See entry.

H5Lunpack_elink_val     

herr_t H5Lunpack_elink_val( char *ext_linkval, size_t link_size, const char **filename, const char **obj_path )
See entry.

H5Lunregister     

herr_t H5Lunregister( H5L_type_t link_cls_id )
See entry.

H5Lvisit     

herr_t H5Lvisit( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data )
See entry.

H5Lvisit_by_name     

herr_t H5Lvisit_by_name( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data, hid_t lapl_id )
See entry.

Enhanced attribute handling

 

H5Acreate2     

hid_t H5Acreate2( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id )
See entry.

H5Adelete_by_idx     

herr_t H5Adelete_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
See entry.

H5Adelete_by_name     

herr_t H5Adelete_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id )
See entry.

H5Aget_info     

herr_t H5Aget_info( hid_t attr_id, H5A_info_t *ainfo )
See entry.

H5Aget_info_by_idx     

herr_t H5Aget_info_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id )
See entry.

H5Aget_info_by_name     

herr_t H5Aget_info_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, H5A_info_t *ainfo, hid_t lapl_id )
See entry.

H5Aget_name_by_idx     

ssize_t H5Aget_name_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id )
See entry.

H5Aget_storage_size     

hsize_t H5Aget_storage_size( hid_t attr_id )
See entry.

H5Aiterate2     

herr_t H5Aiterate2( hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *n, H5A_operator2_t op, void *op_data )
See entry.

H5Aiterate_by_name     

herr_t H5Aiterate_by_name( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *n, H5A_operator2_t op, void *op_data, hid_t lapd_id )
See entry.

H5Aopen     

herr_t H5Aopen( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id )
See entry.

H5Aopen_by_idx     

herr_t H5Aopen_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id )
See entry.

H5Arename_by_name     

herr_t H5Arename_by_name( hid_t loc_id, const char *obj_name, const char *old_attr_name, const char *new_attr_name, hid_t lapl_id )
See entry.

Managing large numbers of attributes

 

H5Pset_attr_phase_change     

herr_t H5Pset_attr_phase_change( hid_t ocpl_id, unsigned max_compact, unsigned min_dense )
See entry.

H5Pget_attr_phase_change     

herr_t H5Pget_attr_phase_change( hid_t ocpl_id, unsigned *max_compact, unsigned *min_dense )
See entry.

Attribute creation order
tracking and indexing

 

H5Pset_attr_creation_order     

herr_t H5Pset_attr_creation_order( hid_t ocpl_id, unsigned crt_order_flags )
See entry.

H5Pget_attr_creation_order     

herr_t H5Pget_attr_creation_order( hid_t ocpl_id, unsigned *crt_order_flags )
See entry.

H5Aget_create_plist     

hid_t H5Aget_create_plist( hid_t attr_id )
See entry.

Shared object header messages (SOHM)

 

H5Pset_shared_mesg_index     

herr_t H5Pset_shared_mesg_index( hid_t fcpl_id, unsigned index_num, unsigned mesg_type_flags, unsigned min_mesg_size )
See entry.

H5Pset_shared_mesg_nindexes     

herr_t H5Pset_shared_mesg_nindexes( hid_t plist_id, unsigned nindexes )
See entry.

H5Pset_shared_mesg_phase_change     

herr_t H5Pset_shared_mesg_phase_change( hid_t fcpl_id, unsigned max_list, unsigned min_btree )
See entry.

H5Pget_shared_mesg_index     

herr_t H5Pget_shared_mesg_index( hid_t fcpl_id, unsigned index_num, unsigned *mesg_type_flags, unsigned *min_mesg_size )
See entry.

H5Pget_shared_mesg_nindexes     

herr_t H5Pget_shared_mesg_nindexes( hid_t fcpl_id, unsigned *nindexes )
See entry.

H5Pget_shared_mesg_phase_change     

herr_t H5Pget_shared_mesg_phase_change( hid_t fcpl_id, unsigned *max_list, unsigned *min_btree )
See entry.

Enhanced local heap size
management   (group meta data)

 

H5Pset_local_heap_size_hint     

herr_t H5Pset_local_heap_size_hint( hid_t gcpl_id, size_t *size_hint )
See entry.

H5Pget_local_heap_size_hint     

herr_t H5Pget_local_heap_size_hint( hid_t gcpl_id, size_t size_hint )
See entry.

UNICODE support

 

H5Pset_char_encoding     

herr_t H5Pset_char_encoding( hid_t plist_id, H5T_cset_t encoding )
See entry.

H5Pget_char_encoding     

herr_t H5Pget_char_encoding( hid_t plist_id, H5T_cset_t encoding )
See entry.

Object API (H5O), all new

 

H5Oclose     

herr_t H5Oclose( hid_t object_id )
See entry.

H5Ocopy     

herr_t H5Ocopy( hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name, hid_t ocp_plist_id, hid_t lcpl_id )
See entry.

H5Odecr_refcount     

herr_t H5Odecr_refcount( hid_t object_id )
See entry.

H5Oget_comment     

ssize_t H5Oget_comment( hid_t object_id, char *comment, size_t bufsize )
See entry.

H5Oget_comment_by_name     

ssize_t H5Oget_comment_by_name( hid_t loc_id, const char *name, char *comment, size_t bufsize, hid_t lapl_id )
See entry.

H5Oget_info     

herr_t H5Oget_info( hid_t object_id, H5O_info_t *obj_info )
See entry.

H5Oget_info_by_idx     

herr_t H5Oget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, H5O_info_t *obj_info, hid_t lapl_id )
See entry.

H5Oget_info_by_name     

herr_t H5Oget_info_by_name( hid_t loc_id, const char *object_name, H5O_info_t *object_info, hid_t lapl_id )
See entry.

H5Oincr_refcount     

herr_t H5Oincr_refcount( hid_t object_id )
See entry.

H5Olink     

herr_t H5Olink( hid_t obj_id, hid_t new_loc_id, const char *new_link_name, hid_t lcpl, hid_t lapl )
See entry.

H5Oopen     

hid_t H5Oopen( hid_t loc_id, const char *name, hid_t lapl_id )
See entry.

H5Oopen_by_addr     

hid_t H5Oopen_by_addr( hid_t loc_id, haddr_t addr )
See entry.

H5Oopen_by_idx     

herr_t H5Oopen_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
See entry.

H5Oset_comment     

herr_t H5Oset_comment( hid_t object_id, const char *comment )
See entry.

H5Oset_comment_by_name     

herr_t H5Oset_comment_by_name( hid_t loc_id, const char *name, const char *comment, hid_t lapl_id )
See entry.

H5Ovisit     

herr_t H5Ovisit( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data )
See entry.

H5Ovisit_by_name     

herr_t H5Ovisit_by_name( hid_t loc_id, const char *object_name, H5_index_t index_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t lapl_id )
See entry.

Object creation or copying with
"create intermediate groups" property

 

H5Pset_create_intermediate_group  

herr_t H5Pset_create_intermediate_group( hid_t gcpl_id, unsigned crt_intermed_group )
See entry.

H5Pget_create_intermediate_group

herr_t H5Pget_create_intermediate_group( hid_t gcpl_id, unsigned *crt_intermed_group )
See entry.

Object copy properties

 

H5Ocopy     

herr_t H5Ocopy( hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name, hid_t ocp_plist_id, hid_t lcpl_id )
See entry.

H5Pset_copy_object     

herr_t H5Pset_copy_object( hid_t cplist_id, unsigned copy_options )
See entry.

H5Pget_copy_object     

herr_t H5Pget_copy_object( hid_t cplist_id, unsigned *copy_options )
See entry.

Improved object and file
information retrieval

 

H5Aget_info     

herr_t H5Aget_info( hid_t attr_id, H5A_info_t *ainfo )
See entry.

H5Aget_info_by_name     

herr_t H5Aget_info_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, H5A_info_t *ainfo, hid_t lapl_id )
See entry.

H5Aget_info_by_idx     

herr_t H5Aget_info_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id )
See entry.

H5Fget_info     

herr_t H5Fget_info( hid_t obj_id, H5F_info_t *bh_info )
See entry.

H5Gget_info     

herr_t H5Gget_info( hid_t group_id, H5G_info_t group_info )
See entry.

H5Gget_info_by_name     

herr_t H5Gget_info_by_name( hid_t loc_id, const char *group_name, H5G_info_t group_info, hid_t lapl_id )
See entry.

H5Gget_info_by_idx     

herr_t H5Gget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, H5G_info_t group_info, hid_t lapl_id )
See entry.

H5Lget_info     

herr_t H5Lget_info( hid_t link_loc_id, const char *link_name, H5L_info_t *link_buff, hid_t lapl_id )
See entry.

H5Lget_info_by_idx     

herr_t H5Lget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, H5L_info_t *link_val, hid_t lapl_id )
See entry.

H5Oget_info     

herr_t H5Oget_info( hid_t object_id, H5O_info_t *obj_info )
See entry.

H5Oget_info_by_name     

herr_t H5Oget_info_by_name( hid_t loc_id, const char *object_name, H5O_info_t *object_info, hid_t lapl_id )
See entry.

H5Oget_info_by_idx     

herr_t H5Oget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, H5O_info_t *obj_info, hid_t lapl_id )
See entry.

Name of referenced object

 

H5Rget_name     

ssize_t H5Rget_name( hid_t loc_id, H5R_type_t ref_type, void *ref, char *name, size_t size )
See entry.

Anonymous object creation

 

H5Dcreate_anon     

hid_t H5Dcreate_anon( hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id )
See entry.

H5Gcreate_anon     

hid_t H5Gcreate_anon(hid_t loc_id, const char * name, hid_t gcpl_id, hid_t gapl_id)
See entry.

H5Tcommit_anon     

herr_t H5Tcommit_anon( hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id )
See entry.

H5Lcreate_hard     

herr_t H5Lcreate_hard( hid_t obj_loc_id, const char *obj_name, hid_t link_loc_id, const char *link_name, hid_t lcpl, hid_t lapl )
See entry.

New-style object creation and open
functions  (includes anonymous
object creation)

 

H5Dcreate_anon     

hid_t H5Dcreate_anon( hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id )
See entry.

H5Dcreate2     

hid_t H5Dcreate2( hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id )
See entry.

H5Dopen2     

hid_t H5Dopen2( hid_t loc_id, const char *name, hid_t dapl_id )
See entry.

H5Gcreate_anon     

hid_t H5Gcreate_anon(hid_t loc_id, const char * name, hid_t gcpl_id, hid_t gapl_id)
See entry.

H5Gcreate2     

hid_t H5Gcreate2( hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id )
See entry.

H5Gopen2     

hid_t H5Gopen2( hid_t loc_id, const char * name, hid_t gapl_id )
See entry.

H5Tcommit_anon     

herr_t H5Tcommit_anon( hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id )
See entry.

H5Tcommit2     

herr_t H5Tcommit2( hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id )
See entry.

H5Topen2     

hid_t H5Topen2( hid_t loc_id, const char * name, hid_t tapl_id )
See entry.

User-defined identifiers

 

H5Iregister     

hid_t H5Iregister(H5I_type_t type, void *object)
See entry.

H5Iregister_type     

H5I_type_t H5Iregister_type(size_t hash_size, unsigned reserved, H5I_free_t free_func)
See entry.

H5Idestroy_type     

herr_t H5Idestroy_type(H5I_type_t type)
See entry.

H5Iinc_type_ref     

int H5Iinc_type_ref(H5I_type_t type)
See entry.

H5Idec_type_ref     

int H5Idec_type_ref(H5I_type_t type)
See entry.

H5Iget_type_ref     

int H5Iget_type_ref(H5I_type_t type)
See entry.

Other identifier API (H5I) enhancements

 

H5Iclear_type     

herr_t H5Iclear_type(H5I_type_t type, hbool_t force)
See entry.

H5Inmembers     

int H5Inmembers(H5I_type_t type, hsize_t *num_members)
See entry.

H5Iobject_verify     

void * H5Iobject_verify(hid_t id, H5I_type_t id_type)
See entry.

H5Iremove_verify     

void * H5Iremove_verify(hid_t id, H5I_type_t id_type)
See entry.

H5Isearch     

void * H5Isearch(H5I_type_t type, H5I_search_func_t func, void *key)
See entry.

H5Itype_exists     

htri_t H5Itype_exists(H5I_type_t type)
See entry.

Compression filters: N-bit
and scale-offset

 

H5Pset_nbit     

herr_t H5Pset_nbit( hid_t plist_id )
See entry.

H5Pset_scaleoffset     

herr_t H5Pset_scaleoffset( hid_t plist_id, H5Z_SO_scale_type_t scale_type, int scale_factor )
See entry.

User-defined datatype conversion
callback functions

 

H5Pget_type_conv_cb     

herr_t H5Pget_type_conv_cb( hid_t plist, H5T_conv_except_func_t *func, void **op_data )
See entry.

H5Pset_type_conv_cb     

herr_t H5Pset_type_conv_cb( hid_t plist, H5T_conv_except_func_t func, void *op_data )
See entry.

Datatype compiler conversion check

 

H5Tcompiler_conv     

htri_t H5Tcompiler_conv( hid_t src_id, hid_t dst_id )
See entry.

Dataset array size reduction

 

H5Dset_extent     

herr_t H5Dset_extent( hid_t dset_id, const hsize_t *size )
See entry.

Dataspace equivalence

 

H5Sextent_equal     

herr_t H5Sextent_equal( hid_t sid1, hid_t sid2 )
See entry.

Direct I/O, bypassing system cache

 

H5Pset_fapl_direct     

herr_t H5Pset_fapl_direct( hid_t fapl_id, size_t alignment, size_t block_size, size_t cbuf_size )
See entry.

H5Pget_fapl_direct     

herr_t H5Pget_fapl_direct( hid_t fapl_id, size_t *alignment, size_t *block_size, size_t *cbuf_size )
See entry.

Parallel I/O optimization
(collective chunk I/O)

 

H5Pset_dxpl_mpio_chunk_opt     

herr_t H5Pset_dxpl_mpio_chunk_opt(hid_t dxpl_id, H5FD_mpio_chunk_opt_t opt_mode)
See entry.

H5Pset_dxpl_mpio_chunk_opt_num     

herr_t H5Pset_dxpl_mpio_chunk_opt_num(hid_t dxpl_id, unsigned num_chunk_per_proc)
See entry.

H5Pset_dxpl_mpio_chunk_opt_ratio     

herr_t H5Pset_dxpl_mpio_chunk_opt_ratio(hid_t dxpl_id, unsigned percent_num_proc_per_chunk)
See entry.

H5Pset_dxpl_mpio_collective_opt     

herr_t H5Pset_dxpl_mpio_collective_opt( hid_t dxpl_id, H5FD_mpio_collective_opt_t opt_mode )
See entry.

Enhanced error handling (H5E)

 

H5Eauto_is_v2     

herr_t H5Eauto_is_v2( hid_t estack_id, unsigned *is_stack )
See entry.

H5Eclear2     

herr_t H5Eclear2( hid_t estack_id )
See entry.

H5Eclose_msg     

herr_t H5Eclose_msg (hid_t mesg_id)
See entry.

H5Eclose_stack     

herr_t H5Eclose_stack(hid_t estack_id)
See entry.

H5Ecreate_msg     

hid_t H5Ecreate_msg(hid_t class, H5E_type_t msg_type, const char* mesg)
See entry.

H5Eget_auto2     

herr_t H5Eget_auto2(hid_t estack_id, H5E_auto2_t * func, void **client_data)
See entry.

H5Eget_class_name     

ssize_t H5Eget_class_name(hid_t class_id, char* name, size_t size)
See entry.

H5Eget_current_stack     

hid_t H5Eget_current_stack(void)
See entry.

H5Eget_msg     

ssize_t H5Eget_msg(hid_t mesg_id, H5E_type_t* mesg_type, char* mesg, size_t size)
See entry.

H5Eget_num     

ssize_t H5Eget_num(hid_t estack_id)
See entry.

H5Epop     

herr_t H5Epop(hid_t estack_id, size_t count)
See entry.

H5Eprint2     

herr_t H5Eprint2(hid_t estack_id, FILE * stream)
See entry.

H5Epush2     

herr_t H5Epush2(hid_t estack_id, const char *file, const char *func, unsigned line, hid_t class_id, hid_t major_id, hid_t minor_id, const char *msg, ...)
See entry.

H5Eregister_class     

hid_t H5Eregister_class(const char* cls_name, const char* lib_name, const char* version)
See entry.

H5Eset_auto2     

herr_t H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data)
See entry.

H5Eset_current_stack     

herr_t H5Eset_current_stack(hid_t estack_id)
See entry.

H5Eunregister_class     

herr_t H5Eunregister_class(hid_t class_id)
See entry.

H5Ewalk2     

herr_t H5Ewalk2(hid_t estack_id, H5E_direction_t direction, H5E_walk2_t func, void * client_data)
See entry.

Meta data cache management

 

H5Fget_mdc_config     

herr_t H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
See entry.

H5Fget_mdc_hit_rate     

herr_t H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr)
See entry.

H5Fget_mdc_size     

herr_t H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr, size_t *cur_size_ptr, int *cur_num_entries_ptr)
See entry.

H5Freset_mdc_hit_rate_stats     

herr_t H5Freset_mdc_hit_rate_stats(hid_t file_id)
See entry.

H5Fset_mdc_config     

herr_t H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
See entry.

H5Pset_mdc_config     

herr_t H5Pset_mdc_config(hid_t plist_id, H5AC_cache_config_t * config_ptr)
See entry.

H5Pget_mdc_config     

herr_t H5Pget_mdc_config(hid_t plist_id, H5AC_cache_config_t * config_ptr)
See entry.

File read/write status

 

H5Fget_intent     

herr_t H5Fget_intent( hid_t file_id, unsigned *intent )
See entry.

Arithmetic data transform on I/O

 

H5Pget_data_transform     

herr_t H5Pget_data_transform( hid_t plist_id, char* expression, size_t size)
See entry.

H5Pset_data_transform     

herr_t H5Pset_data_transform( hid_t plist, const char* expression )
See entry.

Dataspace and datatype serial conversion

 

H5Sdecode     

hid_t H5Sdecode( const void *buf )
See entry.

H5Sencode     

herr_t H5Sencode( hid_t obj_id, void *buf, size_t *nalloc )
See entry.

H5Tdecode     

hid_t H5Tdecode( const void *buf )
See entry.

H5Tencode     

herr_t H5Tencode( hid_t obj_id, void *buf, size_t *nalloc )
See entry.

Enhanced filter management

 

H5Pget_filter2

herr_t H5Pget_filter2( hid_t plist_id, unsigned idx, unsigned int *flags, size_t *cd_nelmts, unsigned cd_values[], size_t namelen, char name[], unsigned *filter_config)
See entry.

H5Pget_filter_by_id2

hid_t H5Pget_filter_by_id2( hid_t plist_id, H5Z_filter_t filter_id, unsigned int *flags, size_t *cd_nelmts, unsigned int cd_values[], size_t namelen, char name[], unsigned int *filter_config)
See entry.

Comparison of properties

 

H5Pinsert2

hid_t H5Pinsert2( hid_t plid, const char *name, size_t size, void *value, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_compare_func_t compare, H5P_prp_close_func_t close)
See entry.

H5Pregister2

herr_t H5Pregister2( hid_t class, const char * name, size_t size, void * default, H5P_prp_create_func_t create, H5P_prp_set_func_t set, H5P_prp_get_func_t get, H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy, H5P_prp_compare_func_t compare, H5P_prp_close_func_t close)
See entry.

Two-way conversion between datatype
and text description of datatype

 

H5LTtext_to_dtype

hid_t H5LTtext_to_datatype( const char *text, H5LT_lang_t lang_type)
See entry.

H5LTdtype_to_text

herr_t H5LTdtype_to_text( hid_t datatype, char *str, H5LT_lang_t lang_type, size_t *len)
See entry.

Fortran subroutines:

New Fortran subroutines in the main library are not yet included for Release 1.8.0-beta.

See below for Fortran routines for High-Level HDF5 APIs.

C++ wrappers:

There no new C++ wrappers for Release 1.8.0-beta.

High Level C APIs:

H5LT: The following functions have been added to the HDF5 Lite (H5LT) API:

H5LTtext_to_dtype

hid_t H5LTtext_to_datatype( const char *text, H5LT_lang_t lang_type)
See entry.

H5LTdtype_to_text

herr_t H5LTdtype_to_text( hid_t datatype, char *str, H5LT_lang_t lang_type, size_t *len)
See entry.

H5LTset_attribute_long_long

herr_t H5LTset_attribute_long_long( hid_t loc_id, const char *obj_name, const char *attr_name, const long_long *data, size_t *size)
See entry.

H5LTget_attribute_long_long    

herr_t H5LTget_attribute_long_long( hid_t loc_id, const char *obj_name, const char *attr_name, long_long *data)
See entry.

Two high-level C APIs are distributed for the first time with this release:
 

H5PT, the HDF5 Packet Table interface
H5DS, the HDF5 Dimension Scale interface
Full documentation for these interfaces can be accessed directly from the High Level APIs overview page.

The new functions included in the packet table (H5PT) high-level APIs are as follows:

H5PTcreate_fl

hid_t H5PTcreate_fl( hid_t loc_id, const char * dset_name, hid_t dtype_id, hsize_t chunk_size )

H5PTcreate_vl

hid_t H5PTcreate_vl( hid_t loc_id, const char *dset_name, hsize_t chunk_size )

H5PTopen

hid_t H5PTopen( hid_t loc_id, const char *dset_name )

H5PTclose

herr_t H5PTclose( hid_t table_id )

H5PTappend

herr_t H5PTappend( hid_t table_id, hsize_t nrecords, const void *data)

H5PTcreate_index

herr_t H5PTcreate_index( hid_t table_id)

H5PTset_index

herr_t H5PTset_index( hid_t table_id, hsize_t pt_index)

H5PTread_packets

herr_t H5PTread_packets( hid_t table_id, hsize_t start, hsize_t nrecords, void *data)

H5PTget_next

herr_t H5PTget_next( hid_t table_id, hsize_t nrecords, void *data)

H5PTget_num_packets

herr_t H5PTget_num_packets( hid_t table_id, hsize_t * nrecords)

H5PTis_valid

herr_t H5PTis_valid( hid_t table_id)

H5PTis_varlen

herr_t H5PTis_varlen( hid_t table_id)

H5PTfree_vlen_readbuff    

herr_t H5PTfree_vlen_readbuff( hid_t table_id, hsize_t bufflen, void * buff)

 

 
The new functions included in the dimension scale (H5DS) high-level APIs are as follows:

H5DSset_scale

herr_t H5DSmake_scale(hid_t dsid, char *dimname)

H5DSattach_scale

herr_t H5DSattach_scale(hid_t did, hid_t dsid, unsigned int idx)

H5DSdetach_scale

herr_t H5DSdetach_scale(hid_t did, hid_t dsid, unsigned int idx)

H5DSis_attached

htri_t H5DSis_attached(hid_t did, hid_t dsid, unsigned int idx)

H5DSis_scale

htri_t H5DSis_scale(hid_t did)

H5DSiterate_scales

herr_t H5DSiterate_scales(hid_t did, unsigned dim, int *idx, H5DS_iterate_t visitor, void *visitor_data)

H5DSset_label

herr_t H5DSset_label(hid_t did, unsigned int idx, const char *label)

H5DSget_label

ssize_t H5DSget_label(hid_t did, unsigned int idx, char *label, size_t size)

H5DSget_scale_name    

hssize_t H5DSget_scale_name(hid_t did, char name, size_t *size)

H5DSget_num_scales

int H5DSget_num_scales(hid_t did, unsigned int idx)

High Level Fortan90 APIs:

Fortran90 subroutines are now available for the following high-level APIs:
     H5LT: HDF5 Lite
     H5IM: HDF5 Image
     H5TB: HDF5 Table

Tools:

h5stat

Deprecated and Deleted Functions
The following functions, subroutines and wrappers have been removed in this release:

C functions:

 

H5Pset_fapl_gass
H5Pget_fapl_gass
 

      

HDF5 no longer provides GASS support.

H5Pset_fapl_srb
H5Pget_fapl_srb
 
 

      

HDF5 no longer provides SRB support.
SRB functionality is now supported through the mechanism described in SRB - The DICE Storage Resource Broker.

H5Pset_fapl_stream
H5Pget_fapl_stream
 

      

The stream virtual file driver (H5FD_STREAM) has been removed from the HDF5 distribution. The functionality was last available from http://hdf5-addons.origo.ethz.ch/.

H5Tset_overflow
H5Tget_overflow

      

These two functions are replaced by H5Pget_type_conv_cb and H5Pset_type_conv_cb.

Several functions, subroutines, and wrappers are deprecated in this HDF5 release and may eventually be removed from the HDF5 distribution and from the HDF5 Reference Manual. A Release 1.6.x compatibility mode is provided enabling these and other Release 1.6.x compatibility features, but is available only if the HDF5 Library is configured with the default settings or with the flag --with-default-api-version=v16. Release 1.8.0 also provides macros that can be mapped selectively to 1.6.x and 1.8.0 function versions according to the needs of a user application. The backward compatibility mode is enabled in the Release 1.8.0 binaries distributed by NCSA. See API Compatibility Macros in HDF5 for full details.

 
Deprecated functions are marked in the function index at the beginning of each API section in the HDF5 Reference Manual.

Virtual File Driver Removed
The stream virtual file driver (H5FD_STREAM) have been removed in this release. This affects the functions H5Pset_fapl_stream and H5Pget_fapl_stream and the constant H5FD_STREAM.

This virtual file driver will remain available at http://hdf5-addons.origo.ethz.ch/. Note that as of this writing, this transition is still in progress; the necessary integration tools may not be available when HDF5 Release 1.8.0 first comes out.

Functions with New or Changed Symbols or Fixed Values
Symbols and preset values associated with the following functions, subroutines or wrappers have changed as noted.

C functions:

H5Tregister

Null is no longer a valid value for the parameter func.
 

H5Pget_filter_by_id

H5Pmodify_filter

H5Premove_filter

H5Pset_filter

H5Zfilter_avail

H5Zget_filter_info

Two new filter identifiers are available for the filter parameter:
    H5Z_FILTER_NBIT
    H5Z_FILTER_SCALEOFFSET
 

H5Pget_filter

Two new filter identifiers are available as the return value:
    H5Z_FILTER_NBIT
    H5Z_FILTER_SCALEOFFSET
 

Fortran90 subroutines:

h5pget_filter_f

h5pget_filter_by_id_f

h5pmodify_filter_f

h5premove_filter_f

h5pset_filter_f

h5zfilter_avail_f

h5zget_filter_info_f

Two new filter identifiers are available for the filter or filter_id parameters:
    H5Z_FILTER_NBIT_F
    H5Z_FILTER_SCALEOFFSET_F
 

Functions with Changed Syntax
Function syntax changes in this release are handled through the mechanism described in API Compatibility Macros in HDF5.

Exceptions:

The function H5Eget_minor is not governed by the above-mentioned macros and its return type has changed:
H5Eget_minor

char *H5Eget_minor (H5E_minor_t n)

See entry.

An application calling H5Eget_minor from an HDF5 Library of Release 1.8.0 or later will have to free the memory associated with the return value to prevent a memory leak.
(This note added at Release 1.8.4, November 2009.)

 

In the function H5Dextend, the size parameter signature was changed.
H5Dextend

herr_t *H5Dextend( hid_t dataset_id, const hsize_t size[] )

See entry for current information; see the Release 1.6 entry for the old signature.

The new function signature is generally considered equivalent to the old signature, but some compilers require an exact match.
(This note added in February 2010, following Release 1.8.4.)

Renamed Functions
The following C functions have been renamed. The original function names remain available under certain circumstances; see API Compatibility Macros in HDF5 for full details.

Original name

New name

H5Acreate

H5Acreate1

H5Aiterate
 

H5Aiterate1
 

H5Eclear

H5Eclear1

H5Eget_auto

H5Eget_auto1

H5Eprint

H5Eprint1

H5Epush

H5Epush1

H5Eset_auto

H5Eset_auto1

H5Ewalk
 

H5Ewalk1
 

H5Gcreate

H5Gcreate1

H5Gopen
 

H5Gopen1
 

H5Pget_filter

H5Pget_filter1

H5Pget_filter_by_id    

H5Pget_filter_by_id1

H5Pinsert

H5Pinsert1

H5Pregister
 

H5Pregister1
 

H5Rget_obj_type
 

H5Rget_obj_type1
 

H5Tarray_create

H5Tarray_create1

H5Tcommit

H5Tcommit1

H5Tget_array_ndims

H5Tget_array_ndims1

H5Topen

H5Topen1

Changed Library Structures
The following library structures have changed as follows:

C API:

H5FD_t

In this struct, the field fileno[2] has changed to fileno.
The struct is otherwise unchanged.
 

H5Z_class_t

This struct contains three new fields:
    version
    encoder_present
    decoder_present
The struct is now defined as follows:

 
    typedef struct H5Z_class_t {
        int version; 
        H5Z_filter_t filter_id;
        unsigned encoder_present
        unsigned decoder_present
        const char  *comment;
        H5Z_can_apply_func_t can_apply_func;
        H5Z_set_local_func_t set_local_func;
        H5Z_func_t filter_func;            
    } H5Z_class_t;
Changed ENUMs
The following enumerated datatypes used by the library are discontinued in this release:

H5E_major_t - Major error numbers

H5E_minor_t - Minor error numbers

HDF5 error codes have changed from the ENUM format used in earlier releases to an identifier format. As ENUMs, error codes were constants defined at compile time; as identifiers, error codes are defined at runtime, in the same manner as HDF5 object identifiers (file, dataset, group, etc.).

Under normal circumstances, this change should be transparent to user applications.

The following enumerated datatypes used by the library have changed in the C and Fortran APIs as follows:

H5G_obj_t - Object types

This ENUM has reordered as follows:
    H5G_GROUP
    H5G_DATASET
    H5G_TYPE
    H5G_LINK
 

H5I_type_t - Identifier types

The following symbols have been added to this ENUM:
    H5I_UNINT
    H5I_ERROR_CLASS
    H5I_ERROR_MSG
    H5I_ERROR_STACK
 

Another new symbol
    H5I_NTYPES

replaces the old symbol
    H5I_NGROUPS
While visible in the public API, H5I_NTYPES functions as a limit value for the HDF5 Library and is not generally used at the application level. The same was true of H5I_NGROUPS.
 

H5S_class_t - Dataspace type identifiers

The following symbol has been added:
    H5S_NULL
 

H5T_cset_t - Character set encoding identifiers

The following symbol has been added:
    H5T_CSET_UTF8
 

H5Z_filter_t - Filters identifiers

The following symbols have been added:
    H5Z_FILTER_NBIT
    H5Z_FILTER_SCALEOFFSET
 

New Datatypes
The following OpenVMS-specific floating point datatypes are new with this release:

H5T_VAX_F32

H5T_VAX_F64

New Property Lists
The following property lists are new with this release:

H5P_DATASET_ACCESS

H5P_GROUP_CREATE

H5P_GROUP_ACCESS

H5P_DATATYPE_CREATE

H5P_DATATYPE_ACCESS

H5P_ATTRIBUTE_CREATE

Changed Library Configuration
The following configuration features have changed as follows.

New with this release:
    --disable-dependency-tracking
    
    --enable-hdf5v1_6
    
    --enable-dconv-exception
    --disable-dconv-exception
    --enable-dconv-accuracy
    --disable-dconv-accuracy
    
    --program-prefix=PREFIX
    --program-prefix=SUFFIX
    --program-transform-name=PROGRAM
    
    --enable-maintainer-mode
    
    --disable-dependency-tracking
    --enable-dependency-tracking
    
    --enable-codestack
    
    --enable-direct-vfd
    
    --with-tags[=TAGS]
    
New with this release, but expected to be useful only to those working inside the HDF5 Library or working very closely with HDF5 developers:
    --enable-build-all
    --enable-metadata-trace-file
    
New with this release, but not actively supported:
    --enable-fphdf5
    
Changed in this release:
    --enable-shared[=PKGS]
    --enable-static[=PKGS]
    --enable-fast-install[=PKGS]
    
Removed in this release:
    --enable-hdf5v1_4
    --enable-funcstack
    --with-ssl=LIB
    --with-gass=DIR
    --with-srb=DIR
The environment variables FC and FCFLAGS replace F9X and F9XFLAGS, respectively, which are deprecated in this release.

Additional new "influential environment variables" in this release include the following:
    CXX
    CXXFLAGS
    CXXCPP
    F77
    FFLAGS

 
