---
title: Software Changes from Release to Release for HDF5 1.12
redirect_from: 
  - /display/HDF5/Software+Changes+from+Release+to+Release+for+HDF5+1.12
---

# Software Changes from Release to Release in HDF5 1.12

For a description of the major new features that were introduced, please see [New Features in HDF5 Release 1.12](sw_changes_1.12.md).

This page provides information on the changes that a maintenance developer needs to be aware of between successive releases of HDF5, such as:

* New or changed features or tools
* Syntax and behavioral changes in the existing application programming interface (the API)
* Certain types of changes in configuration or build processes

Note that bug fixes and performance enhancements in the C library are automatically picked up by the C++, Fortran, and Java libraries.

The following information is included below.

* <a href="#2versus1">Release 1.12.2 versus Release 1.12.1</a>
* <a href="#1versus0">Release 1.12.1 versus Release 1.12.0</a>
* <a href="#1versus0">Release 1.12.0 versus Release 1.10.6</a>

The release notes also list changes made to the library, but these notes tend to be more at a more detail-oriented level. The release notes may include new features, bugs fixed, supported configuration features, platforms on which the library has been tested, and known problems. The release note files are listed below and can be found at the top level of the HDF5 source code tree in the release_docs directory.

|                               |                                                              |
| ----------------------------- | ------------------------------------------------------------ |
| **Release Notes**             | Technical notes regarding the current release of the HDF5 library (RELEASE.txt in the source code) |
| **HISTORY-1_12.txt**          | Release information for HDF5-1.12.0 through 1.12.1 |
| **HISTORY-1_10_0-1_12_0.txt** | Development history between the HDF5-1.10.0 and HDF5-1.12.0 releases |
| **HISTORY-1_10.txt**          | Release information for all HDF5-1.10 releases |
| **HISTORY-1_8_0-1_10_0.txt**  | Development history between the HDF5-1.8.0 and HDF5-1.10.0 releases |
| **HISTORY-1_8.txt**           | Release information for HDF5-1.8.0 through 1.8.21 |
| **HISTORY-1_0-1_8_0_rc3.txt** | Technical notes starting with HDF5-1.0.0 and ending with HDF5-1.8.0-rc3 (the state of the code prior to the HDF5-1.8.0 release) |

<h2 id="2versus1">Release 1.12.2 versus Release 1.12.1</h2>

This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.12.1 to Release 1.12.2.

### New and Changed Functions, Classes, Subroutines, Wrappers, and Macros

The following are new C functions in this release:

| Function                             | Description                                  |
| ------------------------------------ | -------------------------------------------- |
| [H5DSwith_new_ref](/documentation/hdf5/latest/group___h5_d_s.html#gaed2b97139202dfe69f3f2a7364c10fbc)         | Determines if new references are used with dimension scales
| [H5LTget_attribute_ullong](/documentation/hdf5/latest/group___h5_l_t.html#ga366fbd6822449e762109c012cf2b6f08) | Reads an unsigned long long attribute
| [H5LTset_attribute_ullong](/documentation/hdf5/latest/group___h5_l_t.html#ga004f5266cea7ce5f6a77ee41b05faa8b) | Create an unsigned long long attribute
| [H5VLobject_is_native](/documentation/hdf5/latest/group___h5_v_l.html#gad7fa2adf3bb8a834169ef2fa50c76827)     | Determines whether an object ID represents a native VOL connector object

See API Compatibility Reports for 1.12.2 for complete details.

<h2 id="1versus0">Release 1.12.1 versus Release 1.12.0</h2>

This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.12.0 to Release 1.12.1.

The following are new C functions in this release:

| Function                             | Description                                  |
| ------------------------------------ | -------------------------------------------- |
| [H5Pget_fapl_splitter](/documentation/hdf5/latest/group___f_a_p_l.html#gaf6ac1c131acee33dfb878593dfefb4ac) | Retrieves information for a splitter file access property list
| [H5Pset_fapl_splitter](/documentation/hdf5/latest/group___f_a_p_l.html#ga49f386ea235bb48128e54c962c499f07) | Sets the file access property list to use the splitter driver
| [H5Pget_file_locking](/documentation/hdf5/latest/group___f_a_p_l.html#ga5de19a5a8ac23ca417aa2d49d708dc2d) | Gets the file locking property values
| [H5Pset_file_locking](/documentation/hdf5/latest/group___f_a_p_l.html#ga503e9ff6121a67cf53f8b67054ed9391) | Sets the file locking property values
| [H5get_free_list_sizes](/documentation/hdf5/latest/group___h5.html#ga2310d963a6f48ec12fda8c0c8bbefbbb) | Gets the current size of the free lists used to manage memory
| [H5Ssel_iter_reset](/documentation/hdf5/latest/group___h5_s.html#ga411bbc06a31814bff9f476712c2a791c) | Resets a dataspace selection iterator back to an initial state
| [H5VLquery_optional](/documentation/hdf5/latest/group___h5_v_l.html#ga17ef00e528d99eda5879d749c2a12043) | Determines whether a VOL connector supports a particular optional callback operation
 

In the C++ Wrapper

The following C++ wrappers were added:

| Function                             | Description                                  |
| ------------------------------------ | -------------------------------------------- |
| [DataSet::operator=](myurl) | Operator= for [DataSet](myurl) class
| [FileAccPropList::getFileLocking](myurl) | See [H5Pget_file_locking](/documentation/hdf5/latest/group___f_a_p_l.html#ga5de19a5a8ac23ca417aa2d49d708dc2d) for details
| [FileAccPropList::setFileLocking](myurl) | See [H5Pset_file_locking](/documentation/hdf5/latest/group___f_a_p_l.html#ga503e9ff6121a67cf53f8b67054ed9391) for details

See the API Compatibility report for complete details.

Compatibility Notes and Report
See API Compatibility Reports for 1.12 for information regarding compatibility with previous releases.

<h2 id="0versus10\_6">Release 1.12.1 versus Release 1.12.0</h2>

This section lists interface-level changes and other user-visible changes in behavior in the transition from HDF5 Release 1.10.6 to Release 1.12.0.

### New Features
For a description of the major new features that were introduced, please see [New Features in HDF5 Release 1.12](new_features_1_12.md).

### New and Changed Functions, Classes, Subroutines, Wrappers, and Macros

### In the C/Fortran Interface (main library)

Following are the new or changed APIs introduced in HDF5-1.12.0. Those introduced with a new feature list the specific new feature that they were added for.

| Function              | Fortran      | Description                                  |
| ----------------------|------------- | -------------------------------------------- |
| [H5Fdelete](/documentation/hdf5/latest/group___h5_f.html#ga2e8b5e19b343123e8ab21442f9169a62) | N | Deletes an HDF5 file |
| [H5Fget_fileno](/documentation/hdf5/latest/group___h5_f.html#ga402205688af065ab5db0fe20417d5484) | Y | Retrieves a file's file number that uniquely identifies the open file |
| [H5Fis_accessible](/documentation/hdf5/latest/group___h5_f.html#ga584471c3b98453b9b04a4bf9af847442) | Y | Determines if a file can be opened with a given fapl |
|  | | |
| [H5Iiterate](/documentation/hdf5/latest/group___h5_i_u_d.html#ga3bc2a11a594bf484cc5dc69ac987d0f4) | N | Calls a callback for each member of the identifier type specified |
|  | | |
| [H5Lget_info](/documentation/hdf5/latest/group___h5_l.html#ga97279697f3010a6ad31dd7f4341eb698)<br>[H5Lget_info1](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_l.html#gacc2ad7f2b402c4bf9bb122d7f43b98dc)<br>[H5Lget_info2](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_l.html#ga65e63c6e880fd0183c40486d6748e400) | Y (no change) | The function H5L_GET_INFO was moved to H5L_GET_INFO1, and H5L_GET_INFO2 was introduced. The macro H5L_GET_INFO was created that can be mapped to either H5L_GET_INFO1 or H5L_GET_INFO2. For HDF5-1.12, H5L_GET_INFO is mapped to H5L_GET_INFO2 by default.  In earlier releases, H5L_GET_INFO is mapped to H5L_GET_INFO1. |
|  | | |
| [H5Lget_info_by_idx](/documentation/hdf5/latest/group___h5_l.html#ga4db00b8b944eae68233438165c784b67)<br>[H5Lget_info_by_idx1](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_l.html#ga7ed207f47e0e0f768f0d540c73e37e2a)<br>[H5Lget_info_by_idx2](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_l.html#gaecfb3ef8520e9224b24a151ff8459ba9) | Y (no change) | The function H5L_GET_INFO_BY_IDX was moved to H5L_GET_INFO_BY_IDX1, and H5L_GET_INFO_BY_IDX2 was introduced. The macro H5L_GET_INFO_BY_IDX was created that can be mapped to either H5L_GET_INFO_BY_IDX1 or H5L_GET_INFO_BY_IDX2. For HDF5-1.12, H5L_GET_INFO_BY_IDX is mapped to H5L_GET_INFO_BY_IDX2 by default.  In earlier releases, H5L_GET_INFO_BY_IDX is mapped to H5L_GET_INFO_BY_IDX1. |
| [H5Literate](/documentation/hdf5/latest/group___t_r_a_v.html#ga55406698106930db68242987c11ba051)<br>[H5Literate1](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#ga1e7c0a8cf17699563c02e128f27042f1)<br>[H5Literate2](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#gad7ca4206f06b5ada85b6ec5867ec6c73) | Y (no change) | The function H5L_ITERATE was moved to H5L_ITERATE1, and H5L_ITERATE2 was introduced. The macro H5L_ITERATE was created that can be mapped to either H5L_ITERATE1 or H5L_ITERATE2. For HDF5-1.12, H5L_ITERATE is mapped to H5L_ITERATE2 by default.  In earlier releases, H5L_ITERATE is mapped to H5L_ITERATE1. |
| [H5Literate_by_name](/documentation/hdf5/latest/group___t_r_a_v.html#ga655b002428e0176c2fa23a0315fbbcc2)<br>[H5Literate_by_name1](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#ga87e036da0c8d1146a073f3ee08e0fedc)<br>[H5Literate_by_name2](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#ga745a65eb516ce40a3be43490aaeb5c5e) | Y (no change) | The function H5L_ITERATE_BY_NAME was moved to H5L_ITERATE_BY_NAME1, and H5L_ITERATE_BY_NAME2 was introduced. The macro H5L_ITERATE_BY_NAME was created that can be mapped to either H5L_ITERATE_BY_NAME1 or H5L_ITERATE_BY_NAME2. For HDF5-1.12, H5L_ITERATE_BY_NAME is mapped to H5L_ITERATE_BY_NAME2 by default.  In earlier releases, H5L_ITERATE_BY_NAME is mapped to H5L_ITERATE_BY_NAME1. |
| [H5Lvisit](/documentation/hdf5/latest/group___t_r_a_v.html#gac0558936502924d9e898d8b6e041ed69)<br>[H5Lvisit1](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#ga5424ef7043c82147490d027a0e8a59ef)<br>[H5Lvisit2](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#gae1c6f963892a5f4e8922a66fbe338f66) | N | The function H5L_VISIT was moved to H5L_VISIT1, and H5L_VISIT2 was introduced. The macro H5L_VISIT was created that can be mapped to either H5L_VISIT1 or H5L_VISIT2. For HDF5-1.12, H5L_VISIT is mapped to H5L_VISIT2 by default.  In earlier releases, H5L_VISIT is mapped to H5L_VISIT1. |
| [H5Lvisit_by_name](/documentation/hdf5/latest/group___t_r_a_v.html#ga138405315e233673741893e4e250f055)<br>[H5Lvisit_by_name1](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#ga1f1ba1bb4d44f2c111990024809417ac)<br>[H5Lvisit_by_name2](https://docs.hdfgroup.org/hdf5/v1_14/group___t_r_a_v.html#gafee93792c7e27a7e78b1ec221876b173) | N | The function H5L_VISIT_BY_NAME was moved to H5L_VISIT_BY_NAME1, and H5L_VISIT_BY_NAME2 was introduced. The macro H5L_VISIT_BY_NAME was created that can be mapped to either H5L_VISIT_BY_NAME1 or H5L_VISIT_BY_NAME2. For HDF5-1.12, H5L_VISIT_BY_NAME is mapped to H5L_VISIT_BY_NAME2 by default.  In earlier releases, H5L_VISIT_BY_NAME is mapped to H5L_VISIT_BY_NAME1. |
|  |  | |
| [H5Oget_info](/documentation/hdf5/latest/group___h5_o.html#gaf4f302a33faba9e1c2b5f64c62ca4ed5)<br>[H5Oget_info3](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_o.html#gaf0fbf7d780a1eefce920facadb198013) | N/A | The function H5O_GET_INFO was replaced by the macro H5O_GET_INFO and the function H5O_GET_INFO3 was added. |
| [H5Oget_info_by_idx](/documentation/hdf5/latest/group___h5_o.html#gafe764884e1530f86079288dd5895a7bd)<br>[H5Oget_info_by_idx3](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_o.html#gafa2f8884f7d3e7fd9b8549f5b59fd9eb) | N/A | The function H5O_GET_INFO_BY_IDX was replaced by the macro H5O_GET_INFO_BY_IDX. The function H5O_GET_INFO_BY_IDX3 was added. |
| [H5Oget_info_by_name](/documentation/hdf5/latest/group___h5_o.html#ga16d8ac07f9244cfccb42b5f309ca6b3c)<br>[H5Oget_info_by_name3](https://docs.hdfgroup.org/hdf5/v1_14/group___h5_o.html#gabb69c962999e027cef0079bbb1282199) | N/A | The function H5O_GET_INFO_BY_NAME was replaced by the macro H5O_GET_INFO_BY_NAME. The function H5O_GET_INFO_BY_NAME3 was added. |
| [H5Oget_native_info](/documentation/hdf5/latest/group___h5_o.html#ga677d99ab106e2032b991b75b75de0e46) | N | Retrieves the native file format information about an object |
| [H5Oget_native_info_by_idx](/documentation/hdf5/latest/group___h5_o.html#gafa6570d8b0ef6e2aff75093e1f99f67e) | N | Retrieves native file format information about an object according to the order of an index |
| [H5Oget_native_info_by_name](/documentation/hdf5/latest/group___h5_o.html#ga296ded21aeac3921fee07272353b8476) | N | Retrieves native file format information about an object given its name |
| [H5Oopen_by_token](/documentation/hdf5/latest/group___h5_o.html#ga2ea3627cf171d0565307702a5e203262) | Y | Opens an object in an HDF5 file using its VOL independent token |
| [H5Otoken_cmp](/documentation/hdf5/latest/group___h5_o.html#gaeb8da4fbe62f8a3cd9146a7ac1093562) | Y | Compares two VOL connector object tokens |
| [H5Otoken_from_str](/documentation/hdf5/latest/group___h5_o.html#ga5136c14b4e907f15007030d7a6d6cd24) | N | Deserializes a string into a connector object token |
| [H5Otoken_to_str](/documentation/hdf5/latest/group___h5_o.html#ga2bdd7528090f7f2c4b361ab4cc7735f6) | N | Serializes a connector's object token into a string |
| [H5Ovisit](/documentation/hdf5/latest/group___h5_o.html#ga5ce86255fcc34ceaf84a62551cd24233)
 [H5Ovisit3](/documentation/hdf5/latest/group___h5_o.html#ga6d03115ae0e5e5b516bbf35bb492266a) | N/A | The function H5O_VISIT was replaced by the macro H5O_VISIT and the function H5O_VISIT3 was added |
| [H5Ovisit_by_name](/documentation/hdf5/latest/group___h5_o.html#gab02a69e88b11404e7fd61f55344b186c)
 [H5Ovisit_by_name3](/documentation/hdf5/latest/group___h5_o.html#ga34815400b01df59c4dac19436124885a) | N/A | The function H5O_VISIT_BY_NAME was replaced by the macro H5O_VISIT_BY_NAME, and the function H5O_VISIT_BY_NAME3 was added. |
|  |  | |
| [H5Pencode](/documentation/hdf5/latest/_h5version_8h.html#af1a9ff52a69251d57ffa686102f162a8)
 [H5Pencode1](/documentation/hdf5/latest/group___p_l_c_r_a.html#gaf40518cb161ee9508da4b9c0d34553bf)
 [H5Pencode2](/documentation/hdf5/latest/group___p_l_c_r.html#ga37b1b6666e62a86389015e7dfc384faa) | N | Encodes properties on a property list into a buffer<br>The previous function was renamed to H5P_ENCODE1 and deprecated, and the macro H5P_ENCODE was introduced. |
| [H5Pget_vol_id](/documentation/hdf5/latest/group___f_a_p_l.html#ga5f133bdf09ca5a32622688d1ba5cc838) | Y | Returns the identifier of the current VOL connector |
| [H5Pget_vol_info](/documentation/hdf5/latest/group___f_a_p_l.html#gafc58db23c257cdcf2f0c1c3ae911ab0f) | N | Returns a copy of the VOL information for a connector |
| [H5Pset_vol](/documentation/hdf5/latest/group___f_a_p_l.html#ga8aaa97e70b2544c3d95d908e1ae5b0f0) | Y | Set the file VOL connector for a file access property list |
|  | | |
| [H5Rcopy](/documentation/hdf5/latest/group___h5_r.html#gad5b7b117cfaea90d1e4786304c58c55c) | N | Copies an existing reference |
| [H5Rcreate_attr](/documentation/hdf5/latest/group___h5_r.html#gacae1bf2263befeac54d81f27995721f8) | N | Creates an attribute reference |
| [H5Rcreate_object](/documentation/hdf5/latest/group___h5_r.html#gad0fb6ec99ecefa602756a5682addfc69) | N | Creates an object reference |
| [H5Rcreate_region](/documentation/hdf5/latest/group___h5_r.html#ga60134eb917afbe89aa23eb25a30d249b) | N | Creates a region reference |
| [H5Rdestroy](/documentation/hdf5/latest/group___h5_r.html#ga026114e6c23588bf89bb473eb9e4d095) | N | Closes a reference |
| [H5Requal](/documentation/hdf5/latest/group___h5_r.html#ga8d7dac2c604356f2fc657f36a201aea0) | N | Determines whether two references are equal |
| [H5Rget_attr_name](/documentation/hdf5/latest/group___h5_r.html#gaf9c6823346cc34f02b9f5d11e56344a0) | N | Retrieves the attribute name for a referenced object |
| [H5Rget_file_name](/documentation/hdf5/latest/group___h5_r.html#ga659d950e37122a59d50612b407a48900) | N | Retrieves the file name for a referenced object |
| [H5Rget_obj_name](/documentation/hdf5/latest/group___h5_r.html#gac5a0a3a874cd3c4dc630579521539bb6) | N | Retrieves the object name for a referenced object |
| [H5Rget_obj_type3](/documentation/hdf5/latest/group___h5_r.html#ga6fb098a1a4c9369d76dbeeaf40d6d2af) | N | Retrieves the type of object that an object reference points to |
| [H5Rget_type](/documentation/hdf5/latest/group___h5_r.html#ga56989d33835ec289269d366dc28aa4ad) | N | Retrieves the type of reference |
| [H5Ropen_attr](/documentation/hdf5/latest/group___h5_r.html#ga2a00d73b9a13b9069d0ad39225dd9f71) | N | Opens the referenced HDF5 attribute |
| [H5Ropen_object](/documentation/hdf5/latest/group___h5_r.html#gaa6692bd3a5b490c8db05b90a5888d0dd) | N | Opens the referenced HDF5 object |
| [H5Ropen_region](/documentation/hdf5/latest/group___h5_r.html#ga400154b014acd1736bae9a127f4e9a1a) | N | Sets up a dataspace and selection as specified by a region reference |
|  | | |
| [H5Scombine_hyperslab](/documentation/hdf5/latest/group___h5_s.html#gae7578a93bb7b22989bcb737f26b60ad1) | N | Performs an operation on a hyperslab and an existing selection and returns the resulting selection |
| [H5Scombine_select](/documentation/hdf5/latest/group___h5_s.html#ga356600d12d3cf0db53cc27b212d75b08) | N | Combines two hyperslab selections with an operation, returning a dataspace with the resulting selection | 
| [H5Sencode](/documentation/hdf5/latest/group___h5_s.html#ga35e289baf490229e233296929fbf5208)
 [H5Sencode1](/documentation/hdf5/latest/group___h5_s.html#ga82cf9f6af03ad89be21c36922e03baea)
 [H5Sencode2](/documentation/hdf5/latest/group___h5_s.html#ga178ec7b8769ad5704a170d9bd3421074) | N/A | Encodes a dataspace object description into a binary buffer<br>The function H5S_ENCODE was renamed to H5S_ENCODE1 and deprecated in this release. The macro H5S_ENCODE and the function H5S_ENCODE2 were introduced in this release. |
| [H5Smodify_select](/documentation/hdf5/latest/group___h5_s.html#ga0ccb190f72fe41a927407ffb9f19ef1b) | N | Refines a hyperslab selection with an operation using a second hyperslab to modify it |
| [H5Ssel_iter_close](/documentation/hdf5/latest/group___h5_s.html#ga75b79e15e8fa3e591f7ab25a8624e690) | N | Closes a dataspace selection iterator |
| [H5Ssel_iter_create](/documentation/hdf5/latest/group___h5_s.html#gac73247e4ef206a47c8aa97dd2ff5febe) | N | Creates a dataspace selection iterator for a dataspace's selection |
| [H5Ssel_iter_get_seq_list](/documentation/hdf5/latest/group___h5_s.html#ga793fb6ff150f45d43e379995b5c0d297) | N | Retrieves a list of offset / length sequences for the elements in an iterator |
| [H5Sselect_adjust](/documentation/hdf5/latest/group___h5_s.html#ga64f08c187b899f2728d4ac016d44f890) | N |  Adjusts a selection by subtracting an offset |
| [H5Sselect_copy](/documentation/hdf5/latest/group___h5_s.html#ga57e5eba2d1b282803835ba3f7e0b9bfa) | N | Copies a selection from one dataspace to another |
| [H5Sselect_intersect_block](/documentation/hdf5/latest/group___h5_s.html#ga51472bcb9af024675fba6294a6aefa5e) | N | Checks if current selection intersects with a block |
| [H5Sselect_project_intersection](/documentation/hdf5/latest/group___h5_s.html#ga1e914ba45afb15ded99a0afeaf124c04) | N | Projects the intersection of two source selections to a destination selection |
| [H5Sselect_shape_same](/documentation/hdf5/latest/group___h5_s.html#gafc6cafae877900ee060709eaa0b9b261) | N | Checks if two selections are the same shape |
|  |
| [H5Treclaim](/documentation/hdf5/latest/group___v_l_e_n.html#ga6851783a68a0f868c27300cb5622fbbe) | N | Frees the buffers allocated for storing variable-length data in memory |
| | | |
| [H5VLclose](/documentation/hdf5/latest/group___h5_v_l.html#gaa3324ac7aedf9362b498226903288094) | Y | Closes a VOL connector identifier |
| [H5VLget_connector_id(](/documentation/hdf5/latest/group___h5_v_l.html#ga5b69c29931e55208517c598ac3039f77) | Y | Retrieves the VOL connector identifier for a given object identifier |
| [H5VLget_connector_id_by_name](/documentation/hdf5/latest/group___h5_v_l.html#gabcbf9b9b07a6b60e17ff9681684f944d) | Y | Retrieves the identifier for a registered VOL connector name |
| [H5VLget_connector_id_by_value](/documentation/hdf5/latest/group___h5_v_l.html#ga8f6d366bc6b8323bbffe1e5a5ba18bee) | Y | Retrieves the identifier for a registered VOL connector value |
| [H5VLget_connector_name](/documentation/hdf5/latest/group___h5_v_l.html#gaf326406d7733c0ab8d12118c13c78dfa) | Y | Retrieves the connector name for the VOL associated with the object or file identifier |
| [H5VLis_connector_registered_by_name](/documentation/hdf5/latest/group___h5_v_l.html#ga9be3c92e4430b9cf42a376534a47fcca) | Y | Tests whether a VOL class has been registered or not for a connector name |
| [H5VLis_connector_registered_by_value](/documentation/hdf5/latest/group___h5_v_l.html#ga83ba8986ed68f67c41b492dfd273804b) | Y | Tests whether a VOL class has been registered or not for a connector value |
| [H5VLregister_connector](/documentation/hdf5/latest/group___h5_v_l_d_e_v.html#ga439c150299522a0e0f401a86d083097b) | N | Registers a new VOL connector |
| [H5VLregister_connector_by_name](/documentation/hdf5/latest/group___h5_v_l.html#gaf48d1225927e1e701656346b832ee6b1) | Y | Registers a new VOL connector by name |
| [H5VLregister_connector_by_value](/documentation/hdf5/latest/group___h5_v_l.html#ga11e69930e47f654805a265f417412ea8) | Y | Registers a new VOL connector by connector value |
| [H5VLunregister_connector](/documentation/hdf5/latest/group___h5_v_l.html#gaffbdc22f724c2c818f3be3845145d73e) | Y | Removes a VOL connector identifier from the library |
 

### In the C++ Wrapper


| Function                             | Description                                  |
| ------------------------------------ | -------------------------------------------- |
| H5File::getFileNum | See H5F_GET_FILENO |
| H5File::isAccessible | See H5F_IS_ACCESSIBLE |
| LinkCreatPropList::getCreateIntermediateGroup | See H5P_GET_CREATE_INTERMEDIATE_GROUP |
| LinkCreatPropList::setCreateIntermediateGroup | See H5P_SET_CREATE_INTERMEDIATE_GROUP |
| H5Location::getLinkInfo | See H5L_GET_INFO |
| H5Location::getNativeObjinfo | See H5O_GET_NATIVE_INFO |
| H5Location::getObjinfo | See H5O_GET_INFO |
| H5Object::visit | See H5O_VISIT |


## API Compatibility
See API Compatibility Macros in HDF5 for details on using HDF5 version 1.12 with previous releases.

[Compatibility report for Release 1.12.0 versus Release 1.10.6](Compatibility report for Release 1.12.0 versus Release 1.10.6)

 
