---
title: API Compatibility Macros
redirect_from:
  - display/HDF5/API+Compatibility+Macros
---

# API Compatibility Macros

## Audience
The target audience for this document has existing applications that use the HDF5 library, and is considering moving to the latest HDF5 release to take advantage of the latest library features and enhancements.

## Compatibility Issues
With each major release of HDF5, such as 1.12 or 1.10, certain compatibility issues must be considered when migrating applications from an earlier major release.

This document describes the approach taken by The HDF Group to help existing users of HDF5 address compatibility issues in the HDF5 API.

## Summary and Motivation
In response to new and evolving requirements for the library and data format, several basic functions have changed since HDF5 was first released. To allow existing applications to continue to compile and run properly, all versions of these functions have been retained in the later releases of the HDF5 library.

Given the scope of changes available with each major release of HDF5, and recognizing the potentially time-consuming task of editing all the affected calls in user applications, The HDF Group has created a set of macros that can be used to flexibly and easily map existing API calls to previous release functions. We refer to these as the API compatibility macros.

The HDF Group generally encourages users to update applications to work with the latest HDF5 library release so that all new features and enhancements are available to them. At the same time, The HDF Group understands that, under some circumstances, updating applications may not be feasible or necessary. The API compatibility macros, described in this document, provide a bridge from old APIs to new and can be particularly helpful in situations such as these:

* Source code is not available - only binaries are available; updating the application is not feasible.
* Source code is available, but there are no resources to update it.
* Source code is available, as are resources to update it, but the old version works quite well so updates are not a priority. At the same time, it is desirable to take advantage of certain efficiencies in the newer HDF5 release that do not require code changes.
* Source code is available, as are resources to update it, but the applications are large or complex, and must continue to run while the code updates are carried out.

## Understanding and Using the Macros
As part of latest HDF5 release, several functions that existed in previous versions of the library were updated with new calling parameters and given new names. The updated versions of the functions have a number (for eg '2') at the end of the original function name. The original versions of these functions were retained and renamed to have an earlier number (for eg '1') at the end of the original function name.

For example, consider the function H5Lvisit in HDF5 release 1.10 as compared with 1.12:

|               |                 |
| ------------------------------------- | ------------------------------------------------------------------------------------------------- |
| **Original function name and signature in 1.10.0** | herr\_t H5Lvisit(hid\_t grp\_id, H5\_index\_t idx\_type, H5\_iter\_order\_t order, H5L\_iterate\_t op, void \*op\_data) |
| **Updated function and signature, introduced in release 1.12.0** | herr\_t H5Lvisit2(hid\_t group\_id, H5\_index\_t idx\_type, H5\_iter\_order\_t order, H5L\_iterate2\_t op, void \*op\_data) |
| **Original function and signature, renamed in release 1.12.0**   | herr\_t H5Lvisit1(hid\_t group\_id, H5\_index\_t idx\_type, H5\_iter\_order\_t order, H5L\_iterate1\_t op, void \*op\_data) |
| **API compatibility macro, introduced in release 1.12.0** | H5Lvisit <br>The macro, H5Lvisit, will be mapped to either H5Lvisit1 or H5Lvisit2. The mapping is determined by a combination of the configuration options use to build the HDF5 library and compile-time options used to build the application. The calling parameters used with the H5Lvisit compatibility macro should match the number and type of the function the macros will be mapped to (H5Lvisit1 or H5Lvisit2). <br>The function names ending in '1' or '2' are referred to as versioned names, and the corresponding functions are referred to as versioned functions. For new code development, The HDF Group recommends use of the compatibility macro mapped to the latest version of the function. The original version of the function should be considered deprecated and, in general, should not be used when developing new code. |

## Compatibility Macro Mapping Options

To determine the mapping for a given API compatibility macro in a given application, a combination of user-controlled selections, collectively referred to as the compatibility macro mapping options, is considered in the following sequence:

What compatibility macro configuration option was used to build the HDF5 library? We refer to this selection as the library mapping.

Was a compatibility macro global compile-time option specified when the application was built? We refer to this (optional) selection as the application mapping. If an application mapping exists, it overrides the library mapping. (See adjacent notes.)

Were any compatibility macro function-level compile-time options specified when the application was built? We refer to these (optional) selections as function mappings. If function mappings exist, they override library and application mappings for the relevant API compatibility macros. (See adjacent notes.)

Notes: An application mapping can map APIs to the same version or to a version older than the configured library mapping. When the application attempts to map APIs to a newer version of the API than the library was configured with, it will fail to "upgrade" the mapping (and may fail silently).
When it is necessary to "upgrade" the macro mappings from those set in the library mapping, it must be done at the per-function level, using the function-level mappings. As long as one does not try to map a function to a version that was compiled out in the library mapping, individual functions can be upgraded or downgraded freely.


### Library Mapping Options

When the HDF5 library is built, configure flags can be used to control the API compatibility macro mapping behavior exhibited by the library. This behavior can be overridden by application and function mappings. One configure flag excludes deprecated functions from the HDF5 library, making them unavailable to applications linked with the library.



Table 1:  Library Mapping Options

| configure flag | Macros map to release<br>(versioned function; H5Lvisit shown) | Deprecated functions available?<br>(H5Lvisit1) |
| -------------- | ------------------------------------------------------------- | ---------------------------------------------- |
| --with-default-api-version=v112 <br> (the default in 1.12) | 1.12.x   (H5Lvisit2) |   yes |
| --with-default-api-version=v110                            | 1.10.x   (H5Lvisit1) |   yes |
| --with-default-api-version=v18 | 1.8.x     (H5Lvisit1) | yes |
| --with-default-api-version=v16 | 1.6.x     (H5Lvisit1) | yes |
| --disable-deprecated-symbols   | 1.12.x    (H5Lvisit2) | no  |


Refer to the file libhdf5.settings in the directory where the HDF5 library is installed to determine the configure flags used to build the library. In particular, look for the two lines shown here under Features:

   Default API mapping: v112

  With deprecated public symbols: yes

### Application Mapping Options

When an application using HDF5 APIs is built and linked with the HDF5 library, compile-time options to h5cc can be used to control the API compatibility macro mapping behavior exhibited by the application. The application mapping overrides the behavior specified by the library mapping, and can be overridden on a function-by-function basis by the function mappings.

If the HDF5 library was configured with the --disable-deprecated-symbols flag, then the deprecated functions will not be available, regardless of the application mapping options.

Table 2:  Application Mapping Options

| h5cc option | Macros map to release<br>(versioned function; H5Lvisit shown) | Deprecated functions available?<br>(H5Lvisit1) |
| -------------- | ------------------------------------------------------------- | ---------------------------------------------- |
| -DH5\_USE\_112\_API<br> (Default behavior if no option specified.) | 1.12.x   (H5Lvisit2) |  yes<br>\*if available in library |
| -DH5\_USE\_110\_API                                                | 1.10.x   (HLvisit1) |  yes\*<br>\*if available in library |
| -DH5\_USE\_18\_API                                               | 1.8.x     (H5Lvisit1) |  yes\*
\*if available in library |
| -DH5\_USE\_16\_API                                               | 1.6.x     (H5Lvisit1) |  yes\*
\*if available in library |
| -DH5\_NO\_DEPRECATED\_SYMBOLS                                                | 1.10.x    (H5Lvisit1) |  no |


### Function Mapping Options

Function mappings are specified when the application is built. These mappings can be used to control the mapping of the API compatibility macros to underlying functions on a function-by-function basis. The function mappings override the library and application mappings discussed earlier.

If the HDF5 library was configured with the --disable-deprecated-symbols flag, or -DH5\_NO\_DEPRECATED\_SYMBOLS is used to compile the application, then the deprecated functions will not be available, regardless of the function mapping options.

For every function with multiple available versions, a compile-time version flag can be defined to selectively map the function macro to the desired versioned function. The function mapping consists of the function name followed by "\_vers" which is mapped by number to a specific function or struct:

| Macro | Function Mapping | Mapped to function or struct |
| ----- | --------------------- | -------------------------- |
| H5xxx |  H5xxx\_vers=1  |  H5xxx1 |
|       |  H5xxx\_vers=2  |  H5xxx2 |

For example, in version 1.10 the H5Rreference macro can be mapped to either H5Rreference1 or H5Rreference2. When used, the value of the H5Rreference\_vers compile-time version flag determines which function will be called:

* When H5Rreference\_vers is set to 1, the macro H5Rreference will be mapped to H5Rreference1.
     h5cc ... -DH5Rreference\_vers=1 ...


* When H5Rdereference\_vers is set to 2, the macro H5Rdereference will be mapped to H5Rdereference2.
     h5cc ... -DH5Rreference\_vers=2 ...


* When H5Rreference\_vers is not set, the macro H5Rreference will be mapped to either H5Rreference1 or H5Rreference2, based on the application mapping, if one was specified, or on the library mapping.
     h5cc ...

~~~
Please be aware that some function mappings use mapped structures, as well.  If compiling an application with a function mapping that uses a mapped structure, you must include each function and mapped structure plus EVERY function that uses the mapped structure, whether or not that function is used in the application. In 1.12, mappings of structures are used by the H5L and H5O function mappings.

For example, the application h5ex\_g\_iterate.c (found on the Examples by API page under "Groups") only calls H5Lvisit , H5Ovisit , and H5Oget\_info\_by\_name. <br>To compile this application with 1.10 APIs in 1.12 with the function specific mappings, then not only must H5Lvisit\_vers, H5Ovisit\_vers, and H5Oget\_info\_by\_name\_vers be specified on the command line, but the mapped structures and every function that uses the mapped structures must be included, as well. <br> The full compile line is shown below:
                                                                                                
h5cc -DH5Lvisit\_vers=1 -DH5Ovisit\_vers=1 -DH5Oget\_info\_by\_name\_vers=1 -DH5Lvisit\_by\_name\_vers=1 -DH5Literate\_vers=1 -DH5Literate\_by\_name\_vers=1 -DH5O\_info\_t\_vers=1 -DH5L\_info\_t\_vers=1 -DH5L\_iterate\_t\_vers=1 -DH5Lget\_info\_by\_idx\_vers=1 -DH5Lget\_info\_vers=1 h5ex\_g\_visit.c
~~~

#### Function Mapping Options in Releases 1.12.x

|   Macro<br>H5xxx    | Default function used if no macro specified)<br> Function/struct mapping: H5xxx\_vers=N | Function used if specifying 1.10<br>Function/struct mapping: H5xxx\_vers=1 |
| ------------------- | ------------------------------------------------ | --------------------------- |
| H5L\_GET\_INFO      | H5L\_GET\_INFO2<br>* Function mapping: H5Lget\_info\_vers=2<br>* Struct mapping: H5L\_info\_vers=2 | H5L\_GET\_INFO1<br>* Function mapping H5Lget\_info\_vers=1<br>* Struct mapping: H5L\_info\_t\_vers=1 |
| H5L\_GET\_INFO\_BY\_IDX | H5L\_GET\_INFO\_BY\_IDX2<br> * Function mapping: H5Lget\_info\_by\_idx\_vers=2<br> * Struct mapping: H5L\_info\_t\_vers=2 | H5L\_GET\_INFO\_BY\_IDX1<br> * Function mapping: H5Lget\_info\_by\_idx\_vers=1<br> * Struct mapping: H5L\_info\_t\_vers=1 |
| H5L\_ITERATE | H5L\_ITERATE2<br> * Function mapping: H5Literate\_vers=2<br> * Struct mapping: H5L\_iterate\_t\_vers=2<br> | H5L\_ITERATE1<br> * Function mapping: H5Literate\_vers=1<br> * Struct mapping: H5L\_iterate\_t\_vers=1<br> |
| H5L\_ITERATE\_BY\_NAME | H5L\_ITERATE\_BY\_NAME2<br> * Function mapping: H5Literate\_by\_name\_vers=2<br> * Struct mapping: H5L\_iterate\_t\_vers=2 | H5L\_ITERATE\_BY\_NAME1<br> * Function mapping: H5Literate\_by\_name\_vers=1<br> * Struct mapping: H5L\_iterate\_t\_vers=1 |
| H5L\_VISIT | H5L\_VISIT2<br> * Function mapping: H5Lvisit\_vers=2<br> * Struct mapping: H5L\_iterate\_t\_vers=2 | H5L\_VISIT1<br> * Function mapping: H5Lvisit\_vers=1<br> * Struct mapping: H5L\_iterate\_t\_vers=1<br>  |
| H5L\_VISIT\_BY\_NAME | H5L\_VISIT\_BY\_NAME2<br> * Function mapping: H5Lvisit\_by\_name\_vers=2<br> * Struct mapping: H5L\_iterate\_t\_vers=2 | H5L\_VISIT\_BY\_NAME1<br> * Function mapping: H5Lvisit\_by\_name\_vers=1<br> * Struct mapping: H5L\_iterate\_t\_vers=1 |
| H5O\_GET\_INFO | H5O\_GET\_INFO3<br> * Function mapping: H5Oget\_info\_vers=3<br> * Struct mapping: H5O\_info\_t\_vers=2 | H5O\_GET\_INFO1<br> * Function mapping: H5Oget\_info\_vers=1<br> * Struct mapping: H5O\_info\_t\_vers=1 |
| H5O\_GET\_INFO\_BY\_IDX | H5O\_GET\_INFO\_BY\_IDX3<br> * Function mapping: H5Oget\_info\_by\_idx\_vers=3<br> * Struct mapping: H5O\_info\_t\_vers=2 | H5O\_GET\_INFO\_BY\_IDX1<br> * Function mapping: H5Oget\_info\_by\_idx\_vers=1<br> * Struct mapping: H5O\_info\_t\_vers=1 |
| H5O\_GET\_INFO\_BY\_NAME | H5O\_GET\_INFO\_BY\_NAME3<br> * Function mapping: H5O\_get\_info\_by\_name\_vers=3<br> * Struct mapping: H5O\_info\_t\_vers=2 | H5O\_GET\_INFO\_BY\_NAME1<br> * Function mapping: H5O\_get\_info\_by\_name\_vers=1<br> * Struct mapping: H5O\_info\_t\_vers=1 |
| H5O\_VISIT | H5O\_VISIT3<br> * Function mapping: H5Ovisit\_vers=3<br> * Struct mapping: H5O\_iterate\_t\_vers=2 | H5O\_VISIT1<br> * Function mapping: H5Ovisit\_vers=1<br> * Struct mapping: H5O\_iterate\_t\_vers=1 |
| H5O\_VISIT\_BY\_NAME | H5O\_VISIT\_BY\_NAME3<br> * Function mapping: H5Ovisit\_by\_name\_vers=3<br> * Struct mapping: H5O\_iterate\_t\_vers=2 | H5O\_VISIT\_BY\_NAME1<br> * Function mapping: H5Ovisit\_by\_name\_vers=1<br> * Struct mapping: H5O\_iterate\_t\_vers=1 |
| H5P\_ENCODE | H5P\_ENCODE2<br> * Function mapping: H5Pencode\_vers=2 | H5P\_ENCODE1<br> * Function mapping: H5Pencode\_vers=1 |
| H5S\_ENCODE | H5S\_ENCODE2<br> * Function mapping: H5Sencode\_vers=2 | H5S\_ENCODE1<br> * Function mapping: H5Sencode\_vers=1 |


#### Function Mapping Options in Releases 1.10.x

The versioned H5Oget\_info functions (H5Oget\_info1 and H5Oget\_info2) were added in 1.10.3, and H5Oget\_info was replaced by a macro to invoke H5Oget\_info1 or H5Oget\_info2. However, this broke compatibility and caused problems for users because there was no longer a function H5Oget\_info.  In 1.10.4 and subsequent 1.10.x versions the macro was removed, H5Oget\_info1 was deprecated, and H5Oget\_info was resurrected as a function. H5Oget\_info2 remained as a function, but is not a versioned alternative to the original H5Oget\_info. The same is true for H5Oget\_info\_by name, H5Oget\_info\_by\_idx, H5Ovisit, and H5Ovisit\_by\_name. The version 2 functions were added to improve performance.

The unversioned originals and version 2 of those functions exist in 1.10 because having released them in 1.10.3, it would break compatibility to remove them, so the original and version 2 functions remained in the source but without any macro to map to one or the other.  Therefore, version 2 functions are available, but only when invoked directly.

In 1.12 there is a version 3 of all 5 functions which uses version 2 H5Oinfo2\_t or H5Oiterate2\_t structures.  Both versions 1 and 2 are deprecated and macros replace the unversioned functions, mapping to version 1 for 18 and 110 default apis and to version 3 for 112 default api.  Version 2 is available, but will only be invoked if invoked directly.


|       Macro         | Default function used<br>(if no macro specified) | Introduced in   | h5cc version flag and value | Mapped to function or struct |
| ------------------- | ----------------------------- | ------------ | ------------------------------------- | ------------------------------------ |
| H5Rdereference | H5Rdereference2 | 1.10.0 | -DH5Rdereference\_vers=1 | H5Rdereference1 |
|                |                 |             | -DH5Rdereference\_vers=2 | H5Rdereference2 |
| H5Fget\_info   | H5Fget\_info2   | 1.10.0 | -DH5Fget\_info\_vers=1   | H5Fget\_info1 with struct H5F\_info1\_t |
|                |                 |             | -DH5Fget\_info\_vers=2    H5Fget\_info2 with struct H5F\_info2\_t |
| H5Oget\_info   | H5Oget\_info1   | 1.10.3 | -DH5Oget\_info\_vers=1    H5Oget\_info1 |
|                |                 |             | -DH5Oget\_info\_vers=2    H5Oget\_info2 |
| H5Oget\_info\_by\_idx | H5Oget\_info\_by\_idx1 | 1.10.3 | -DH5Oget\_info\_by\_idx\_vers=1 | H5Oget\_info\_by\_idx1 |
|                |                 |             | -DH5Oget\_info\_by\_idx\_vers=2 H5Oget\_info\_by\_idx2 |
| H5Oget\_info\_by\_name | H5Oget\_info\_by\_name1 | 1.10.3 | -DH5Oget\_info\_by\_name\_vers=1 | H5Oget\_info\_by\_name1 |
|                |                 |             | -DH5Oget\_info\_by\_name\_vers=2    H5Oget\_info\_by\_name2 |
| H5Ovisit       | H5Ovisit1       | 1.10.3 | -DH5Ovisit\_vers=1 |   H5Ovisit1 |
|                |                 |             | -DH5Ovisit\_vers=2 | H5Ovisit2 |
| H5Ovisit\_by\_name | H5Ovisit\_by\_name1 | 1.10.3 | -DH5Ovisit\_by\_name\_vers=1 | H5Ovisit\_by\_name1 |
|                |                 |             | -DH5Ovisit\_by\_name\_vers=2    H5Ovisit\_by\_name2 |

#### Function Mapping Options in Releases 1.8.x

At release 1.8.0, the API compatibility macros, function mapping compile-time version flags and values, and corresponding versioned functions listed in the following table were introduced. If the application being compiled to run with any 1.10.x release was written to use any 1.6.x release of HDF5, you must also consider these macros and mapping options.


##### Table 5:  Function Mapping Options in Releases 1.8.x

| Macro  |  h5cc version flag and value  |  Mapped to function or struct |
| ------ | ----------------------------- | ----------------------------- |
| H5Acreate | -DH5Acreate\_vers=1   | H5Acreate1 |
|        | -DH5Acreate\_vers=2  | H5Acreate2 |
| H5Aiterate | -DH5Aiterate\_vers=1 |   H5Aiterate1 <br>with struct H5A\_operator1\_t |
|            | -DH5Aiterate\_vers=2 |   H5Aiterate2 <br>with struct H5A\_operator2\_t |
| H5Dcreate  | -DH5Dcreate\_vers=1 |   H5Dcreate1 |
|            | -DH5Dcreate\_vers=2 |   H5Dcreate2 |
| H5Dopen    | -DH5Dopen\_vers=1 |   H5Dopen1 |
|            | -DH5Dopen\_vers=2 |   H5Dopen2 |
| H5Eclear   | -DH5Eclear\_vers=1 |   H5Eclear1 |
|            | -DH5Eclear\_vers=2 |   H5Eclear2 |
| H5Eprint   | -DH5Eprint\_vers=1 |   H5Eprint1 |
|            | -DH5Eprint\_vers=2 |   H5Eprint2 |
| H5Epush    |-DH5Epush\_vers=1 |   H5Epush1 |
|            | -DH5Epush\_vers=2 |   H5Epush2 |
| H5Eset\_auto |   -DH5Eset\_auto\_vers=1 |   H5Eset\_auto1 |
|            | -DH5Eset\_auto\_vers=2 |   H5Eset\_auto2 |
| H5Eget\_auto |   -DH5Eget\_auto\_vers=1 |   H5Eget\_auto1 |
|            | -DH5Eget\_auto\_vers=2 |   H5Eget\_auto2 |
| H5E\_auto\_t<br>Struct for H5Eset\_auto<br>and H5Eget\_auto |   -DH5E\_auto\_t\_vers=1 |  H5E\_auto1\_t |
|            | -DH5E\_auto\_t\_vers=2 |   H5E\_auto2\_t |
| H5Ewalk    | -DH5Ewalk\_vers=1 |    H5Ewalk1 with callback H5E\_walk1\_t and struct H5E\_error1\_t |
|            | -DH5Ewalk\_vers=2 |   H5Ewalk2 with callback H5E\_walk2\_t and struct H5E\_error2\_t |
| H5Gcreate  | -DH5Gcreate\_vers=1 |   H5Gcreate1 |
|            | -DH5Gcreate\_vers=2    H5Gcreate2 |
| H5Gopen    | -DH5Gopen\_vers=1 |   H5Gopen1 |
|            | -DH5Gopen\_vers=2 |   H5Gopen2 |
| H5Pget\_filter |   -DH5Pget\_filter\_vers=1 |   H5Pget\_filter1 |
|            | -DH5Pget\_filter\_vers=2 |   H5Pget\_filter2 |
| H5Pget\_filter\_by\_id |   -DH5Pget\_filter\_by\_id\_vers=1 |   H5Pget\_filter\_by\_id1 |
|            | -DH5Pget\_filter\_by\_id\_vers=2 |   H5Pget\_filter\_by\_id2 |
| H5Pinsert  | -DH5Pinsert\_vers=1 |   H5Pinsert1 |
|            | -DH5Pinsert\_vers=2 |   H5Pinsert2 |
| H5Pregister   | -DH5Pregister\_vers=1 |   H5Pregister1 |
|            | -DH5Pregister\_vers=2 |   H5Pregister2 |
| H5Rget\_obj\_type   | -DH5Rget\_obj\_typever\_vers=1 |   H5Rget\_obj\_type1 |
|            | -DH5Rget\_obj\_type\_vers=2 |   H5Rget\_obj\_type2 |
| H5Tarray\_create   | -DH5Tarray\_create\_vers=1 |   H5Tarray\_create1 |
|            | -DH5Tarray\_create\_vers=2 |   H5Tarray\_create2 |
| H5Tcommit  | -DH5Tcommit\_vers=1 |   H5Tcommit1 |
|            | -DH5Tcommit\_vers=2 |   H5Tcommit2 |
| H5Tget\_array\_dims   | -DH5Tget\_array\_dims\_vers=1 |   H5Tget\_array\_dims1 |
|            | -DH5Tget\_array\_dims\_vers=2 |   H5Tget\_array\_dims2 |
| H5Topen    | -DH5Topen\_vers=1  |  H5Topen1 |
|            | -DH5Topen\_vers=2  |  H5Topen2 |
| H5Z\_class\_t Struct for H5Zregister |  -DH5Z\_class\_t\_vers=1 |   H5Z\_class1\_t |
|            | -DH5Z\_class\_t\_vers=2 |   H5Z\_class2\_t |

_Further Information_

See the [HDF5 Reference Manual]()  for complete descriptions of all API compatibility macros and versioned functions shown.

It is possible to specify multiple function mappings for a single application build:

h5cc ... -DH5Rdereference\_vers=1 -DH5Fget\_info\_vers=2 ...As a result of the function and struct mappings in this compile example, all occurrences of the macro H5Rdereference will be mapped to H5Rdereference1 and all occurrences of the macro H5Fget\_info will be mapped to H5Fget\_info2 for the application being built.

The function and struct mappings can be used to guarantee that a given API compatibility macro will be mapped to the desired underlying function or struct version regardless of the library or application mappings. In cases where an application may benefit greatly from features offered by some of the later APIs, or must continue to use some earlier API versions for compatibility reasons, this fine-grained control may be very important.

As noted earlier, the function mappings can only reference versioned functions that are included in the HDF5 library, as determined by the configure flag used to build the library. For example, if the HDF5 library being linked with the application was built with the --disable-deprecated-symbols option, version 1 of the underlying functions would not be available, and the example above that defined H5Rdereference\_vers=1 would not be supported.

The function mappings do not negate any available functions. If H5Rdereference1 is available in the installed version of the HDF5 library, and the application was not compiled with the -DH5\_NO\_DEPRECATED\_SYMBOLS flag, the function H5Rdereference1 will remain available to the application through its versioned name. Similarly, H5Rdereference2 will remain available to the application as H5Rdereference2. The function mapping version flag H5Rdereference\_vers only controls the mapping of the API compatibility macro H5Rdereference to one of the two available functions.

This can be especially useful in any case where the programmer does not have direct control over global macro definitions, such as when writing code meant to be copied to multiple applications or when writing code in a header file.

## Compatibility Macros in HDF5 1.6.8 and Later
A series of similar compatibility macros were introduced into the release 1.6 series of the library, starting with release 1.6.8. These macros simply alias the '1' version functions, callbacks, and typedefs listed above to their original non-numbered names.

These macros were strictly a forward-looking feature at that time; they were not necessary for compatibility in 1.6.x. These macros were created at that time to enable writing code that could be used with any version of the library after 1.6.8 and any library compilation options except H5\_NO\_DEPRECATED\_SYMBOLS, by always using the '1' version of versioned functions and types. For example, H5Dopen1 will always be interpreted in exactly the same manner by any version of the library since 1.6.8.

## Common Use Case

A common scenario where the API compatibility macros may be helpful is the migration of an existing application to a new HDF5 release An incremental migration plan is outlined here:

1. Build the HDF5 library without specifying any library mapping configure flag. In this default mode, the 1.6.x, 1.8.x, and 1.10.x versions of the underlying functions are available, and the API compatibility macros will be mapped to the current HDF5 versioned functions.

2. Compile the application with the -DH5\_USE\_NN\_API application mapping option if it was written for use with an earlier HDF5 library.  Because the application mapping overrides the library mapping, the macros will all be mapped to the earlier versions of the functions.

3. Remap one API compatibility macro at a time (or sets of macros), to use the current HDF5 versions. At each stage, use the function mappings to map the macros being worked on to the current versions. For example, use the -DH5Rdereference\_vers=2 version flag setting to remap the H5Rdereference macro to H5Rdereference2, the 1.10.x version. <br>
During this step, the application code will need to be modified to change the calling parameters used with the API compatibility macros to match the number and type of the 1.10.x versioned functions. The macro name, for example H5Rdereference, should continue to be used in the code, to allow for possible re-mappings to later versioned functions in a future release.

4. After all macros have been migrated to the latest versioned functions in step 3, compile the application without any application or function mappings. This build uses the library mappings set in step 1, and maps API compatibility macros to the latest versions.

5. Finally, compile the application with the application mapping -DH5\_NO\_DEPRECATED\_SYMBOLS, and address any failures to complete the application migration process.
