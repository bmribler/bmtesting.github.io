---
title: Migrating from HDF5 1.12 to HDF5 1.14
redirect_from: 
  - /display/HDF5/Migrating+from+HDF5+1.12+to+HDF5+1.14.html
---

# Migrating from HDF5 1.12 to HDF5 1.14

## API Changes
There are new API calls that require API Compatibility Macros in HDF5 1.14.0. There are, however, two API calls which have had their signature change.

### `H5Iregister_type()` / `H5I_free_t`
The signature of `H5Iregister_type()` did not change, but the `H5I_free_t` callback it accepts did have its signature change to add an asynchronous request parameter. There is no API compatibility macro to paper over this change. The request parameter can be ignored in the callback if you are not concerned with asynchronous operations and future IDs. A description of how the request parameter should be used will be found in the (soon to be released) HDF5 Asynchronous Programming Guide.

   &emsp;**Old:**

   &emsp;`typedef herr_t (*H5I_free_t)(void *obj);`

   &emsp;**New:**

   &emsp;`typedef herr_t (*H5I_free_t)(void *obj, void **request);`

### `H5VLquery_optional()`
The way optional operations are handled in the virtual object layer (VOL) changed significantly in HDF5 1.14.0. To better support this, the `H5VLquery_optional()` API call now takes an output flags parameter instead of a Boolean. Since the entire 1.12 VOL API has been deprecated, no API compatibility macro exists for this API call.

   &emsp;**Old:** 

   &emsp;`herr_t H5VLquery_optional(hid_t obj_id, H5VL_subclass_t subcls, int opt_type, hbool_t *supported);`

   &emsp;**New:**

   &emsp;`herr_t H5VLquery_optional(hid_t obj_id, H5VL_subclass_t subcls, int opt_type, uint64_t *flags);`

## Virtual File Layer (VFL) Changes
The virtual file layer has changed in HDF5 1.14.0. Existing virtual file drivers (VFDs) will have to be updated to work with this version of the library.

## Virtual Object Layer (VOL) Changes
The virtual object layer has changed significantly in HDF5 1.14.0 and the 1.12 VOL API is now considered deprecated and unsupported. Existing virtual object layer connectors should be updated to work with this version of the library.
