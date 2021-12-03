/*  LAST MODIFIED: 11/9/98 */

/**
 * This program converts binary data into chunked HDF data.
 * 
 * Command Line Arguments:
 *   1. binary file pathname
 *   2. output HDF filename
 *   3. "noattr" option - if this is specified, this program will
 *      not attempt to write any attributes for the dataset
 * 
 * This program trivially skips the first SKIP bytes because they
 * are assumed to be the header for the ingested data.
 * The next X_LENGTH*Y_LENGTH positions are interpreted as 16bit
 * unsigned data, and the remaining data is used to extract attributes.
 *
 * The output of this file is verbose, and the 'status' for each
 * step is shown in the output. 
 *
 * Author: Apu Kapadia (apu@ncsa.uiuc.edu)
 */

#include <mfhdf.h>
#include <hdf.h>

#include <stdio.h>
#include <stdlib.h>

/* X and Y dimensions of the dataset */
#define Y_LENGTH 6144
#define X_LENGTH 6144

/* Chunking dimensions */
#define CHUNK_X 256
#define CHUNK_Y 256

/* The header size for the ingested data that is to be 
   skipped before reading the 16bit unsigned data */
#define SKIP 1536 /* 3 * 512 */

int attributes=TRUE;

/* Valid FRACTIONS include (6144/FRACTION MUST be an integer)
   1,2,4,8,16,32,64,128,256,512,1024,2048
   1 is the most preferable, and 2048 is the least preferable.
   Certain OS'es complain about the array sizes, and hence we have to 
   deal with 1/FRACTION of the dataset at a time. It seems that 2 works
   well in most cases.
   
   If FRACTION fails, then the value is doubled, and an attempt to 
   allocate memory with this new value is made. 

   If this scheme fails for some reason, you can manually specify the
   value of FRACTION below.
*/
int FRACTION=24;

#define MAX_FRACTION 128

/**
 * This function checks the status of an operation. If a -1 
 * was received, then the program exits and displays the appropriate
 * error message
 */
void check(int status)
{
  if(status == -1)
    {
      printf("\nreceived a -1...terminating\n");
      printf("Valid data was NOT written to the output file\n");
      exit(1);
    }
}

/**
 * This function prints out whether the status is OK or FAIL.
 */
void printStatusString(int status)
{
  if(status >= 0)
    printf("OK");
  else
    printf("FAIL");
}


/**
 * Main function
 * Sets up the input and output files
 * Sets the chunking parameters
 * Iterates through the binary dataset and writes the 
 * corresponding data to the output hdf file.
 */

void main(int argc, char* argv[])
{
  int fid_out;
  FILE *inFile;
  int32 nt, edges[2], start[2];
  int sds_id;

  char *outFileName, *inFileName;
  char c;
  char *s;

  /* This array is used to write two dimensional data to the hdf file */
  uint16 *image;
  uint32 *image32; 

  uint16 pixVal;

  int loop;
  int i,j, jj;
  int status;
  int32 chunkFlag;
  HDF_CHUNK_DEF c_def, r_def; 
  int chunkflag;
  int cachesize;
  
  float clon=-1, clat=-1, dlon=-1, dlat=-1;
  float attrVal;

  /* check arguments */
 
  s = (char *)malloc(5000);

  if(argc == 4)
    {
      if(strcmp(argv[3], "noattr") == 0)
        attributes = FALSE; 
    }

  if(argc != 3 && argc != 4)
    {
      printf("Usage: %s <binary file> <outfile.hdf> [noattr]\n", argv[0]);
      exit(0);
    }

  /* initialize parameters */
  edges[0] = Y_LENGTH;
  edges[1] = X_LENGTH;
  start[0] = 0;
  start[1] = 0;

  /* write the data out in HDF as 32 signed ints, so the DODS server */
  /* can read it */
  nt = DFNT_UINT32;

  outFileName = argv[2];
  inFileName = argv[1];

  /* Allocate image array */

  printf("Allocating image array...");
  
  
  image = (uint16 *)HDmalloc(sizeof(uint16)*(X_LENGTH*Y_LENGTH)/FRACTION);
  image32 = (uint32 *)HDmalloc(sizeof(uint32)*(X_LENGTH*Y_LENGTH)/FRACTION);
  while((image == NULL || image32 ==NULL) && (FRACTION <= MAX_FRACTION))
    {
      free(image); free(image32);
      printf("FRACTION=%d FAIL...", FRACTION);
      FRACTION *= 2;
      printf("Trying FRACTION=%d\n", FRACTION);
      image = (uint16 *)HDmalloc(sizeof(uint16)*(X_LENGTH*Y_LENGTH)/FRACTION);
      image32 = (uint32 *)HDmalloc(sizeof(uint32)*(X_LENGTH*Y_LENGTH)/FRACTION);
    }
  if(FRACTION >= 64)
    {
      printf("Unable to allocate memory\n");
      exit(1);
    }

  if((image == NULL) || (image32 == NULL))
    {
      printf("FAIL\n");
      exit(1);
    }
  printf("done\n");

  /* Open input and output files */
  printf("opening input binary file %s...", inFileName); fflush(stdout);
  inFile = fopen(inFileName, "r");
  if(inFile == NULL)
    {
      printf("Unable to open input file...terminating\n");
      exit(0);
    }
  printf("done\n");
  
  printf("opening output file %s...", outFileName); fflush(stdout);
  fid_out = SDstart(outFileName, DFACC_CREATE);
  if(fid_out == FAIL)
    {
      printf("Unable to create output file...terminating\n");
      exit(0);
    }
  printf("done\n");


  /* Set the chunking parameters */

  printf("setting chunking parameters..."); fflush(stdout);
  /* create SD in HDF file */
  sds_id = SDcreate(fid_out, "Binary Data", nt, 2,
		    edges);
  printf("sds_id = %d...", sds_id);
  check(sds_id);

  c_def.chunk_lengths[0] = CHUNK_Y;
  c_def.chunk_lengths[1] = CHUNK_X;
  
  c_def.comp.chunk_lengths[0] = CHUNK_Y;
  c_def.comp.chunk_lengths[1] = CHUNK_X;
  c_def.comp.comp_type = COMP_CODE_DEFLATE; /* GZIP */
  c_def.comp.cinfo.deflate.level = 6; 
  chunkFlag = HDF_CHUNK | HDF_COMP;

  status = SDsetchunk(sds_id, c_def, chunkFlag);
  printf("SDsetchunk status = %d...", status);
  check(status);

  chunkflag = 0; 
  cachesize = X_LENGTH/CHUNK_X;
  status = SDsetchunkcache(sds_id, cachesize, chunkflag);
  printf("cache status = %d...", status);
  check(status);
  printf("done\n");

  /* Skip first SKIP bytes */

  printf("skipping first %d bytes...", SKIP); fflush(stdout);
  fseek(inFile, SKIP, 0);
  printf("done\n");

  /* loop through binary file */
  edges[0] = Y_LENGTH/FRACTION;

  /* extract 1/FRACTION of the dataset at a time (to conserve memory) */

  for(loop=0;loop<FRACTION;loop++)
    {
      printf("%d/%d - reading...", loop+1, FRACTION);fflush(stdout);
      status = fread(image, sizeof(uint16)*X_LENGTH*(Y_LENGTH/FRACTION), 1, inFile);
      printf("(status = ");
      printStatusString(status);
      printf("), ");
      check(status);

      for ( j = 0; j < (Y_LENGTH/FRACTION); j++) {
      for ( i = 0; i < X_LENGTH; i++) {
      *(image32+((j*X_LENGTH)+i))=*(image+((j*X_LENGTH)+i));
      }
      }
  

      
      printf("writing..."); fflush(stdout);
      status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)image32);
      printf("(status = ");
      printStatusString(status);
      printf(")\n");

      check(status);
    
      start[0] += Y_LENGTH/FRACTION;
    }
 
  printf("Done writing data\n");

  /*********************  Begin Attributes *******************/  
  
  if(attributes == TRUE)
    {
      
      printf("Writing attributes...");fflush(stdout);
      
      /* Search the tail of the file for info on Center Lat/Lon and
	 delta lat/lon */
      
      while(fscanf(inFile, "%s",s) > 0)
	{
	  if(strcmp(s,"center") == 0)
	    {
	      fscanf(inFile, " lon,lat,dlon,dlat = %f %f %f %f", &clon, &clat, 
		     &dlon, &dlat);
	      break;
	    }
	}
      
      attrVal = 1.25;
      SDsetattr(sds_id, "slope", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      attrVal = -4.0;
      SDsetattr(sds_id, "intercept", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      attrVal = clon;
      SDsetattr(sds_id, "center longitude", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      attrVal = clat;
      SDsetattr(sds_id, "center latitude", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      attrVal = dlon;
      SDsetattr(sds_id, "delta longitude", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      attrVal = dlat;
      SDsetattr(sds_id, "delta latitude", DFNT_FLOAT32, 1, (VOIDP) &attrVal);
      
      printf("done\n");
    }
  /*********************  End Attributes *********************/

  printf("closing files, please wait..."); fflush(stdout);

  SDendaccess(sds_id);
  SDend(fid_out);
  fclose(inFile);
  printf("done\n");
  free(image);
  printf("Data was successfully written to %s\n", outFileName);
}



