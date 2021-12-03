
#include "mfhdf.h"
#include "szlib.h"

int verbose = 0;

int
main ()
{
  int status;
  uint32 comp_config;

  /* Check if SZIP encoding is available */

  status = HCget_config_info (COMP_CODE_SZIP, &comp_config);
  if (verbose)
    printf ("status: %d szip config flags: 0x%X\n", status, comp_config);
  if ((comp_config & (COMP_DECODER_ENABLED | COMP_ENCODER_ENABLED)) == 0)
    {
      /* coder not present?? */
      if (verbose)
	printf ("SZIP not configured\n");
      return (2);
    }
  if ((comp_config & COMP_ENCODER_ENABLED) == 0)
    {
      /* decoder not present?? */
      if (verbose)
	printf ("SZIP encoding not allowed\n");
      return (1);
    }

  if (verbose)
    printf ("SZIP encoder and decoder present\n");

  return (0);
}
