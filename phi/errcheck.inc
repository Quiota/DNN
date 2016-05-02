/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!******************************************************************************/

#include <stdlib.h>

void CheckVslError(int num)
{
    switch(num) {
        case VSL_ERROR_FEATURE_NOT_IMPLEMENTED: {
            printf("Error: this feature not implemented yet (code %d).\n",num);
            break;
        }
        case VSL_ERROR_UNKNOWN: {
            printf("Error: unknown error (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BADARGS: {
            printf("Error: bad arguments (code %d).\n",num);
            break;
        }
        case VSL_ERROR_MEM_FAILURE: {
            printf("Error: memory failure. Memory allocation problem maybe (code %d).\n",num);
            break;
        }
        case VSL_ERROR_NULL_PTR: {
            printf("Error: null pointer (code %d).\n",num);
            break;
        }
        case VSL_ERROR_INVALID_BRNG_INDEX: {
            printf("Error: invalid BRNG index (code %d).\n",num);
            break;
        }
        case VSL_ERROR_LEAPFROG_UNSUPPORTED: {
            printf("Error: leapfrog initialization is unsupported (code %d).\n",num);
            break;
        }
        case VSL_ERROR_SKIPAHEAD_UNSUPPORTED: {
            printf("Error: skipahead initialization is unsupported (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BRNGS_INCOMPATIBLE: {
            printf("Error: BRNGs are not compatible for the operation (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_STREAM: {
            printf("Error: random stream is invalid (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BRNG_TABLE_FULL: {
            printf("Error: table of registered BRNGs is full (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_STREAM_STATE_SIZE: {
            printf("Error: value in StreamStateSize field is bad (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_WORD_SIZE: {
            printf("Error: value in WordSize field is bad (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_NSEEDS: {
            printf("Error: value in NSeeds field is bad (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_NBITS: {
            printf("Error: value in NBits field is bad (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_UPDATE: {
            printf("Error: number of updated entries in buffer is invalid (code %d).\n",num);
            break;
        }
        case VSL_ERROR_NO_NUMBERS: {
            printf("Error: zero number of updated entries in buffer (code %d).\n",num);
            break;
        }
        case VSL_ERROR_INVALID_ABSTRACT_STREAM: {
            printf("Error: abstract random stream is invalid (code %d).\n",num);
            break;
        }
        case VSL_ERROR_FILE_CLOSE: {
            printf("Error: can`t close file (code %d).\n",num);
            break;
        }
        case VSL_ERROR_FILE_OPEN: {
            printf("Error: can`t open file (code %d).\n",num);
            break;
        }
        case VSL_ERROR_FILE_WRITE: {
            printf("Error: can`t write to file (code %d).\n",num);
            break;
        }
        case VSL_ERROR_FILE_READ: {
            printf("Error: can`t read from file (code %d).\n",num);
            break;
        }
        case VSL_ERROR_BAD_FILE_FORMAT: {
            printf("Error: file format is unknown (code %d).\n",num);
            break;
        }
        case VSL_ERROR_UNSUPPORTED_FILE_VER: {
            printf("Error: unsupported file version (code %d).\n",num);
            break;
        }
    }

    if(num < 0) {
       exit(1);
    }
}
