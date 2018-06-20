/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : ncdf_animation
 * COMMENT  :
 *
 * $Id: ncdf_animation.c 7753 2015-11-20 22:19:18Z markstro $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <netcdf.h>
#include <time.h>
#include <unistd.h>
#include "mms.h"

/* Handle NetCDF errors by printing an error message and exiting with a
 * non-zero status. */
#define ERR(e) {printf("NetCDF Error: %s\n", nc_strerror(e)); exit (2);}



/**5*********************** LOCAL VARIABLES ***************************/
void ***data = NULL;
int ts_ani, ts, n_hru;
static void ncVarAttributes (int, int, char *, char *, float *, char *, char *, char *);
static void globalAttributsForCf (int);

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : ncdfInitAni
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ncdfInitAni(long naniVars, PUBVAR **ani_out_vars) {
    int i;

    ts = 0;
    ts_ani = Mendtime->jd - Mstrttime->jd + 1;
    n_hru = getdim("nhru");

    for (i = 0; i < naniVars; i++) {
        ani_out_vars[i]->d = (float *) umalloc(n_hru * ts_ani * sizeof(float));
    }
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ncdfCloseAni
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int ncdfCloseAni(long naniVars, PUBVAR **ani_out_vars, int ani_out_flag) {
    int retval;
    int ncdf, nc_hru_dim, nc_hru_name_dim, nc_time_dim, nc_hru_names;
    int nc_y_var, nc_x_var, nc_area_var, nc_time_var, nc_nhm_name_dim;
    int nc_nhm_names, i;
    int hruVarDims[2]; //any reason these are 3? i chnaged it to 2
    float *dates, *lons, *lats, *hru_area, mfv;
    size_t start[2], count[2]; //any reason these are 3?
    int jd_base, *names;
    char fmt[5];
    int *nhmId;
    char ani_path[MAXPATHLEN];

    sprintf(ani_path, "%s.nc", *((char **) control_var("ani_output_file")));

    retval = nc_create(ani_path, NC_NETCDF4 | NC_CLOBBER, &ncdf);
    if (retval != 0) ERR(retval);

    if (ani_out_flag > 2) {
        globalAttributsForCf (ncdf);
    }

//  Define the dimensions.
    retval = nc_def_dim(ncdf, "hru_id", n_hru, &nc_hru_dim);
    if (retval) ERR(retval);

    retval = nc_def_dim(ncdf, "time", ts_ani, &nc_time_dim);
    //retval = nc_def_dim(ncdf, "time", NC_UNLIMITED, &nc_time_dim); // if you do this, ncview thinks time is huge WHY?
    if (retval) ERR(retval);

    //retval = nc_def_dim(ncdf, "nhm_id", n_hru, &nc_hru_dim);
    //if (retval) ERR(retval);

// Set up time variable
    retval = nc_def_var(ncdf, "time", NC_FLOAT, 1, &nc_time_dim, &nc_time_var);
    if (retval) ERR(retval);

    mfv = -999.0;
    ncVarAttributes (ncdf, nc_time_var, "days since 1970-01-01 00:00:00", "", &mfv, "time", "time", "");

// Build the variable for the HRU name (ie ID as integer)
    retval = nc_def_var(ncdf, "hru_id", NC_INT, 1, &nc_hru_dim, &nc_hru_names);
    if (retval) ERR(retval);

    mfv = -999.0;
    ncVarAttributes (ncdf, nc_hru_names, "", "", &mfv, "hydrologic reponse unit id", "station_id", "");

// Build the variable for the NHM name (ie ID as integer)

    //retval = nc_def_var(ncdf, "nhm_id", NC_INT, 1, &nc_hru_dim, &nc_nhm_names);
    //if (retval) ERR(retval);

    //ncVarAttributes (ncdf, nc_nhm_names, "", "", NULL, "national hydrologic model unit id", "station_id", "");

// Set up the latitude variable
    if (ani_out_flag > 2) {
        retval = nc_def_var(ncdf, "lat", NC_FLOAT, 1, &nc_hru_dim, &nc_y_var);
        if (retval) ERR(retval);

        mfv = -999.0;
        ncVarAttributes (ncdf, nc_y_var, "degrees_north", "", &mfv, "Latitude", "Latitude", "");
    }

// Set up the longitude variable
    if (ani_out_flag > 2) {
        retval = nc_def_var(ncdf, "lon", NC_FLOAT, 1, &nc_hru_dim, &nc_x_var);
        if (retval) ERR(retval);

        ncVarAttributes (ncdf, nc_x_var, "degrees_east", "", &mfv, "Longitude", "Longitude", "");
    }

// Set up the hru_area variable
    if (ani_out_flag > 2) {
        retval = nc_def_var(ncdf, "hru_area", NC_FLOAT, 1, &nc_hru_dim, &nc_area_var);
        if (retval) ERR(retval);

        ncVarAttributes (ncdf, nc_area_var, "km2", "", &mfv, "HRU area", "HRU area", "");
    }

// Put in the selected animation variables
    hruVarDims[0] = nc_hru_dim;
    hruVarDims[1] = nc_time_dim;

    for (i = 0; i < naniVars; i++) {
        retval = nc_def_var(ncdf, ani_out_vars[i]->name, NC_FLOAT, 2, hruVarDims, &(ani_out_vars[i]->nc_var));
        if (retval) ERR(retval);

        if (strncmp (ani_out_vars[i]->units, "decimal fraction", 16) == 0) {  // change PRMS units to udunits
            ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, "count", "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "time");
        } else if (strncmp (ani_out_vars[i]->units, "cfs", 3) == 0) {
            ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, "ft3/s", "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "time");
        } else if (strncmp (ani_out_vars[i]->units, "inches", 6) == 0) {
            ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, "in/d", "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "time");
        } else if (strncmp (ani_out_vars[i]->units, "Langleys", 8) == 0) {
            ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, "langleys/d", "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "time");
        } else {
            //ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, ani_out_vars[i]->units, "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "time");
           // if you leave time as coordinate, ncview thinks time has an extra dimension WHY
            ncVarAttributes (ncdf, ani_out_vars[i]->nc_var, ani_out_vars[i]->units, "", &mfv, ani_out_vars[i]->help, ani_out_vars[i]->name, "");
        }

//        retval = nc_put_att_text(ncdf, ani_out_vars[i]->nc_var, "coordinates", strlen("square kilometers"), "square kilometers");
//        if (retval) ERR(retval);
    }

//  End define mode.
    retval = nc_enddef(ncdf);
    if (retval) ERR(retval);

// Write in the time steps: days since 1970-01-01
//    dt.year = 1970;
//    dt.month = 1;
//    dt.day = 1;
//    julday (&dt);
//    jd_base = dt.jd;
    jd_base = 2440588;
//    printf ("jd_base = %d\n", jd_base);

    dates = (float *) umalloc(ts_ani * sizeof(float));
    for (i = 0; i < ts_ani; i++) {
        dates[i] = (Mstrttime->jd + i) - jd_base;
        //printf ("%d %d %d %d %f\n",i,Mstrttime->jd,jd_base, (Mstrttime->jd + i) - jd_base,dates[i] );
    }
    retval = nc_put_var_float(ncdf, nc_time_var, dates);

// Write the lat/lon variable values.
    if (ani_out_flag > 2) {
        lons = (float *) umalloc(n_hru * sizeof(float));
        getparam("ncdf_animation", "hru_lon", n_hru, "float", (double *)lons);

        lats = (float *) umalloc(n_hru * sizeof(float));
        getparam("ncdf_animation", "hru_lat", n_hru, "float", (double *)lats);

        retval = nc_put_var_float(ncdf, nc_y_var, lats);
        if (retval) ERR(retval);

        retval = nc_put_var_float(ncdf, nc_x_var, lons);
        if (retval) ERR(retval);
    }

// Write the HRU areas
    if (ani_out_flag > 2) {
        hru_area = (float *) umalloc(n_hru * sizeof(float));
        getparam("ncdf_animation", "hru_area", n_hru, "float", (double *)hru_area);

// convert acres to km2
        for (i = 0; i < n_hru; i++) {
            hru_area[i] = hru_area[i] / 247.11;
        }

        retval = nc_put_var_float(ncdf, nc_area_var, hru_area);
        if (retval) ERR(retval);
    }

// Write the HRU IDs

    names = (int *) umalloc(n_hru * sizeof(int));
    for (i = 0; i < n_hru; i++) {
        names[i] = i+1;
    }
    retval = nc_put_var_int(ncdf, nc_hru_names, names);
    if (retval) ERR(retval);

// Write the NHM IDs
  //  nhmId = (int *) umalloc(n_hru * sizeof(int));
  //  getparam("ncdf_animation", "nhm_id", n_hru, "int", (double *)nhmId);
  //  retval = nc_put_var_int(ncdf, nc_nhm_names, nhmId);
  //  if (retval) ERR(retval);

// Write in the HRU data
    for (i = 0; i < naniVars; i++) {
        start[1] = 0;
        start[0] = 0;
        count[0] = n_hru;
        count[1] = ts_ani;

        retval = nc_put_vara_float(ncdf, ani_out_vars[i]->nc_var, start, count, ani_out_vars[i]->d);
        if (retval) ERR(retval);
    }

    /* Close the file. */
    retval = nc_close(ncdf);
    if (retval) ERR(retval);

    return retval;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ncdfUpdateValuesAni
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ncdfUpdateValuesAni (int num_ani_dims, DIMEN **ani_out_dims, long naniVars, PUBVAR **ani_out_vars) {
    int j, k;
 //   DIMEN *dim;
    PUBVAR *var;

 //   for (i = 0; i < num_ani_dims; i++) {
 //       dim = ani_out_dims[i];
 //       for (j = 0; j < dim->value; j++) {
            for (k = 0; k < naniVars; k++) {
                var = ani_out_vars[k];
                for (j = 0; j < n_hru; j++) {
                    switch (var->type) {
                        case M_DOUBLE:
                            ((float *)(ani_out_vars[k]->d))[(j * ts_ani) + ts]  = (float)(*((double *) var->value + j));
                        break;

                        case M_FLOAT:
                             ((float *)(ani_out_vars[k]->d))[(j * ts_ani) + ts]  = *((float *) var->value + j);
                        break;

                        case M_LONG:
    //                        fprintf (ani_var_files[i], " %10ld", *((long *) var->value + j));
                        break;
                    }
                }
            }
 //       }
//    }
    ts++;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ncVarAttributes
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ncVarAttributes (int nc_id, int ncv_id, char *u_str, char *mv_str, float *mv_fp, char *ln_str,
                        char *sn_str, char *c_str) {
    int retval;

    retval = nc_put_att_text(nc_id, ncv_id, "units", strlen(u_str), u_str);
    if (retval) ERR(retval);

    if (mv_fp == NULL) {
        retval = nc_put_att_text(nc_id, ncv_id, "missing_value", strlen(mv_str), mv_str);
        if (retval) ERR(retval);
    } else {
        retval = nc_put_att_float(nc_id, ncv_id, "missing_value", NC_FLOAT, 1, mv_fp);
        if (retval) ERR(retval);
    }

    retval = nc_put_att_text(nc_id, ncv_id, "long_name", strlen(ln_str), ln_str);
    if (retval) ERR(retval);

    retval = nc_put_att_text(nc_id, ncv_id, "standard_name", strlen(sn_str), sn_str);
    if (retval) ERR(retval);

    retval = nc_put_att_text(nc_id, ncv_id, "coordinates", strlen(c_str), c_str);
    if (retval) ERR(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : globalAttributsForCf
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void globalAttributsForCf (int ncdf) {
    int retval;
    time_t rawtime;
    struct tm * timeinfo;
    char *un;
    char *email;

//  Global attributes
    retval = nc_put_att_text(ncdf, NC_GLOBAL, "Conventions", strlen("CF-1.6"), "CF-1.6");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "featureType", strlen("timeSeries"), "timeSeries");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "cdm_data_type", strlen("Station"), "Station");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "standard_name_vocabulary", strlen("CF-1.6"), "CF-1.6");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "title", strlen("Precipitation Runoff Modeling System"), "Precipitation Runoff Modeling System");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "institution", strlen("United States Geological Survey"), "United States Geological Survey");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "nc_source", strlen("Precipitation Runoff Modeling System"), "Precipitation Runoff Modeling System");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "reference", strlen("Markstrom, S.L., Regan, R.S., Hay, L.E., Viger, R.J., Webb, R.M.T., Payn, R.A., and LaFontaine, J.H., 2015, PRMS-IV, the precipitation-runoff modeling system, version 4: U.S. Geological Survey Techniques and Methods, book 6, chap. B7, 158 p., http://dx.doi.org/10.3133/tm6B7."), "Markstrom, S.L., Regan, R.S., Hay, L.E., Viger, R.J., Webb, R.M.T., Payn, R.A., and LaFontaine, J.H., 2015, PRMS-IV, the precipitation-runoff modeling system, version 4: U.S. Geological Survey Techniques and Methods, book 6, chap. B7, 158 p., http://dx.doi.org/10.3133/tm6B7.");
    if (retval) ERR(retval);

    retval = nc_put_att_text(ncdf, NC_GLOBAL, "summary", strlen("Selected PRMS model output"), "Selected PRMS model output");
    if (retval) ERR(retval);

    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    retval = nc_put_att_text(ncdf, NC_GLOBAL, "date_created", strlen(asctime (timeinfo)), asctime (timeinfo));
    if (retval) ERR(retval);

    un = getlogin();
    retval = nc_put_att_text(ncdf, NC_GLOBAL, "creator_name", strlen(un), un);
    if (retval) ERR(retval);

    email = *control_svar("creator_email");
    retval = nc_put_att_text(ncdf, NC_GLOBAL, "creator_email", strlen(email), email);
    if (retval) ERR(retval);
}
