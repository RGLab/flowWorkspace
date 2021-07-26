/*
 * h5_error_r_handler.cpp
 *
 *  Created on: Dec 6, 2018
 *      Author: wjiang2
 */
#include <hdf5.h>
#include <flowWorkspace/pairVectorCpp11Convert.h>
#define MSG_SIZE 1024
herr_t my_hdf5_error_handler(unsigned n, const H5E_error2_t *err_desc, void *client_data)
{
	char                maj[MSG_SIZE];
	char                min[MSG_SIZE];

	const int		indent = 4;

	if(H5Eget_msg(err_desc->maj_num, NULL, maj, MSG_SIZE)<0)
		return -1;

	if(H5Eget_msg(err_desc->min_num, NULL, min, MSG_SIZE)<0)
		return -1;

	REprintf("%*s error #%03d: in %s(): line %u\n",
		 indent, "", n, err_desc->func_name, err_desc->line);
	REprintf("%*smajor: %s\n", indent*2, "", maj);
	REprintf("%*sminor: %s\n", indent*2, "", min);

   return 0;
}

/*TODO:cat all error msg into single R error throw
 * customize the printing function so that it print to R error console
 * also raise the R error once the error stack printing is done
 */
herr_t custom_print_cb(hid_t estack, void *client_data)
{
	hid_t estack_id = H5Eget_current_stack();//copy stack before it is corrupted by my_hdf5_error_handler
	H5Ewalk2(estack_id, H5E_WALK_DOWNWARD, my_hdf5_error_handler, client_data);
	H5Eclose_stack(estack_id);
	cpp11::stop("hdf Error");
    return 0;

}

[[cpp11::register]]
void h5_set_error_handler(){
	H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)custom_print_cb, NULL);
}




