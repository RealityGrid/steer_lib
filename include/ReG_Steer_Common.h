/* Following two includes are for use of stat system call 
   in Open_next_file */
#include <sys/types.h>
#include <sys/stat.h>

/* Types and structures for reading parameter defs */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  char  value[REG_MAX_STRING_LENGTH];
  char  max_val[REG_MAX_STRING_LENGTH];
  char  min_val[REG_MAX_STRING_LENGTH];
  int   modified;
  int   is_internal;

} param_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  param_entry *param;

} Param_table_type;

/* Types and structures for reading supported commands */

typedef struct {

  int               cmd_id;
  Param_table_type  cmd_params;

}supp_cmd_entry;

typedef struct {

  int             num_registered;
  int             max_entries;
  supp_cmd_entry *cmd;

} Supp_cmd_table_type;

/* Types and structures for reading IO types */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  char  filename[REG_MAX_STRING_LENGTH];
  int   handle;
  /* Whether input, output or a checkpoint */
  int   direction;
  /* Whether this channel supports automatic emission/consumption
     every frequency steps */
  int   auto_io_support;
  /* Handle of the (steerable) frequency in the parameter table */
  int   freq_param_handle;

} IOdef_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  IOdef_entry *io_def;

}IOdef_table_type;

/*-------- Function prototypes --------*/

extern FILE *Open_next_file(char* base_name);

extern int Create_lock_file(char *filename);

extern int Delete_file(char *filename);

extern int Directory_valid(char *directory);

extern int Next_free_param_index(Param_table_type *table);

/* A look-up function - return the index of the parameter with handle
   ParamHandle in the table pointed to by *table. Returns 
   REG_PARAM_HANDLE_NOTSET if no matching handle found. */
extern int Param_index_from_handle(Param_table_type *table, int ParamHandle);

extern int Increment_param_registered(Param_table_type *table);

extern int Increment_cmd_registered(Supp_cmd_table_type *table);

extern int Increment_iodef_registered(IOdef_table_type *table);

/* Called when steering finished - cleans up any files that either the app
   or steerer hasn't got around to consuming */

extern int Remove_files(char* base_name);

extern REG_MsgType Get_message_type(const char *name);

/* Write ReG-specific XML header & footer information into supplied message
   buffer */

extern int  Write_xml_header(char **pchar);

extern int  Write_xml_footer(char **pchar);

