#include "xmlparse.h"

/* Types and structures for reading parameter defs */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  void *ptr;
  char  value[REG_MAX_STRING_LENGTH];
  int   modified;

} param_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  param_entry *param;

} Param_table_type;

/* Types and structures for reading supported commands */

typedef struct {

  int   cmd_id;
  int   data_int;
  float data_flt;

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
  /* Whether sample data is consumed or emitted */
  enum {IN = 0,
	OUT} direction;

} IOdef_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  int          next_handle;
  IOdef_entry *io_def;

}IOdef_table_type;

/* Structures used by XML parser */

/* Top-level user-info structure */

typedef struct {

  REG_MsgType  msg_type;
  
  void  *gen_xml_struct;

} user_data_type;

/* Structure for parsing parameter definitions */

typedef struct {

  enum {PARAM_NOTSET = 0,
	PARAM_LABEL,
	STRABLE,
	TYPE,
	HANDLE,
	VALUE
  }                 field_type;

  Param_table_type *table;

} param_xml_struct;

/* Structure for parsing supported commands */

typedef struct {

  enum {CMD_NOTSET = 0,
	CMD_ID,
	CMD_DATA_INT,
	CMD_DATA_FLT
  }                    field_type;

  Supp_cmd_table_type *table;

} supp_cmds_xml_struct;

/* Structure for parsing IOType definitions */

typedef struct {

  enum {IODEF_NOTSET = 0,
	IODEF_LABEL,
	IODEF_HANDLE,
	IODEF_DIRN
  } field_type;

  IOdef_table_type *table;

} iodef_xml_struct;

/* Structure for parsing steerer control files */

typedef struct {

  enum {CTRL_NOTSET = 0,
	CTRL_CMD,
	CTRL_PARAM
  } field_type;

  param_xml_struct     *param_struct;
  supp_cmds_xml_struct *cmd_struct;
  /*  Supp_cmd_table_type *table;*/

} control_xml_struct;

/* Structure for parsing app status files - may eventually differ
   from control files, hence separate structure def */

typedef struct {

  enum {STAT_NOTSET = 0,
	STAT_CMD,
	STAT_PARAM
  } field_type;

  param_xml_struct     *param_struct;
  supp_cmds_xml_struct *cmd_struct;
  /*  Supp_cmd_table_type *table;*/

} status_xml_struct;

/*-------- Function prototypes --------*/

extern FILE *Open_next_file(char* base_name);

extern int Create_lock_file(char *filename);

extern int Delete_file(char *filename);

extern int Directory_valid(char *directory);

extern int Next_free_param_index(Param_table_type *table);

extern int Increment_param_registered(Param_table_type *table);

extern int Increment_cmd_registered(Supp_cmd_table_type *table);

extern int Increment_iodef_registered(IOdef_table_type *table);

/* Called when steering finished - cleans up any files that either the app
   or steerer hasn't got around to consuming */

extern int Remove_files(char* base_name);

extern REG_MsgType Get_message_type(const char *name);

/* 'Callback' routines for XML parser */

extern void startElement(void *userData, const char *name, const char **atts);

extern void endElement(void *userData, const char *name);

extern void dataHandler(void *userData, const XML_Char *s, int len);

/* These two used only by Get_next_message in order to search for a 
   recognised message type */

extern void scan_startElement(void *userData, const char *name, 
			      const char **atts);

extern void scan_endElement(void *userData, const char *name);

/* Utility routines for identifying and storing values from
   XML elements */

extern int  Write_xml_header(FILE *fp);

extern void Set_param_field_type(void *ptr, const char* name);
       
extern void Set_iodef_field_type(void *ptr, const char* name);
       
extern void Set_supp_cmd_field_type(void *ptr, const char* name);
       
extern void Set_ctrl_field_type(void *ptr, const char* name);

extern void Set_status_field_type(void *ptr, const char* name);

extern void Store_param_field_value(void *ptr, char *buf);
       
extern void Store_supp_cmds_field_value(void *ptr, char *buf);
       
extern void Store_iodef_field_value(void *ptr, char *buf);

extern void Store_control_field_value(void *ptr, char *buf);

extern void Store_status_field_value(void *ptr, char *buf);

